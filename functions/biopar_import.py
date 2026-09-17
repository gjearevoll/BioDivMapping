#!/usr/bin/env python3
"""
Fetch one BIOPAR biophysical parameter (FAPAR/LAI/FCOVER) for a given
boundary as a single static raster: the per-pixel annual MAXIMUM, averaged
across the last N calendar years.

In plain terms: BIOPAR is a family of satellite-derived measurements of
vegetation health/density (e.g. "how green and leafy is this patch of land"
- see BAND_PARAMS below for what each one actually measures). Satellite
readings for a spot of land come in roughly every 10 days, but are noisy and
sometimes missing (cloud cover, sensor gaps, etc.). To get one reliable
number per pixel, for every year we take the single highest reading (the
"annual maximum" -- roughly "how leafy did this spot get at its greenest
moment that year"), then average that yearly peak across several years to
smooth out unusual years. The final result is one image, one number per
pixel, ready to feed into the species models alongside the other covariates.

Built as the single subprocess call behind BioDivMapping's get_biopar(), same
pairing as soilgrids_import.py / get_soil_grids() -- R passes a boundary and
a variable, this script owns the year loop and the cross-year average, and
writes one predictably-named GeoTIFF for R to load.

Data source -- CDSE's free OData/S3 CGLS catalogue (see
BIOPAR_download/test_clms_odata.py and biopar_s3_prototype.py for the
investigation behind this), NOT the paid openEO Processing API this script
used previously. A single 10-day satellite image ("dekad") for the whole
world is one large file (roughly 1-3 GB); rather than downloading the whole
thing, we read just the small rectangular window of pixels covering the
boundary directly from where it's stored online, so the amount of data
transferred (and therefore the time and any cost) scales with the size of
the boundary you ask for, not with the size of the whole planet. A 3-year
pilot run for one variable using the old, paid route cost about 1500 "openEO
credits"; the same result via this free route costs nothing but a modest
amount of plain internet data transfer.

Auth: only CDSE S3 access-key credentials are needed (AWS_ACCESS_KEY_ID /
AWS_SECRET_ACCESS_KEY / AWS_S3_ENDPOINT env vars) -- generated once from
https://eodata-s3keysmanager.dataspace.copernicus.eu/. 

Pipeline per run
-----------------
    for each of the last --years calendar years:
        for each of that year's ~36 ten-day windows ("dekads": the 10th,
        20th, and last day of every month -- CGLS's fixed cadence):
            read just the boundary's rectangle of pixels out of that
            10-day image, and throw out any pixel that is missing, out of
            range, or flagged low-quality by the satellite product's own
            quality flag
        keep, for each pixel, whichever of that year's 10-day readings was
        the highest (the "annual maximum")
    average the resulting one-picture-per-year stack together, pixel by
    pixel, skipping any pixel that was unusable in a given year
    convert the result into the map projection/pixel size the rest of the
    pipeline expects

Usage
-----
    uv run biopar_import.py --biopar FAPAR \
        --boundary norge_border/Noreg_polygon.shp \
        --resolution 500 --years 10 --out-dir output

Setup
-----
    uv add boto3 rasterio geopandas shapely numpy

KNOWN GAP -- as of this script being written, only tested up to a
single-county-sized boundary, not yet a whole-country run. The amount of
data read scales with the size of the boundary's bounding rectangle, so a
much bigger area means a much bigger download per 10-day window, and a much
bigger stack of images held in memory at once (up to ~36-75 per year) when
working out the annual maximum. Watch memory use and how long it takes on
the first whole-country run before assuming this holds at that scale. If a
whole-country run turns out to be too much to do in one go, the fix is to
split the area into smaller pieces and combine the results afterwards -- not
to go back to the old, paid download method.

KNOWN GAP -- the satellite product's own quality flag can mark a reading as
lower-confidence in more ways than this script currently checks for. Right
now it only throws out the one unambiguous case: a value that's really just
copied over from an earlier reading because nothing new was available. It
does not yet look at the flag's other information about *how* a value was
worked out (a direct measurement vs. various kinds of estimate/interpolation)
-- check the satellite product's own documentation before deciding whether
those less-certain readings should also be excluded.
"""

from __future__ import annotations

import argparse
import calendar
import logging
import os
import sys
import time
from datetime import date
from pathlib import Path

import boto3
import geopandas as gpd
import numpy as np
import rasterio
from rasterio.warp import Resampling, calculate_default_transform, reproject
from rasterio.windows import from_bounds

log = logging.getLogger("biopar_import")

# --------------------------------------------------------------------------- #
# Constants
# --------------------------------------------------------------------------- #

# The three vegetation measurements this script can fetch. Two related ones
# (CCC/CWC) were considered but dropped in an earlier session because the
# satellite catalogue this script reads from doesn't offer a ready-made
# equivalent for them -- see project memory for that decision.
BIOPAR_TYPES = ["FAPAR", "LAI", "FCOVER"]

# Which underlying satellite dataset each measurement comes from.
DATASETS = {
    "LAI": "lai_global_300m_10daily_v2",       # Leaf Area Index -- roughly, how much leaf surface covers the ground
    "FAPAR": "fapar_global_300m_10daily_v2",   # Fraction of sunlight absorbed by plants for photosynthesis
    "FCOVER": "fcover_global_300m_10daily_v2", # Fraction of the ground covered by vegetation, seen from above
}

# Satellite images store their readings as whole numbers ("DN" = digital
# number) rather than the real-world value directly, to save space. These
# per-parameter settings say how to turn a DN back into the real value
# (physical_value = DN * scale + offset), and the highest DN that still
# counts as a genuine reading -- anything above that is the satellite
# product's way of flagging "this pixel isn't usable" (e.g. sea, or another
# surface type this parameter doesn't apply to), so those get excluded the
# same as missing data. Confirmed directly from each dataset's own metadata
# on 2026-09-14.
BAND_PARAMS = {
    "LAI": {"scale": 0.0333333333, "offset": 0.0, "valid_max_dn": 210},
    "FAPAR": {"scale": 0.004, "offset": 0.0, "valid_max_dn": 235},
    "FCOVER": {"scale": 0.004, "offset": 0.0, "valid_max_dn": 250},
}

# The quality-flag bit meaning "this pixel's value wasn't really measured
# this time around -- it's just been carried over from an earlier reading,
# or is missing outright." Confirmed directly against the dataset's own
# metadata on 2026-09-14, and the same across all three parameters above.
# This is the only quality issue currently filtered out -- see the module
# docstring's second KNOWN GAP for the confidence-level information this
# does NOT yet act on.
QFLAG_MISSING_OR_CARRIED_OVER = 16

# The shared starting folder path ("prefix") under which all of these
# satellite products are stored in CDSE's online storage.
CLMS_PREFIX = "CLMS/bio-geophysical/vegetation_properties"


# --------------------------------------------------------------------------- #
# Auth -- S3 access keys only. No separate login step needed any more.
# --------------------------------------------------------------------------- #


def require_s3_credentials() -> None:
    """Stop early with a clear, actionable message if the login details
    needed to read the satellite data aren't set, rather than failing
    partway through with a more confusing error."""
    missing = [v for v in ("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_S3_ENDPOINT")
               if not os.environ.get(v)]
    if missing:
        sys.exit(
            f"Missing {', '.join(missing)} in the environment. This pipeline reads "
            "CGLS data directly from CDSE's S3 bucket, which needs S3 access-key "
            "credentials (separate from any older login method) -- generate a "
            "pair at https://eodata-s3keysmanager.dataspace.copernicus.eu/ and set:\n"
            "  AWS_ACCESS_KEY_ID=<access key>\n"
            "  AWS_SECRET_ACCESS_KEY=<secret key>\n"
            "  AWS_S3_ENDPOINT=eodata.dataspace.copernicus.eu"
        )


def s3_client():
    """Set up the connection used to read files from CDSE's online storage."""
    return boto3.client(
        "s3",
        endpoint_url=f"https://{os.environ['AWS_S3_ENDPOINT']}",
        aws_access_key_id=os.environ["AWS_ACCESS_KEY_ID"],
        aws_secret_access_key=os.environ["AWS_SECRET_ACCESS_KEY"],
    )


# --------------------------------------------------------------------------- #
# Boundary
# --------------------------------------------------------------------------- #


def get_boundary(boundary_path: str) -> gpd.GeoDataFrame:
    """Read a boundary file (shapefile, GeoJSON, GPKG -- anything the
    geopandas library can read) and return it in the standard WGS84
    (latitude/longitude) map projection."""
    gdf = gpd.read_file(boundary_path)
    if gdf.crs is None:
        raise RuntimeError(f"Boundary file {boundary_path} has no CRS set.")
    return gdf.to_crs(4326)


# --------------------------------------------------------------------------- #
# Dekad discovery + windowed read
# --------------------------------------------------------------------------- #


def dekad_dates(year: int) -> list[date]:
    """The 36 fixed "dekad" (10-day window) dates in one year: the 10th,
    20th, and last day of every month. This is the satellite product's own
    fixed release schedule -- confirmed directly against real product dates,
    including checking a leap-day case (2024-02-29)."""
    out = []
    for month in range(1, 13):
        last_day = calendar.monthrange(year, month)[1]
        out += [date(year, month, 10), date(year, month, 20),
                date(year, month, last_day)]
    return out


def _list_with_retry(s3, prefix: str, retries: int = 3):
    """List the files/folders under `prefix` in online storage, retrying a
    couple of times if the request fails, since a brief network hiccup
    shouldn't sink the whole run."""
    for attempt in range(retries):
        try:
            resp = s3.list_objects_v2(Bucket="eodata", Prefix=prefix, Delimiter="/")
            return resp.get("CommonPrefixes", [])
        except Exception as e:
            if attempt == retries - 1:
                raise
            log.warning("S3 list failed (%s), retrying...", e)
            time.sleep(2 ** attempt)


def find_dekad_keys(s3, dataset_id: str, band: str, d: date) -> tuple[str, str] | None:
    """Work out the exact storage locations for one 10-day window's data
    file and its matching quality-flag file. Returns None if that date
    simply has no product yet (e.g. a very recent date that hasn't been
    processed by the data provider yet)."""
    date_prefix = f"{CLMS_PREFIX}/{dataset_id}/{d.year}/{d.month:02d}/{d.day:02d}/"
    subfolders = _list_with_retry(s3, date_prefix)
    cog_dirs = [cp["Prefix"] for cp in subfolders if cp["Prefix"].endswith("_cog/")]
    if not cog_dirs:
        return None
    resp = s3.list_objects_v2(Bucket="eodata", Prefix=cog_dirs[0])
    keys = [o["Key"] for o in resp.get("Contents", [])]
    band_key = next((k for k in keys if f"-{band}-" in k), None)
    qflag_key = next((k for k in keys if "-QFLAG-" in k), None)
    if band_key is None or qflag_key is None:
        log.warning("%s %s: found a product but missing band/QFLAG file under %s",
                    band, d, cog_dirs[0])
        return None
    return band_key, qflag_key


def _read_window_with_retry(vsis3_path: str, bbox, retries: int = 3):
    """Read just the pixels covering `bbox` out of a (potentially huge)
    remote file, without downloading the rest of it. Retries a couple of
    times on a transient network failure before giving up."""
    for attempt in range(retries):
        try:
            with rasterio.Env(AWS_S3_ENDPOINT=os.environ["AWS_S3_ENDPOINT"],
                               AWS_VIRTUAL_HOSTING="FALSE",
                               GDAL_DISABLE_READDIR_ON_OPEN="EMPTY_DIR"):
                with rasterio.open(vsis3_path) as src:
                    window = from_bounds(*bbox, transform=src.transform)
                    data = src.read(1, window=window, masked=True)
                    win_transform = src.window_transform(window)
                    return data, win_transform, src.crs
        except Exception as e:
            if attempt == retries - 1:
                raise
            log.warning("windowed read failed (%s), retrying...", e)
            time.sleep(2 ** attempt)


def read_dekad(s3, dataset_id: str, band: str, d: date, bbox, valid_max_dn: int):
    """Read one 10-day window's values for the area of interest, with
    unusable pixels blanked out -- both pixels outside the valid reading
    range, and pixels the quality flag says are just carried over from an
    earlier date. Returns None if this date has no product at all."""
    found = find_dekad_keys(s3, dataset_id, band, d)
    if found is None:
        return None
    band_key, qflag_key = found
    data, transform, crs = _read_window_with_retry("/vsis3/eodata/" + band_key, bbox)
    data = np.ma.masked_greater(data, valid_max_dn, copy=False)
    qflag, _, _ = _read_window_with_retry("/vsis3/eodata/" + qflag_key, bbox)
    bad_quality = (np.asarray(qflag) & QFLAG_MISSING_OR_CARRIED_OVER) != 0
    data = np.ma.masked_where(bad_quality, data)
    return data, transform, crs


def annual_max(s3, dataset_id: str, band: str, bbox, year: int, valid_max_dn: int):
    """For one year, work out -- pixel by pixel -- the highest value seen
    across that year's ~36 ten-day readings (comparing the raw stored
    numbers is fine here, since a higher stored number always means a
    higher real-world value for these parameters)."""
    arrays, transform, crs = [], None, None
    n_found = 0
    for d in dekad_dates(year):
        result = read_dekad(s3, dataset_id, band, d, bbox, valid_max_dn)
        if result is None:
            log.debug("%s %s: no product, skipping this dekad", band, d)
            continue
        data, transform, crs = result
        arrays.append(data)
        n_found += 1
    if not arrays:
        return None
    stack = np.ma.stack(arrays)
    log.info("%s %d: %d/%d dekads found", band, year, n_found, len(dekad_dates(year)))
    return stack.max(axis=0), transform, crs


def multi_year_average(s3, dataset_id: str, band: str, bbox, years: list[int]):
    """Work out each requested year's annual maximum, convert each from
    stored numbers into real-world values, then average those yearly peaks
    together pixel by pixel -- smoothing out any single unusual year."""
    params = BAND_PARAMS[band]
    yearly, transform, crs = [], None, None
    for year in years:
        result = annual_max(s3, dataset_id, band, bbox, year, params["valid_max_dn"])
        if result is None:
            log.warning("%s %d: no usable data at all, skipping this year", band, year)
            continue
        year_max_dn, transform, crs = result
        physical = year_max_dn.astype("float32") * params["scale"] + params["offset"]
        log.info("%s %d: annual max done, %d/%d valid px", band, year,
                 physical.count(), physical.size)
        yearly.append(physical)
    if not yearly:
        raise RuntimeError(f"{band}: no year produced usable data -- nothing to average")
    if len(yearly) < len(years):
        log.warning("%s: only %d/%d year(s) succeeded -- averaging what's available",
                    band, len(yearly), len(years))
    stack = np.ma.stack(yearly)
    # np.ma's mean automatically skips masked (unusable) pixels per-pixel,
    # rather than letting one bad year drag every pixel's average down.
    return stack.mean(axis=0), transform, crs


# --------------------------------------------------------------------------- #
# Reprojection + output
# --------------------------------------------------------------------------- #


def reproject_to_grid(array, src_transform, src_crs, dst_crs, resolution, nodata):
    """Convert the finished image into the requested map projection and
    pixel size, so it lines up on the same grid as the pipeline's other
    covariate layers."""
    filled = array.filled(nodata).astype("float32")
    height, width = filled.shape
    left, top = src_transform * (0, 0)
    right, bottom = src_transform * (width, height)
    dst_transform, dst_width, dst_height = calculate_default_transform(
        src_crs, dst_crs, width, height, left, bottom, right, top,
        resolution=resolution,
    )
    dst = np.full((dst_height, dst_width), nodata, dtype="float32")
    reproject(
        source=filled, destination=dst,
        src_transform=src_transform, src_crs=src_crs, src_nodata=nodata,
        dst_transform=dst_transform, dst_crs=dst_crs, dst_nodata=nodata,
        # "average" blends the values of the pixels being combined, which
        # makes sense for a continuous measurement like these.
        resampling=Resampling.average,
    )
    return dst, dst_transform


def write_geotiff(array, transform, crs, nodata, out_path):
    """Save the finished single-band image to disk as a standard GeoTIFF
    file, the format the rest of the pipeline expects to load."""
    profile = dict(driver="GTiff", height=array.shape[0], width=array.shape[1],
                   count=1, dtype="float32", crs=crs, transform=transform,
                   nodata=nodata, compress="deflate")
    with rasterio.open(out_path, "w", **profile) as dst:
        dst.write(array, 1)


def crs_tag(crs_str: str) -> str:
    """Turn a map-projection argument like 'EPSG:25833' into a short,
    filename-safe tag like 'epsg25833', worked out from whatever --crs was
    actually given -- never hard-coded separately, so the output filename
    can't drift out of sync with the projection actually used.
    get_biopar.R reconstructs this same tag from the crs argument it passes
    in; keep the two in sync if this changes."""
    epsg = rasterio.crs.CRS.from_user_input(crs_str).to_epsg()
    if epsg is None:
        raise ValueError(f"--crs {crs_str!r} has no EPSG code; give one that does "
                          "(needed for the output filename).")
    return f"epsg{epsg}"


# --------------------------------------------------------------------------- #
# CLI
# --------------------------------------------------------------------------- #


def parse_args(argv=None):
    p = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    g = p.add_argument_group("variable")
    g.add_argument("--biopar", choices=BIOPAR_TYPES, required=True,
                   help="biophysical parameter, e.g. FAPAR")

    g = p.add_argument_group("temporal")
    g.add_argument("--years", type=int, default=10,
                   help="how many of the most recent complete calendar years "
                        "to average (default: 10)")
    g.add_argument("--end-year", type=int, default=None,
                   help="last calendar year to include (default: last "
                        "complete year)")

    g = p.add_argument_group("area of interest")
    g.add_argument("--boundary", required=True,
                   help="path to a boundary vector file (any "
                        "geopandas-readable format)")

    g = p.add_argument_group("output grid")
    g.add_argument("--resolution", type=float, default=None,
                   help="output cell size in --crs units (metres). Omit to "
                        "keep CGLS's native ~333 m pixel size.")
    g.add_argument("--crs", default="EPSG:25833",
                   help="output CRS (default: EPSG:25833, ETRS89/UTM33N, "
                        "matching the other covariates). Any CRS with an "
                        "EPSG code works -- it is never hard-coded beyond "
                        "this argument, including in the output filename.")

    g = p.add_argument_group("execution")
    g.add_argument("--out-dir", type=Path, default=Path("./biopar_output"))
    g.add_argument("-v", "--verbose", action="store_true")
    return p.parse_args(argv)


def main(argv=None) -> int:
    args = parse_args(argv)
    logging.basicConfig(level=logging.DEBUG if args.verbose else logging.INFO,
                        format="%(levelname)s: %(message)s")
    # Quiet down the very chatty default logging from the libraries this
    # script depends on, so our own progress messages aren't buried.
    for noisy in ("boto3", "botocore", "urllib3", "s3transfer", "rasterio"):
        logging.getLogger(noisy).setLevel(logging.WARNING)

    require_s3_credentials()

    end_year = args.end_year or (date.today().year - 1)
    years = list(range(end_year - args.years + 1, end_year + 1))
    log.info("BIOPAR %s, %d year(s): %s", args.biopar, len(years), years)

    boundary = get_boundary(args.boundary)
    bbox = tuple(boundary.total_bounds)
    log.info("boundary bbox (EPSG:4326): %s", bbox)

    s3 = s3_client()
    avg, src_transform, src_crs = multi_year_average(
        s3, DATASETS[args.biopar], args.biopar, bbox, years)

    nodata = -9999.0
    out_arr, out_transform = reproject_to_grid(
        avg, src_transform, src_crs, args.crs, args.resolution, nodata)

    args.out_dir.mkdir(parents=True, exist_ok=True)
    out_path = args.out_dir / f"{args.biopar}_max_{args.years}yr_avg_{crs_tag(args.crs)}.tif"
    write_geotiff(out_arr, out_transform, args.crs, nodata, out_path)

    log.info("done -> %s", out_path)
    return 0


if __name__ == "__main__":
    sys.exit(main())
