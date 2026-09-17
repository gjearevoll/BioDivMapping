#!/usr/bin/env python3
"""
Download SoilGrids raw data, clip it to a boundary, and save as GeoTIFF.

SoilGrids (https://soilgrids.org) publishes its soil maps as one giant image
per variable, covering the whole world, hosted online. Rather than
downloading that entire huge file, this script opens it remotely and reads
only the small rectangular window of pixels that covers the boundary you
give it — so the download stays small and fast no matter how big the source
file is.

Requirements:
    uv add rasterio geopandas shapely requests pyproj

Usage examples:
    # Mean topsoil pH (0-5 cm) clipped to a boundary file
    uv run soilgrids_import.py --boundary norge_border/Noreg_polygon.shp

    # Several variables/depths at once
    uv run soilgrids_import.py --variable phh2o clay soc --depth 0-5cm 5-15cm --stat mean --boundary norge_border/Noreg_polygon.shp

    # Resample the reprojected output to a specific pixel size (metres)
    uv run soilgrids_import.py --boundary norge_border/Noreg_polygon.shp --resolution 100

    # Keep the native SoilGrids map projection instead of converting to UTM 33N
    uv run soilgrids_import.py --boundary norge_border/Noreg_polygon.shp --no-reproject

SoilGrids variable codes (see https://docs.isric.org/globaldata/soilgrids/SoilGrids_faqs_01.html#sec-faq2):
    bdod   bulk density
    cec    cation exchange capacity
    cfvo   coarse fragments volume
    clay   clay content
    nitrogen
    ocd    organic carbon density
    ocs    organic carbon stock (0-30cm only)
    phh2o  pH in H2O
    sand
    silt
    soc    soil organic carbon

Depths: 0-5cm, 5-15cm, 15-30cm, 30-60cm, 60-100cm, 100-200cm (ocs is 0-30cm only)
Stats:  mean, uncertainty (Q0.05/Q0.5/Q0.95 also exist for some products)
"""

import argparse
import os
import sys
import time
import traceback

import geopandas as gpd
import rasterio
from rasterio.mask import mask
from rasterio.warp import Resampling, calculate_default_transform, reproject
from shapely.geometry import mapping

# A handful of settings that make reading the remote SoilGrids files more
# reliable and a bit faster. These are the kind of low-level network/caching
# knobs you'd rarely need to touch by hand; they're set once here so every
# run benefits automatically.
os.environ.setdefault("GDAL_DISABLE_READDIR_ON_OPEN", "EMPTY_DIR")
os.environ.setdefault("CPL_VSIL_CURL_ALLOWED_EXTENSIONS", ".tif,.vrt")
os.environ.setdefault("VSI_CACHE", "TRUE")
os.environ.setdefault("VSI_CACHE_SIZE", "200000000")  # ~200MB
os.environ.setdefault("GDAL_HTTP_MULTIPLEX", "YES")
os.environ.setdefault("GDAL_HTTP_VERSION", "2")
# If a network read has a brief hiccup (a dropped connection, a slow
# response), retry a few times instead of failing the whole run — a
# country-sized clip can involve hundreds of small requests to the remote
# file, so an odd blip here and there is expected.
os.environ.setdefault("GDAL_HTTP_MAX_RETRY", "5")
os.environ.setdefault("GDAL_HTTP_RETRY_DELAY", "2")
os.environ.setdefault("GDAL_HTTP_TIMEOUT", "60")
os.environ.setdefault(
    "CPL_CURL_VERBOSE", "NO"
)  # set to "YES" for very detailed low-level network debug output

SOILGRIDS_BASE = "https://files.isric.org/soilgrids/latest/data"
# The map projection SoilGrids' own files are stored in (an unusual one
# called "Interrupted Goode Homolosine", designed to minimise distortion
# across the whole globe at once).
HOMOLOSINE_PROJ4 = "+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs"


def get_boundary(boundary_path: str) -> gpd.GeoDataFrame:
    """Read a boundary file (shapefile, GeoJSON, GPKG — anything the
    geopandas library can read) and return it in the standard WGS84
    (latitude/longitude) map projection."""
    print(f"Reading boundary from {boundary_path} ...")
    gdf = gpd.read_file(boundary_path)
    if gdf.crs is None:
        raise RuntimeError(f"Boundary file {boundary_path} has no CRS set.")
    return gdf.to_crs(4326)


def clip_soilgrids_layer(
    variable: str,
    depth: str,
    stat: str,
    boundary_wgs84: gpd.GeoDataFrame,
    out_dir: str,
    reproject_to_utm: bool = True,
    resolution: float = None,
) -> str:
    """Fetch one SoilGrids layer (one variable, at one depth, as one
    statistic — e.g. "mean pH at 0-5cm"), clip it to the boundary, and write
    the result as a GeoTIFF file. Returns the path of the file written."""
    vrt_url = f"{SOILGRIDS_BASE}/{variable}/{variable}_{depth}_{stat}.vrt"
    # The "/vsicurl/" prefix tells the underlying library to treat this web
    # address as if it were a local file, transparently downloading only the
    # bytes it actually needs as it reads — this is what avoids downloading
    # the whole global file.
    src_path = f"/vsicurl/{vrt_url}"

    print(f"\nOpening remote layer: {vrt_url}")
    with rasterio.open(src_path) as src:
        # The boundary needs to be in the same map projection as the
        # SoilGrids file before we can use it to select which pixels to read.
        boundary_native = boundary_wgs84.to_crs(src.crs)
        geoms = [mapping(boundary_native.geometry.iloc[0])]

        print("Reading + clipping (windowed read over the network)...")
        last_exc = None
        for attempt in range(1, 4):
            try:
                clipped, clipped_transform = mask(
                    src, geoms, crop=True, all_touched=True
                )
                last_exc = None
                break
            except Exception as exc:
                # A network read can fail transiently; wait a little longer
                # each time before trying again rather than giving up at once.
                last_exc = exc
                print(
                    f"  Read attempt {attempt}/3 failed ({exc}); retrying...",
                    file=sys.stderr,
                )
                time.sleep(3 * attempt)
        if last_exc is not None:
            raise last_exc
        clipped_meta = src.meta.copy()
        clipped_meta.update(
            {
                "height": clipped.shape[1],
                "width": clipped.shape[2],
                "transform": clipped_transform,
                "driver": "GTiff",
                "compress": "deflate",  # shrink file size on disk, no quality loss
            }
        )
        src_crs = src.crs

    os.makedirs(out_dir, exist_ok=True)
    base_name = f"{variable}_{depth}_{stat}"

    if not reproject_to_utm:
        # Save as-is, still in SoilGrids' own unusual map projection.
        out_path = os.path.join(out_dir, f"{base_name}_homolosine.tif")
        with rasterio.open(out_path, "w", **clipped_meta) as dst:
            dst.write(clipped)
        print(f"Saved: {out_path}")
        return out_path

    # Convert the clipped image to EPSG:25833 (ETRS89 / UTM zone 33N), the
    # standard map projection used for Norway elsewhere in this pipeline.
    dst_crs = "EPSG:25833"
    transform_kwargs = {}
    if resolution is not None:
        transform_kwargs["resolution"] = (resolution, resolution)
    transform, width, height = calculate_default_transform(
        src_crs,
        dst_crs,
        clipped.shape[2],
        clipped.shape[1],
        *_bounds_from_transform(clipped_transform, clipped.shape[2], clipped.shape[1]),
        **transform_kwargs,
    )
    dst_meta = clipped_meta.copy()
    dst_meta.update(
        {"crs": dst_crs, "transform": transform, "width": width, "height": height}
    )

    out_path = os.path.join(out_dir, f"{base_name}_utm33n.tif")
    with rasterio.open(out_path, "w", **dst_meta) as dst:
        for band_idx in range(1, clipped_meta["count"] + 1):
            reproject(
                source=clipped[band_idx - 1],
                destination=rasterio.band(dst, band_idx),
                src_transform=clipped_transform,
                src_crs=src_crs,
                dst_transform=transform,
                dst_crs=dst_crs,
                # "bilinear" blends nearby pixels together when resizing —
                # appropriate here since these are continuous measurements
                # (pH, carbon content, etc.), not categories.
                resampling=Resampling.bilinear,
            )
    print(f"Saved: {out_path}")
    return out_path


def _bounds_from_transform(transform, width, height):
    """Work out the real-world left/bottom/right/top edges of an image from
    its pixel-to-map-coordinates conversion info ("transform") and its size
    in pixels."""
    left, top = transform * (0, 0)
    right, bottom = transform * (width, height)
    return left, bottom, right, top


def main():
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument(
        "--variable",
        nargs="+",
        default=["phh2o"],
        help="One or more SoilGrids variable codes",
    )
    parser.add_argument(
        "--depth", nargs="+", default=["0-5cm"], help="One or more depth intervals"
    )
    parser.add_argument(
        "--stat", default="mean", help="Statistic: mean, uncertainty, etc."
    )
    parser.add_argument(
        "--out-dir", default="soilgrids_output", help="Output directory"
    )
    parser.add_argument(
        "--no-reproject",
        action="store_true",
        help="Keep native Homolosine map projection instead of converting to UTM 33N",
    )
    parser.add_argument(
        "--boundary",
        required=True,
        help="Path to a boundary vector file (any geopandas-readable format).",
    )
    parser.add_argument(
        "--resolution",
        type=float,
        default=None,
        help="Target pixel size in metres for the converted output (only "
        "applies when converting to UTM 33N). Left unset, the pixel size "
        "is worked out automatically from the source data.",
    )
    args = parser.parse_args()

    boundary = get_boundary(args.boundary)

    # Fetch every combination of variable x depth that was asked for (e.g.
    # pH and soil organic carbon, both at 0-5cm, is two combinations).
    outputs = []
    for variable in args.variable:
        for depth in args.depth:
            try:
                out_path = clip_soilgrids_layer(
                    variable=variable,
                    depth=depth,
                    stat=args.stat,
                    boundary_wgs84=boundary,
                    out_dir=args.out_dir,
                    reproject_to_utm=not args.no_reproject,
                    resolution=args.resolution,
                )
                outputs.append(out_path)
            except Exception:
                # Keep going with the remaining variable/depth combinations
                # even if one of them fails, rather than losing everything
                # that has already succeeded.
                print(f"FAILED for {variable} {depth} {args.stat}:", file=sys.stderr)
                traceback.print_exc()

    print("\nDone. Files written:")
    for p in outputs:
        print(f"  {p}")


if __name__ == "__main__":
    main()
