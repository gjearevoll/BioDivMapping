#!/usr/bin/env python3
"""
Compute which ETH Global Canopy Height 10m (2020) 3-degree tiles intersect a
given boundary, and download them from ETH's libdrive share into a local
folder — for later mosaicking with canopy_trondelag_local.py.

Generalizes canopy_trondelag_download_tiles.sh (which hardcoded the 6 tiles
covering Trøndelag) to any boundary: tiles are derived from the boundary
geometry itself (not just its bounding box), so e.g. mainland Norway pulls in
only the ~23 tiles that actually touch land, not the ~50 in its bbox.

Usage:
    uv run download_canopy_tiles.py --boundary /path/to/boundary.shp
    uv run download_canopy_tiles.py --boundary boundary.shp -o canopy_tiles
"""

import argparse
import math
import os
import time

import geopandas as gpd
import rasterio
import requests
from rasterio.enums import Resampling
from shapely.geometry import box

LIBDRIVE_SHARE = "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download"
TILE_SIZE_DEG = 3


def tiles_intersecting(boundary_wgs84: gpd.GeoDataFrame) -> list[str]:
    """Return ETH tile names (e.g. 'N60E006') for each 3x3-degree grid cell
    that actually intersects the boundary geometry, not just its bbox."""
    geom = boundary_wgs84.union_all()
    minx, miny, maxx, maxy = boundary_wgs84.total_bounds

    lat0 = math.floor(miny / TILE_SIZE_DEG) * TILE_SIZE_DEG
    lat1 = math.floor(maxy / TILE_SIZE_DEG) * TILE_SIZE_DEG
    lon0 = math.floor(minx / TILE_SIZE_DEG) * TILE_SIZE_DEG
    lon1 = math.floor(maxx / TILE_SIZE_DEG) * TILE_SIZE_DEG

    names = []
    lat = lat0
    while lat <= lat1:
        lon = lon0
        while lon <= lon1:
            cell = box(lon, lat, lon + TILE_SIZE_DEG, lat + TILE_SIZE_DEG)
            if geom.intersects(cell):
                lat_prefix = "N" if lat >= 0 else "S"
                lon_prefix = "E" if lon >= 0 else "W"
                names.append(f"{lat_prefix}{abs(lat):02d}{lon_prefix}{abs(lon):03d}")
            lon += TILE_SIZE_DEG
        lat += TILE_SIZE_DEG
    return names


def aggregate_tile(native_path: str, factor: int) -> str:
    """Block-average `factor`x`factor` native pixels into one (e.g. factor=10
    turns ETH's ~10m tiles into ~100m), via a decimated read — GDAL excludes
    nodata pixels from the average as long as the source has nodata set (ETH
    tiles do: nodata=255 on a uint8 band). Leverages the tile's internal COG
    overviews where the decimation factor allows. Replaces the native file
    with the aggregated one to keep disk usage down."""
    agg_path = native_path.replace(".tif", f"_agg{factor}x.tif")
    with rasterio.open(native_path) as src:
        new_height = max(1, src.height // factor)
        new_width = max(1, src.width // factor)
        data = src.read(
            out_shape=(src.count, new_height, new_width),
            resampling=Resampling.average,
        )
        new_transform = src.transform * src.transform.scale(
            src.width / new_width, src.height / new_height
        )
        meta = src.meta.copy()
        meta.update(
            {
                "height": new_height,
                "width": new_width,
                "transform": new_transform,
                "compress": "deflate",
                "predictor": 2,
            }
        )
    with rasterio.open(agg_path, "w", **meta) as dst:
        dst.write(data)
    os.remove(native_path)
    return agg_path


def download_tile(
    name: str, out_dir: str, aggregate_factor: int, max_attempts=5, base_delay=10
) -> bool:
    """Download one tile, then block-average it down by `aggregate_factor`
    (pass 1 to keep native resolution). Returns False (without raising) on a
    404 — some grid cells intersect the boundary bbox-wise but ETH never
    published a tile for them (e.g. mostly-ocean cells). Retries with
    backoff on 429s and server errors, since libdrive rate-limits
    aggressively."""
    fname = f"ETH_GlobalCanopyHeight_10m_2020_{name}_Map.tif"
    url = f"{LIBDRIVE_SHARE}?path=%2F3deg_cogs&files={fname}"
    out_path = os.path.join(out_dir, fname)

    for attempt in range(1, max_attempts + 1):
        resp = requests.get(url, stream=True, timeout=120)
        if resp.status_code == 404:
            print(f"  {name}: no tile published (404) — skipping")
            return False
        if resp.status_code == 429 or resp.status_code >= 500:
            if attempt == max_attempts:
                resp.raise_for_status()
            delay = base_delay * attempt
            print(
                f"  {name}: HTTP {resp.status_code}, attempt {attempt}/{max_attempts}, "
                f"retrying in {delay}s..."
            )
            time.sleep(delay)
            continue
        resp.raise_for_status()
        with open(out_path, "wb") as f:
            for chunk in resp.iter_content(chunk_size=1024 * 1024):
                f.write(chunk)
        if aggregate_factor > 1:
            agg_path = aggregate_tile(out_path, aggregate_factor)
            print(f"  {name}: saved and aggregated to {agg_path}")
        else:
            print(f"  {name}: saved to {out_path}")
        return True
    return False


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--boundary",
        required=True,
        help="Path to a boundary vector file (shapefile/GeoJSON/GPKG/...)",
    )
    parser.add_argument(
        "-o", "--out-dir", default="canopy_tiles", help="Folder to save tiles into"
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Only list the tiles that would be downloaded, don't fetch them",
    )
    parser.add_argument(
        "--aggregate-factor",
        type=int,
        default=10,
        help=(
            "Block-average this many native pixels into one before saving each "
            "tile (default 10: ETH's ~10m tiles -> ~100m). Pass 1 to keep native "
            "resolution. The final mosaic's exact output resolution is set "
            "separately in canopy_trondelag_local.py's --resolution."
        ),
    )
    args = parser.parse_args()

    os.makedirs(args.out_dir, exist_ok=True)

    print(f"Loading boundary from {args.boundary!r}...")
    boundary = gpd.read_file(args.boundary).to_crs("EPSG:4326")

    tiles = sorted(tiles_intersecting(boundary))
    print(f"{len(tiles)} candidate tile(s) intersect the boundary:")
    for t in tiles:
        print(f"  {t}")

    if args.dry_run:
        print("\n--dry-run set, not downloading anything.")
        return

    print(f"\nDownloading to {args.out_dir!r}...")
    downloaded, skipped = 0, 0
    for name in tiles:
        if download_tile(name, args.out_dir, args.aggregate_factor):
            downloaded += 1
        else:
            skipped += 1

    print(f"\nDone. {downloaded} tile(s) downloaded, {skipped} skipped.")


if __name__ == "__main__":
    main()
