#!/usr/bin/env python3
"""
Compute which ETH Global Canopy Height 10m (2020) 3-degree tiles intersect a
given boundary, and download them from ETH's libdrive share into a local
folder — for later mosaicking with canopy_mosaic.py.

This script figures out which tiles are actually needed by looking at the
real shape of the boundary, not just a rectangular box drawn around it — so
e.g. mainland Norway only pulls in the ~23 tiles that actually touch land,
rather than the ~50 tiles that would fall inside its bounding box (which
includes a lot of open sea).

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

# The public link ETH publishes its canopy height tiles from.
LIBDRIVE_SHARE = "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download"
# ETH splits the world into square tiles, each covering 3 degrees of
# longitude/latitude (roughly 300km x 300km near the equator, smaller
# further north/south).
TILE_SIZE_DEG = 3


def tiles_intersecting(boundary_wgs84: gpd.GeoDataFrame) -> list[str]:
    """Work out which of ETH's tile names (e.g. 'N60E006') actually touch
    the boundary's real shape, not just the rectangular box around it."""
    # Combine every shape in the boundary file into one single shape, so we
    # only have to compare against one thing instead of many.
    geom = boundary_wgs84.union_all()
    # The four edges of the smallest rectangle that fully contains the
    # boundary — a quick starting point before checking individual tiles.
    minx, miny, maxx, maxy = boundary_wgs84.total_bounds

    # Round the rectangle's edges outward to the nearest tile-grid lines, so
    # we cover every tile the rectangle could possibly touch.
    lat0 = math.floor(miny / TILE_SIZE_DEG) * TILE_SIZE_DEG
    lat1 = math.floor(maxy / TILE_SIZE_DEG) * TILE_SIZE_DEG
    lon0 = math.floor(minx / TILE_SIZE_DEG) * TILE_SIZE_DEG
    lon1 = math.floor(maxx / TILE_SIZE_DEG) * TILE_SIZE_DEG

    # Walk across the grid of candidate tiles one by one and keep only the
    # ones that genuinely overlap the boundary's shape (not just its box).
    names = []
    lat = lat0
    while lat <= lat1:
        lon = lon0
        while lon <= lon1:
            cell = box(lon, lat, lon + TILE_SIZE_DEG, lat + TILE_SIZE_DEG)
            if geom.intersects(cell):
                # ETH names tiles by the coordinates of their lower-left
                # corner, e.g. "N60E006" = starts at 60°N, 6°E.
                lat_prefix = "N" if lat >= 0 else "S"
                lon_prefix = "E" if lon >= 0 else "W"
                names.append(f"{lat_prefix}{abs(lat):02d}{lon_prefix}{abs(lon):03d}")
            lon += TILE_SIZE_DEG
        lat += TILE_SIZE_DEG
    return names


def aggregate_tile(native_path: str, factor: int) -> str:
    """Shrink a tile by averaging blocks of neighbouring pixels together
    (e.g. factor=10 turns ETH's ~10m-per-pixel tiles into ~100m-per-pixel
    tiles). This is done so the downloaded files stay a manageable size —
    without it, a full-country run would need to store and process far more
    detail than the pipeline actually needs. Locations with "no data" (e.g.
    open water) are correctly left out of the averaging rather than dragging
    the average down. The original full-detail file is deleted afterwards
    to save disk space, keeping only the shrunk version."""
    agg_path = native_path.replace(".tif", f"_agg{factor}x.tif")
    with rasterio.open(native_path) as src:
        new_height = max(1, src.height // factor)
        new_width = max(1, src.width // factor)
        # Reading with a smaller "out_shape" than the file's real size makes
        # the underlying library do the block-averaging for us as it reads,
        # rather than us reading everything at full size and shrinking it
        # afterwards — much faster and uses far less memory.
        data = src.read(
            out_shape=(src.count, new_height, new_width),
            resampling=Resampling.average,
        )
        # The shrunk image needs its own "where is this pixel located on
        # Earth" information, scaled up to match the new, larger pixel size.
        new_transform = src.transform * src.transform.scale(
            src.width / new_width, src.height / new_height
        )
        meta = src.meta.copy()
        meta.update(
            {
                "height": new_height,
                "width": new_width,
                "transform": new_transform,
                "compress": "deflate",  # shrink the file on disk, no quality loss
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
    """Download one tile and then shrink it by `aggregate_factor` (pass 1 to
    keep it at full detail). Returns False (without stopping the whole run)
    if ETH simply doesn't have a tile for this name — some grid squares
    touch the boundary's rectangle but are mostly open ocean, and ETH never
    published a file for them. If the download service is temporarily
    overloaded or briefly unreachable, this waits a bit and tries again
    rather than giving up immediately, since ETH's download service can be
    strict about how many requests arrive in a short time."""
    fname = f"ETH_GlobalCanopyHeight_10m_2020_{name}_Map.tif"
    url = f"{LIBDRIVE_SHARE}?path=%2F3deg_cogs&files={fname}"
    out_path = os.path.join(out_dir, fname)

    for attempt in range(1, max_attempts + 1):
        resp = requests.get(url, stream=True, timeout=120)
        if resp.status_code == 404:
            # "404" means the file genuinely doesn't exist — not worth retrying.
            print(f"  {name}: no tile published (404) — skipping")
            return False
        if resp.status_code == 429 or resp.status_code >= 500:
            # 429 = "too many requests", 500+ = a problem on ETH's server.
            # Both are usually temporary, so wait longer each time and retry.
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
        # Save the file to disk in chunks rather than all at once, since
        # these tiles can be large.
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
            "How many neighbouring pixels to average into one before saving "
            "each tile (default 10: turns ETH's ~10m pixels into ~100m "
            "pixels). Pass 1 to keep full detail. The final mosaic's exact "
            "output resolution is set separately in canopy_mosaic.py's "
            "--resolution."
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
        # --dry-run is for checking which tiles *would* be fetched, e.g. to
        # sanity-check a new boundary, without actually spending the time
        # and bandwidth to download anything.
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
