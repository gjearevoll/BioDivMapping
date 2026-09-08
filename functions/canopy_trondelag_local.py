#!/usr/bin/env python3
"""
Mosaic locally-downloaded ETH Global Canopy Height (10m, 2020) tiles, clip to
a boundary, and save as GeoTIFF.

Use this once you've downloaded the individual COG tiles you need (e.g. via
download_canopy_tiles.py) into a folder — this script does everything else
locally: mosaic -> clip -> reproject. No repeated remote range-requests, so
none of the rate-limiting/slowness from the VRT approach applies.

Usage:
    uv run canopy_trondelag_local.py /path/to/tiles/folder --boundary /path/to/boundary.shp
    uv run canopy_trondelag_local.py /path/to/tiles/folder --boundary boundary.shp -o output.tif --resolution 100

If --boundary is omitted, falls back to fetching the Trøndelag fylke
boundary (the original pilot region) from the same source as before.

The final output resolution is set by --resolution (default 100m) at the
reprojection step, independent of whatever resolution the input tiles are
in — so it doesn't matter whether tiles were aggregated on download
(see download_canopy_tiles.py's --aggregate-factor) or are still native 10m.
"""

import argparse
import glob
import io
import os

import geopandas as gpd
import rasterio
import requests
from rasterio.mask import mask
from rasterio.merge import merge
from rasterio.warp import Resampling, calculate_default_transform, reproject

OUTPUT_CRS = "EPSG:25833"  # ETRS89 / UTM zone 33N — Norway standard
DEFAULT_OUTPUT = "canopy_height_output.tif"
DEFAULT_RESOLUTION_M = 100  # final output pixel size, regardless of input tile resolution

# Accept the usual raster tile extensions
TILE_GLOB_PATTERNS = ("*.tif", "*.tiff", "*.TIF", "*.TIFF")


def get_trondelag_boundary() -> gpd.GeoDataFrame:
    """Fetch an up-to-date Trøndelag county boundary (post-2020 Norwegian
    regional reform — old GADM Norway files with 'Nord-/Sør-Trøndelag' won't
    match), dissolved to a single polygon in EPSG:4326."""
    url = (
        "https://raw.githubusercontent.com/robhop/fylker-og-kommuner-2020/"
        "master/Fylker-M.geojson"
    )
    resp = requests.get(url, timeout=60)
    resp.raise_for_status()
    gdf = gpd.read_file(io.BytesIO(resp.content))

    name_col = "fylkesnavn" if "fylkesnavn" in gdf.columns else gdf.columns[0]
    trondelag = gdf[
        gdf[name_col].str.contains("Trøndelag", case=False, na=False)
    ].copy()
    if trondelag.empty:
        raise RuntimeError("No feature matching 'Trøndelag' found in boundary source.")

    trondelag["geometry"] = trondelag.geometry.buffer(0)
    trondelag = trondelag.dissolve().reset_index(drop=True)
    if trondelag.crs is None:
        trondelag = trondelag.set_crs("EPSG:4326")
    return trondelag


def get_boundary(path: str | None) -> gpd.GeoDataFrame:
    """Load a boundary from a local vector file (shapefile, GeoJSON, GPKG,
    anything geopandas can read), dissolved to a single polygon in EPSG:4326.
    Falls back to fetching the Trøndelag fylke boundary if no path is given."""
    if path is None:
        return get_trondelag_boundary()

    gdf = gpd.read_file(path)
    gdf["geometry"] = gdf.geometry.buffer(0)
    gdf = gdf.dissolve().reset_index(drop=True)
    if gdf.crs is None:
        raise RuntimeError(
            f"{path!r} has no CRS defined — can't reproject it safely."
        )
    return gdf.to_crs("EPSG:4326")


def find_tiles(folder: str) -> list[str]:
    tiles = []
    for pattern in TILE_GLOB_PATTERNS:
        tiles.extend(glob.glob(os.path.join(folder, pattern)))
    tiles = sorted(set(tiles))
    if not tiles:
        raise RuntimeError(
            f"No .tif/.tiff files found in {folder!r}. "
            "Check the path and that the tiles have been downloaded there."
        )
    return tiles


def mosaic_tiles_to_file(tile_paths: list[str], bounds_native, tmp_mosaic_path: str):
    """Merge local tiles, restricted to `bounds_native` (in the tiles' own CRS),
    writing straight to disk via merge()'s dst_path. This makes rasterio do a
    windowed read per source tile instead of loading each full tile into
    memory — important since real tiles can be large."""
    srcs = [rasterio.open(p) for p in tile_paths]
    try:
        merge(
            srcs,
            bounds=bounds_native,
            dst_path=tmp_mosaic_path,
            dst_kwds={"compress": "deflate", "predictor": 2},
        )
    finally:
        for s in srcs:
            s.close()
    return tmp_mosaic_path


def clip_and_reproject(
    tmp_mosaic_path: str,
    boundary_native: gpd.GeoDataFrame,
    out_path: str,
    resolution_m: float,
) -> str:
    geoms = [boundary_native.geometry.iloc[0].__geo_interface__]

    tmp_clipped_path = out_path + ".clipped_tmp.tif"
    with rasterio.open(tmp_mosaic_path) as src:
        clipped, clipped_transform = mask(
            src, geoms, crop=True, filled=True, nodata=src.nodata
        )
        clipped_meta = src.meta.copy()
        clipped_meta.update(
            {
                "height": clipped.shape[1],
                "width": clipped.shape[2],
                "transform": clipped_transform,
            }
        )
    with rasterio.open(tmp_clipped_path, "w", **clipped_meta) as dst:
        dst.write(clipped)

    # Reproject the clipped result to the target CRS
    with rasterio.open(tmp_clipped_path) as tmp_src:
        transform, width, height = calculate_default_transform(
            tmp_src.crs,
            OUTPUT_CRS,
            tmp_src.width,
            tmp_src.height,
            *tmp_src.bounds,
            resolution=(resolution_m, resolution_m),
        )
        out_meta = tmp_src.meta.copy()
        out_meta.update(
            {
                "crs": OUTPUT_CRS,
                "transform": transform,
                "width": width,
                "height": height,
                "compress": "deflate",
                "predictor": 2,
            }
        )
        with rasterio.open(out_path, "w", **out_meta) as dst:
            for band_idx in range(1, tmp_src.count + 1):
                reproject(
                    source=rasterio.band(tmp_src, band_idx),
                    destination=rasterio.band(dst, band_idx),
                    src_transform=tmp_src.transform,
                    src_crs=tmp_src.crs,
                    dst_transform=transform,
                    dst_crs=OUTPUT_CRS,
                    resampling=Resampling.bilinear,
                )

    os.remove(tmp_clipped_path)
    return out_path


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "tile_folder",
        help="Folder containing the downloaded canopy height GeoTIFF tiles",
    )
    parser.add_argument(
        "-o", "--output", default=DEFAULT_OUTPUT, help="Output GeoTIFF path"
    )
    parser.add_argument(
        "--boundary",
        default=None,
        help=(
            "Path to a boundary vector file (shapefile/GeoJSON/GPKG/...). "
            "Defaults to fetching the Trøndelag fylke boundary."
        ),
    )
    parser.add_argument(
        "--resolution",
        type=float,
        default=DEFAULT_RESOLUTION_M,
        help=(
            f"Final output pixel size in metres (default {DEFAULT_RESOLUTION_M}). "
            "Applied at the reprojection step regardless of input tile resolution."
        ),
    )
    args = parser.parse_args()

    print(f"Looking for tiles in {args.tile_folder!r}...")
    tile_paths = find_tiles(args.tile_folder)
    print(f"Found {len(tile_paths)} tile(s):")
    for p in tile_paths:
        print(f"  {p}")

    print(f"Loading boundary ({args.boundary or 'Trøndelag fylke (default)'})...")
    boundary = get_boundary(args.boundary)

    with rasterio.open(tile_paths[0]) as first_tile:
        tile_crs = first_tile.crs
    boundary_native = boundary.to_crs(tile_crs)
    bounds_native = tuple(boundary_native.total_bounds)  # (minx, miny, maxx, maxy)

    tmp_mosaic_path = args.output + ".mosaic_tmp.tif"
    print("Mosaicking local tiles (cropped to boundary bounding box, streamed to disk)...")
    mosaic_tiles_to_file(tile_paths, bounds_native, tmp_mosaic_path)

    print(f"Clipping to boundary and reprojecting to {OUTPUT_CRS} at {args.resolution}m...")
    result_path = clip_and_reproject(
        tmp_mosaic_path, boundary_native, args.output, args.resolution
    )
    os.remove(tmp_mosaic_path)

    print("\nDone. File written:")
    print(f"  {result_path}")


if __name__ == "__main__":
    main()
