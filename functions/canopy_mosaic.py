#!/usr/bin/env python3
"""
Mosaic locally-downloaded ETH Global Canopy Height (10m, 2020) tiles, clip to
a boundary, and save as GeoTIFF.

Use this once you've downloaded the individual tiles you need (e.g. via
download_canopy_tiles.py) into a folder — this script does everything else
locally: stitch the tiles together into one image ("mosaic"), cut it down to
just the shape you asked for ("clip"), and convert it to the standard map
projection used elsewhere in the pipeline ("reproject"). Because everything
here works from files already on disk, none of the slowness or rate-limiting
that can happen when reading tiles directly over the internet applies.

Usage:
    uv run canopy_mosaic.py /path/to/tiles/folder --boundary /path/to/boundary.shp
    uv run canopy_mosaic.py /path/to/tiles/folder --boundary boundary.shp -o output.tif --resolution 100

The final output resolution is set by --resolution (default 100m) at the
reprojection step, independent of whatever resolution the input tiles are
in — so it doesn't matter whether tiles were shrunk on download (see
download_canopy_tiles.py's --aggregate-factor) or are still at full detail.
"""

import argparse
import glob
import os

import geopandas as gpd
import rasterio
from rasterio.mask import mask
from rasterio.merge import merge
from rasterio.warp import Resampling, calculate_default_transform, reproject

OUTPUT_CRS = "EPSG:25833"  # ETRS89 / UTM zone 33N — Norway's standard map projection
DEFAULT_OUTPUT = "canopy_height_output.tif"
DEFAULT_RESOLUTION_M = 100  # final output pixel size, regardless of input tile resolution

# Accept the usual raster tile file-name endings, both lower- and upper-case.
TILE_GLOB_PATTERNS = ("*.tif", "*.tiff", "*.TIF", "*.TIFF")


def get_boundary(path: str) -> gpd.GeoDataFrame:
    """Load a boundary from a local vector file (shapefile, GeoJSON, GPKG —
    anything the geopandas library can read), and combine it into one single
    polygon shape in the standard WGS84 (latitude/longitude) map projection."""
    gdf = gpd.read_file(path)
    # .buffer(0) is a common trick to repair minor shape glitches (like a
    # boundary line that crosses itself) before we try to combine shapes.
    gdf["geometry"] = gdf.geometry.buffer(0)
    gdf = gdf.dissolve().reset_index(drop=True)
    if gdf.crs is None:
        raise RuntimeError(
            f"{path!r} has no CRS defined — can't reproject it safely."
        )
    return gdf.to_crs("EPSG:4326")


def find_tiles(folder: str) -> list[str]:
    """List every downloaded tile file sitting in `folder`."""
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
    """Stitch the individual tiles together into one image, but only for the
    area covered by `bounds_native` (given in the tiles' own map projection).
    The result is written straight to a file on disk rather than being held
    in memory all at once, since real tiles can add up to a lot of data."""
    srcs = [rasterio.open(p) for p in tile_paths]
    try:
        merge(
            srcs,
            bounds=bounds_native,
            dst_path=tmp_mosaic_path,
            dst_kwds={"compress": "deflate", "predictor": 2},  # shrink file size, no quality loss
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
    """Cut the stitched-together mosaic down to just the boundary's shape,
    then convert it into the standard map projection (and pixel size) the
    rest of the pipeline expects."""
    geoms = [boundary_native.geometry.iloc[0].__geo_interface__]

    # Step 1: clip to the boundary's shape, writing to a temporary file.
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

    # Step 2: reproject the clipped result into the target map projection
    # and pixel size (this is what actually makes the output line up with
    # the rest of the pipeline's data).
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
                    # "bilinear" blends nearby pixels together when resizing,
                    # which gives a smoother result than picking one pixel
                    # verbatim — appropriate for a continuous measurement
                    # like canopy height (as opposed to e.g. a land-cover
                    # category, where blending wouldn't make sense).
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
        required=True,
        help="Path to a boundary vector file (shapefile/GeoJSON/GPKG/...).",
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

    print(f"Loading boundary ({args.boundary})...")
    boundary = get_boundary(args.boundary)

    # The tiles are already in their own map projection (not WGS84), so
    # convert the boundary to match before comparing the two.
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
