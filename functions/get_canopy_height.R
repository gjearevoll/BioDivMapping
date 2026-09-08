

#' Step 1: download the ETH canopy-height tiles intersecting `boundary`,
#' block-averaging each one down by `aggregate_factor` (10 -> ~10m to ~100m).
download_canopy_tiles <- function(boundary_file, out_dir = "canopy_tiles",
                                  aggregate_factor = 10,
                                  uv_bin) {
  
  args <- c(
    "--boundary", boundary_file,
    "-o", out_dir,
    "--aggregate-factor", aggregate_factor
  )
  
  message(sprintf("Downloading canopy tiles into %s ...", out_dir))
  run_python_script("functions/download_canopy_tiles.py", args, uv_bin = uv_bin)
  invisible(out_dir)
}

#' Step 2: mosaic the downloaded tiles, clip to `boundary`, reproject and
#' resample to `resolution` metres.
build_canopy_mosaic <- function(tile_folder, output = "canopy_height_output.tif",
                                boundary_file = NULL, resolution = 100,
                                uv_bin) {
  args <- c(tile_folder, "-o", output, "--resolution", resolution)
  if (!is.null(boundary_file)) args <- c(args, "--boundary", boundary_file)
  
  message(sprintf("Mosaicking/clipping/reprojecting to %s ...", output))
  run_python_script("functions/canopy_trondelag_local.py", args, uv_bin = uv_bin)
  invisible(terra::rast(output))
}

#' End-to-end: download + build the canopy height raster for `boundary`.
#' Skips the whole run if `output` already exists, unless overwrite = TRUE --
#' this is the function to call from defineEnvSource.R.
#'

get_canopy_height <- function(boundary,
                              dataPath = dataPath,
                              output = "canopy_height_output.tif",
                              aggregate_factor = 10,
                              resolution = 100,
                              sync = TRUE) {
  
  tilesDir    <- file.path(dataPath, "tiles")
  output_path <- file.path(dataPath, output) 
  
  if (!dir.exists(tilesDir)) {dir.create(tilesDir)}
  
  # Find uv and define bin
  find_uv <- function() {
    candidates <- c(Sys.which("uv"), path.expand("~/.local/bin/uv"), "/usr/local/bin/uv")
    found <- candidates[nzchar(candidates) & file.exists(candidates)]
    if (length(found) > 0) found[[1]] else ""
  }
  uv_bin <- Sys.getenv("UV_BIN", unset = find_uv())
  if (!nzchar(uv_bin) || !file.exists(uv_bin)) {
    message("uv not found on this machine -- installing it now.")
    uv_bin <- install_uv()
  }
  
  # Define the boundary
  boundary_file <- file.path(dataPath, "boundary.shp")
  
  # Save boundary to temp to provide an shp for python
  write_sf(regionGeometry, boundary_file)
  
  if (sync) ensure_python_env(uv_bin = uv_bin)
  
  download_canopy_tiles(
    boundary_file, out_dir = tilesDir, aggregate_factor = 10,
    uv_bin = uv_bin
  )
  
  output_path <- build_canopy_mosaic(
    tilesDir, output = output_path, boundary_file = boundary_file,
    resolution = resolution, uv_bin = uv_bin
  )
  message(sprintf("Done. Canopy height raster written to %s", file.path(dataPath, output)))
  output_path
}
