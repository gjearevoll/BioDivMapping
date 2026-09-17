

#' Fetch one SoilGrids variable/depth/stat layer clipped to `boundary`, via
#' `soilgrids_import.py`'s remote-VRT windowed read. This is the function to
#' call from defineEnvSource.R.
get_soil_grids <- function(variableName,
                           boundary,
                           dataPath = dataPath,
                           depth = "0-5cm",
                           stat = "mean",
                           resolution = NULL,
                           sync = TRUE) {
  
  uv_bin <- Sys.getenv("UV_BIN", unset = find_uv())
  if (!nzchar(uv_bin) || !file.exists(uv_bin)) {
    message("uv not found on this machine -- installing it now.")
    uv_bin <- install_uv()
  }
  
  # Define the boundary
  boundary_file <- file.path(dataPath, "boundary.shp")
  write_sf(boundary, boundary_file)
  
  if (sync) ensure_python_env(uv_bin = uv_bin)
  
  args <- c(
    "--variable", variableName,
    "--depth", depth,
    "--stat", stat,
    "--boundary", boundary_file,
    "--out-dir", dataPath
  )
  if (!is.null(resolution)) args <- c(args, "--resolution", resolution)
  
  message(sprintf("Fetching SoilGrids %s (%s, %s)...", variableName, depth, stat))
  run_python_script("functions/soilgrids_import.py", args, uv_bin = uv_bin)
  
  # soilgrids_import.py derives its own output filename rather than taking
  # an explicit -o/--output the way canopy's second-stage script does --
  # reconstructing it here so there's something to load. Must be kept in
  # sync with clip_soilgrids_layer()'s `base_name`/"_utm33n.tif" naming.
  output_path <- file.path(
    dataPath,
    sprintf("%s_%s_%s_utm33n.tif", variableName, depth, stat)
  )
  
  message(sprintf("Done. SoilGrids %s raster written to %s", variableName, output_path))
  terra::rast(output_path)
}
