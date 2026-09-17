

#' Fetch one BIOPAR biophysical parameter (FAPAR/LAI/FCOVER) clipped to
#' `boundary`: the annual maximum, averaged across the last `years` calendar
#' years, via `biopar_import.py`'s CDSE OData/S3 pipeline (free, windowed
#' reads of CGLS's pre-computed products -- NOT the paid openEO Processing
#' API this used previously). This is the function to call from
#' defineEnvSource.R.
get_biopar <- function(variableName,
                       boundary,
                       dataPath = dataPath,
                       resolution = NULL,
                       crs = "EPSG:25833",
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
    "--biopar", variableName,
    "--years", 10,
    "--boundary", boundary_file,
    "--crs", crs,
    "--out-dir", dataPath
  )
  if (!is.null(resolution)) args <- c(args, "--resolution", resolution)

  # Auth is CDSE S3 access-key credentials (AWS_ACCESS_KEY_ID /
  # AWS_SECRET_ACCESS_KEY / AWS_S3_ENDPOINT env vars) -- not an OIDC
  # device-flow login, no cached refresh token, no --login step. Generate a
  # key pair once at https://eodata-s3keysmanager.dataspace.copernicus.eu/
  # and set those three env vars on whichever machine this runs on.
  # biopar_import.py's own require_s3_credentials() checks for them before
  # doing anything else and fails fast with an actionable message if any are
  # missing -- run_python_script() below surfaces that failure as an R error
  # (see functions/run_python.R), so there is nothing further to check here.
  message(sprintf("Fetching BIOPAR %s (%d-year max average)...", variableName, 10))
  run_python_script("functions/biopar_import.py", args, uv_bin = uv_bin)

  # biopar_import.py derives its own output filename rather than taking an
  # explicit -o/--output -- reconstructing it here so there's something to
  # load. The CRS tag is derived from `crs` the same way Python's crs_tag()
  # derives it from --crs (an "epsg<code>" tag, not a hard-coded label) --
  # keep this in sync with main()'s `out_path` naming if that changes.
  crs_epsg <- sf::st_crs(crs)$epsg
  output_path <- file.path(
    dataPath,
    sprintf("%s_max_%dyr_avg_epsg%d.tif", variableName, 10, crs_epsg)
  )

  message(sprintf("Done. BIOPAR %s raster written to %s", variableName, output_path))
  terra::rast(output_path)
}
