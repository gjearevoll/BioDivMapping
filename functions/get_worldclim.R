
#' @title \emph{get_worldclim}: This function downloads and merges worldClim data for a study area

#' @description This function downloads and merges worldClim data for a study area
#'
#' @param coords Dataframe with x and y sample coordinates.
#' @param var character. One of variable names provided by worldclim. Valid varnames are "tmin", "tmax", "tavg", "prec", "wind", "vapr", and "bio"
#' @param res The resolution of WorldClim data to download; options are 0.5, 2.5, 5, and 10 arc-minutes (default = 0.5).
#' @param buff A buffer area around sample points for cropping the data layers, expressed as a proportion of the spatial extent for the coordinates (default = 0).
#' @param path path for where to save downloaded worldclim data (with default = NA, data is downloaded into temporary folder that is later deleted).
#' @param dfMaxLength maximum length of a line segment. See `sf::st_segmentize()`.
#'
#' @details
#' If res = 0.5 then the individual WorldClim tiles that cover the sample coordinates are downloaded and merged. If res > 2.5 then global layers are downloaded.
#' The buffer area maintains a large extent for the final cropped data layers around the sample coordinates. e.g. buff = 0.01 creates a 1% buffer area around the coordinates.
#' Code adapted from https://github.com/TheWangLab/algatr
#' 
#' @return A SpatRaster of WorldClim layers.

get_worldclim <- function(coords, var, res = 0.5, buff = 0, path = NA, dfMaxLength = 10000) {
  # stop if variable not provided
  stopifnot(var %in% c("tavg", "tmin", "tmax", "prec", "bio", 
                       "bioc", "elev", "wind", "vapr", "srad"))
  
  # Convert coordinates
  if("SpatExtent" %in% class(coords)) { # if terra spatial extent
    ext <- coords
    if(xmin(ext) < -180 | xmax(ext) >180 | ymin(ext) < -90 | ymax(ext) >90){
      stop("'coords' not in wgs84 lat/long CRS. Please check CRS of 'coords'.")
    } else {
      warning("Assuming 'coords' extent is in WGS84 lat/long CRS.")
    }
  } else if("SpatRaster" %in% class(coords)) {
    ext <- coords |> 
      as.polygons(extent = T) |>
      terra::project("epsg:4326") |>
      ext()
  } else if("SpatVector" %in% class(coords)) {
    ext <- ext(terra::project(coords, "epsg:4326"))
  } else if(inherits(coords, c("sf", "sfc"))){  # if sf simple feature
    # convert 
    coords <- coords |> 
      st_segmentize(dfMaxLength = 10000) |>
      st_transform(4326)
    # define extent 
    ext <- terra::ext(st_bbox(coords)[c(1,3,2,4)]) 
  } 
  # buffer extent
  ext <- buffer(ext, portion = buff, lonlat = T)
  
  # define raster of worldclim tiles
  r <- terra::rast(vals = 1:72, nrows = 6, ncols = 12, ext = terra::ext(c(-180, 180, -90, 90))) 
  # Identify WorldClim tiles to download
  r_nums <- terra::extract(r, vect(ext), ID = FALSE)
  r_xy <- terra::xyFromCell(r, r_nums[[1]]) |>
    matrix(ncol = 2)

  # create temporary download path if non is provided
  if(is.na(path)){
    delete_folder <- T
    path <- paste0(getwd(), "/tmp")
  } else {
    delete_folder <- F
  }
  
  # Download and merge tiles
  if (res == 0.5) {
    for (i in seq_along(r_nums[[1]])) {
      message(paste0("Downloading WorldClim tile ", i, " of ", length(r_nums[[1]]) ,"..."))
      wc <- geodata::worldclim_tile(var = var, lon = r_xy[i, 1], lat = r_xy[i, 2], path = path)
      if(i == 1)
        wclim <- wc
      else
        wclim <- terra::merge(wclim, wc)
    }
  } else {
    wclim <- geodata::worldclim_global(var = var, res = res, path = path)
  }

  # # Assign names to bioclim vars
  # names(wclim) <- c(
  #   "bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10",
  #   "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19"
  # )

  if(delete_folder) {
    unlink(path, recursive = TRUE)
  } 

  return(wclim)
}
