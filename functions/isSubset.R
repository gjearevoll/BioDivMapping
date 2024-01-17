#' Check if the extent of one object encompasses the extent of another
#'
#' @param x a `SpatRaster`, `SpatVector`, or `sf` object
#' @param y a `SpatRaster`, `SpatVector`, or `sf` object
#' @param dfMaxLength maximum length of a line segment. See `sf::st_segmentize()`.
#' @return TRUE if the x is a subset of y, otherwise FALSE.
#' @importFrom terra ext
#' @importFrom sf st_bbox
isSubset <- function(x, y, dfMaxLength){ 
  suppressMessages(sf_use_s2(FALSE))  # conduct all calculation assuming planar geometries 
  library(tidyterra)
  # vectorise
  x <- vectorize(x)
  y <- vectorize(y)
  # check valid crs
  if(is.na(st_crs(x)) & is.na(st_crs(x))){
    warning("CRS not defined for x or y; assuming same CRS for both.")
  } else if (is.na(st_crs(x))){
    warning("CRS not defined for x; assuming same CRS as y.")
    x <- st_transform(x, st_crs(y))
  } else if (is.na(st_crs(y))){
    warning("CRS not defined for y; assuming same CRS as x.")
    y <- st_transform(y, st_crs(x))
  } else if(st_crs(x) != st_crs(y)) {
    # match projections
    x <- x |> 
      st_segmentize(dfMaxLength = 10000) |>
      st_transform(st_crs(y))
  }
  # calculate area of x and check overlap
  area_x <- sum(st_area(x))
  # due to rounding, allow for difference of 0.01 %
  suppressMessages(subset <- as.numeric(abs((sum(st_area(x)) - sum(st_area(st_intersection(x, y))))/sum(st_area(x)))) < 1e-4)
  suppressMessages(sf_use_s2(TRUE))
  return(subset)
}

# Function to get extent for both terra and sf objects
vectorize <- function(obj, dfMaxLength) {
  if (inherits(obj, c("sf", "sfc"))) {
    break
  } else if (inherits(obj, "SpatVector")) {
    obj <- st_as_sf(obj)
  } else if (inherits(obj, "SpatRaster")) {
    obj <- st_as_sf(as.polygons(obj, extent = T))
  } else {
    stop("Invalid input: x and y should be either SpatRaster, SpatVector, or sf objects.")
  }
  return(obj)
}


