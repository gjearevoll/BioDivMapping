#' Check if the extent of one object encompasses the extent of another
#'
#' @param x a `SpatRaster`, `SpatVector`, or `sf` object
#' @param y a `SpatRaster`, `SpatVector`, or `sf` object
#' @return TRUE if the extent of x is subset y, otherwise FALSE
#' @importFrom terra ext
#' @importFrom sf st_bbox
is.subset <- function(x, y){ 
  # Function to get extent for both terra and sf objects
  vectorize <- function(obj) {
    if (inherits(obj, "sf")) {
      return(obj)
    } else if (inherits(obj, "SpatVector")) {
      return(st_as_sf(obj))
    } else if (inherits(obj, "SpatRaster")) {
      return(st_as_sf(as.polygons(obj, extent = T)))
    } else {
      stop("Invalid input: x and y should be either SpatRaster, SpatVector, or sf objects.")
    }
  }
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
    x <- st_transform(x, st_crs(y))
  }
  # calculate area of x and check overlap
  area_x <- sum(st_area(x))
  # due to rounding, allow for difference of 0.0001 %
  return(
    as.numeric(abs((sum(st_area(x)) - sum(st_area(st_intersection(x, y))))/sum(st_area(x)))) < 1e-6
  )
}
