#' Determine the Best UTM Coordinate Reference System for a Spatial Object
#'
#' This function calculates the most suitable UTM CRS for a given spatial object. It supports objects
#' of class 'sf', 'sfc', 'SpatRaster', and 'SpatVector'. The function calculates the
#' mean latitude and longitude of the object to determine the appropriate UTM zone and returns the
#' corresponding CRS Proj4 string. 
#'
#' @param x A spatial object of class 'sf', 'sfc', 'SpatRaster', or 'SpatVector'. 
#'
#' @return A character string (Proj4) representing the UTM CRS. 
#'
#' @export
#'
#' @examples
#' # Assuming 'spatial_obj' is a valid 'sf', 'sfc', 'SpatRaster', or 'SpatVector' object
#' # utm_crs_string <- utmCRS(spatial_obj)
#' # print(utm_crs_string)
#'
#' @importFrom sf st_crs st_transform st_bbox st_as_sf
#' @importFrom terra crs vect project as.polygons

utmCRS <- function(x){
  
  
  if (!inherits(x, c("sf", "sfc", "SpatRaster", "SpatVector"))) {
    stop("'utmCRS' only supports 'sf', 'sfc', 'SpatRaster', and 'SpatVector' objects.")
  }
  # convert terra to sf
  if(inherits(x, "SpatRaster")) {
    x <- as.polygons(x, extent = T) |> 
      terra::project("epsg:4326") |>
      st_as_sf() 
  } else if(inherits(x, "SpatVector")) {
    x <- st_as_sf(x) |>
      st_as_sf()  |>
      st_segmentize(dfMaxLength = 10000) |>
      st_transform(4326)
  } else if(inherits(x, c("sf", "sfc"))){
    # convert to lat/long
    crs <- st_crs(x)
    if (st_crs(crs)$units_gdal != "degree") {
      x <- st_transform(x, 4326)
    }
  }
  bbox <- st_bbox(x)
  
  # get mean mean lat/long and corresponding UTM zone
  meanLon <- mean(st_bbox(x)[c(1,3)])  # mean longitude
  meanLat <- mean(st_bbox(x)[c(2,4)])  # mean latitude
  utmZone <- 1 + ((meanLon + 180) %/% 6)  # UTM zone
  
  # define UTM crs (assuming northern hemisphere)
  crs <- sprintf("+proj=utm +zone=%d +datum=WGS84 +units=m +no_defs +%s", 
                 utmZone, if(meanLat >= 0) "north" else "south")
  return(crs)
}