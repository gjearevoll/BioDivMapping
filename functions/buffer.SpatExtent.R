#' buffer_ext
#'
#' @param x spatial extent as returned by SpatRaster
#' @param portion Description of argument
#' @param distance Description of argument
#'
#' @return Description of the return value
#'
#' @export
#' 
library(terra)

# Custom buffer function for SpatExtent class
buffer.SpatExtent <- function(x, portion = NA, distance = NA, lonlat = NA) {
  # Perform the desired operations for SpatExtent objects
  # Replace this with your custom implementation
  message("Custom buffer function for SpatExtent called.")
  
  if(is.na(lonlat) | !is.logical(lonlat)) stop("Error: lonlat should be TRUE or FALSE")
  if(is.na(portion) & is.na(distance)) stop("Error: must provide either portion or dincance.")
  # Load required packages
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required but not installed.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }
  
  if(is.na(distance) & is.numeric(portion))
    distance <- matrix(c(mean(x[1:2]), x[3],
             mean(x[1:2]), x[4],
             x[1], mean(x[3:4]),
             x[2], mean(x[3:4])), 
           ncol = 2, byrow = T) %>% 
      terra::distance(lonlat = lonlat, 
                      sequential = T) %>% 
      {.[c(2,4)]*portion} %>% 
      max()
  
  # add distance to extent
  if(lonlat)
    buffer(vect(x, crs = "epsg:4326"), distance) %>% ext 
  else
    buffer(vect(x), distance) %>% ext 
    
}

# Register the custom buffer function as an S3 method
setMethod("buffer", signature(x = "SpatExtent"), buffer.SpatExtent)
