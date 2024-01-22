
#' @title \emph{get_nibio}: This function downloads geographical data directly from Geonorge

#' @description This function checks whether or not you have the correct data stored locally for the forest line and elevation in Norway
#'
#' @return An aggregated raster containing elevation data for Norway.
#'
#' @import terra
#' 

# Check out the full list of data repositories here: https://nedlasting.geonorge.no/geonorge/
# See available data sources: https://nedlasting.geonorge.no/geonorge/Basisdata/

get_nibio <- function(regionGeometryBuffer) {
  if (!file.exists("data/temp/nibio/fl_800_2017_e")) {
    cat("Correct file does not exist. If you do not have access to the forest line raster, contact sam.perrin@ntnu.no.")
    return(NULL)
  }
  forestLine <- terra::rast("data/temp/nibio/fl_800_2017_e")
  
  # Get elevation raster
  elevation <- checkAndImportRast("elevation", regionGeometryBuffer, "data/temp/geonorge")
  
  # Project forest line onto elevation
  reprojForestLine <- terra::project(forestLine, elevation)
  
  # Calculate distance to forest line
  distForestLine <- elevation - reprojForestLine  
  return(distForestLine)
  
}