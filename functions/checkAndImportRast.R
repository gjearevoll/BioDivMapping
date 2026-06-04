# Define the path for data source
checkAndImportRast <- function(focalParameter, regionGeometryBuffer, dataPath, temporal = FALSE, 
                               yearInterval = NA, fileType = "tiff", quiet = FALSE) {
  # Initialize the raster found flag
  raster <- NULL
  
  # List all files in the directory that match the parameter
  filePattern <- paste0(focalParameter, sprintf(".*\\.%s$", fileType))
  fileList <- list.files(path = dataPath, pattern = filePattern, full.names = TRUE)
  
  # Check each file
  for (file in fileList) {
    rastObj <- terra::rast(file)
    
    # First make sure all correct yuears are there if we're looking at a temporal dataset
    if (temporal == TRUE) {
      if (all(yearInterval %in% names(rastObj))) {
        rastObj <- rastObj[[as.character(yearInterval)]]
        } else {next}
    }
    
    # Now check if raster encompasses our region
    if (isSubset(regionGeometryBuffer, rastObj)) {
      raster <- rastObj
      if (!temporal & nlyr(raster) > 1) {
        yearsAvailable <- names(raster)
        latestYear <- max(as.integer(yearsAvailable))
        raster <- raster[[as.character(latestYear)]]
      }
      if (!quiet) {
        message(sprintf("Raster for '%s' encompassing region found and imported successfully!\n", focalParameter))
      }
      break
    }
  }
  
  return(raster)
}
