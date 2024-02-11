# Define the path for data source
checkAndImportRast <- function(parameter, regionGeometry, dataPath, fileType = "tiff", quiet = FALSE) {
  # Initialize the raster found flag
  raster <- NULL
  
  # List all files in the directory that match the parameter
  filePattern <- paste0(parameter, sprintf(".*\\.%s$", fileType))
  fileList <- list.files(path = dataPath, pattern = filePattern, full.names = TRUE)
  
  # Check each file
  for (file in fileList) {
    rastObj <- terra::rast(file)
    if (isSubset(regionGeometry, rastObj)) {
      raster <- rastObj
      if (!quiet) {
        message(sprintf("Raster for '%s' encompassing region found and imported successfully!\n", parameter))
      }
      break
    }
  }
  
  return(raster)
}
