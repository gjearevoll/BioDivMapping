generateRastFileName <- function(raster, parameter, dataPath, fileType = "tiff") {
  # Extract CRS information
  crsInfo <- terra::crs(raster, describe = TRUE)
  
  # Construct info string based on CRS and extent
  if (!is.na(crsInfo$authority)) {
    extentObj <- terra::ext(raster)
    info <- sprintf("%s%s_X%s_%s_Y%s_%s",
                    crsInfo$authority, crsInfo$code, 
                    extentObj[1], extentObj[2], extentObj[3], extentObj[4])
  } else {
    # Use digest for non-descriptive CRS
    info <- digest::digest(list(terra::crs(raster), terra::ext(raster)))
  }
  
  # Construct the file path with raster information
  filePath <- sprintf("%s/%s_%s.%s", dataPath, parameter, info, fileType)
  return(filePath)
}

