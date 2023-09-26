
### ENVIRONMENTAL SOURCE DEFINITION ####

# This script defines parameters for extracting the necessary environmental data from their
# various sources. It relies on the functions found in fucntions/import/environment.

### 1. geonorge ####
if (dataSource == "geonorge") {
  
  if (file.exists("data/temp/geonorge/elevationRaster.tiff")) { 
    elevation <- rast("data/temp/geonorge/elevationRaster.tiff")
  } else {
    elevation <- get_geonorge(targetDir = "data/run_2023-09-21/temp/")
    writeRaster(elevation, "data/temp/geonorge/elevationRaster.tiff", overwrite=TRUE)
  }
  
  # Now get the raster you're actually looking for
  if (focalParameter == 'elevation') {
    rasterisedVersion <- elevation
  } else {
    rasterisedVersion <- terra::terrain(elevation, v=focalParameter, unit='degrees', neighbors=8)
  }
  
### 2. worldclim ####  
} else if (dataSource == "worldclim") {
  # Set extent for download
  regionExtent <- ext(regionGeometry_buffer)
  
  print(paste0("Downloading ", focalParameter," from ", dataSource))
  
  # prep download variables
  recode_vector <- c("temperature" = "tavg",
                     "minimum temperature" = "tmin",
                     "maximum temperature" = "tmax",
                     "precipitation" = "prec",
                     "bio" = "bio",
                     "bioc" = "bioc",
                     "elevation" = "elev",
                     "wind speed" = "wind",
                     "vapor pressure" = "vapr",
                     "solar radiation" = "srad")
  
  var <- recode_vector[focalParameter]
  
  # download
  annualStack <- get_worldclim(regionExtent, var, 0.5)
  
  # average
  rasterisedVersion <- mean(annualStack)
}
