
### ENVIRONMENTAL SOURCE DEFINITION ####

# This script defines parameters for extracting the necessary environmental data from their
# various sources. It relies on the functions found in fucntions/import/environment.
if(inherits(regionGeometryBuffer, c("sf", "sfc"))){
  regionGeometryBuffer <- terra::vect(regionGeometryBuffer)
}
### 1. geonorge ####
if (dataSource == "geonorge") { 
  # check if encompassing corine alreadydownloaded
  elevation <- checkAndImportRast("elevation", regionGeometryBuffer, dataPath)
  # download and save if missing
  if(is.null(elevation)){
    # download
    elevation <- get_geonorge(targetDir = tempFolderName)
    # save
    file_path <- generateRastFileName(elevation, dataSource, "elevation", dataPath)
    writeRaster(elevation, filename = file_path, overwrite = TRUE)
  }
  
  # Now get the raster you're actually looking for
  if (focalParameter == 'elevation') {
    rasterisedVersion <- elevation
  } else {
    rasterisedVersion <- terra::terrain(elevation, v=focalParameter, unit='degrees', neighbors=8)
  }
  
### 2. worldclim ####  
} else if (dataSource == "worldclim") {
  
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
  annualStack <- get_worldclim(regionGeometryBuffer, var, 0.5)
  
  # average
  rasterisedVersion <- mean(annualStack)
  
### 3. SSB ####
} else if (dataSource == "ssb") {
    rasterisedVersion <- get_ssb(focalParameter)
    
### 4. MODIS ####    
} else if (dataSource == "modis") {
  rasterisedVersion <- get_modis(regionGeometryBuffer, projCRS, focalParameter)
} else if (dataSource == "corine") {
  rasterisedVersion <- get_corine()
}

### merge with requested download area to make missing data explicit
rasterisedVersion <- extend(rasterisedVersion, terra::project(regionGeometryBuffer, rasterisedVersion), snap = "out")

### generate descriptive file name and save
file_path <- generateRastFileName(rasterisedVersion, focalParameter, dataPath)
writeRaster(rasterisedVersion, filename = file_path, overwrite = TRUE)
