
### ENVIRONMENTAL SOURCE DEFINITION ####

# This script defines parameters for extracting the necessary environmental data from their
# various sources. It relies on the functions found in functions/import/environment that begin
# with 'get_'. A full description of each data source can be found at 
# https://github.com/gjearevoll/BioDivMapping/tree/main/data/temp 


if(inherits(regionGeometryBuffer, c("sf", "sfc"))){
  regionGeometryBuffer <- terra::vect(regionGeometryBuffer)
}

### 1. geonorge ####
if (dataSource == "geonorge") { 
  if (file.exists("data/temp/geonorge/elevationRaster.tiff")) { 
    elevation <- rast("data/temp/geonorge/elevationRaster.tiff")
  } else {
    if (!dir.exists("data/temp/geonorge")) {
      dir.create("data/temp/geonorge", recursive = TRUE)
    }
    elevation <- get_geonorge(targetDir = tempFolderName)
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
  rasterisedVersion <- get_modis(regionGeometry, projCRS, focalParameter)

### 5. CORINE ###  
} else if (dataSource == "corine") {
  rasterisedVersion <- get_corine()
}

### merge with requested download area to make missing data explicit
rasterisedVersion <- extend(rasterisedVersion, terra::project(regionGeometryBuffer, rasterisedVersion), snap = "out")

