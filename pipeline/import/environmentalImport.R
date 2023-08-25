

#### ENVIRONMENTAL DATA IMPORT ####

# The following script imports our various forms of environmental data and processes them, based on the type of data
# and other specifications related to the source.

# NOTE: Before runnign this script, the speciesImport.R script needs to have been run.

library(raster)
library(terra)
library(sf)

###-----------------###
### 1. Preparation ####
###-----------------###

externalImport <- FALSE

# Run script to define geographical region and resolution we are working with 
# Initialise folders for storage of all run data
if (!exists("dateAccessed")) {
  dateAccessed <- as.character(Sys.Date())
}

speciesDataList <- readRDS(paste0("data/run_", dateAccessed, "/temp/speciesDataImported.RDS"))
regionGeometry <- readRDS(paste0("data/run_", dateAccessed, "/regionGeometry.RDS"))

# The following is a list of the various environmental variables we have available.
parameters <- read.csv("data/external/focalCovariates.csv")
parameters <- parameters$parameters[parameters$selected]

# Correct crs
newproj <- "+proj=longlat +ellps=WGS84 +no_defs"

###--------------------###
### 2. Dataset Import ####
###--------------------###

# define region with buffer 
regionGeometry_buffer <- vect(st_buffer(regionGeometry, 20000))

# Import and correctly project all covariate data selected in the csv file
parametersCropped <- lapply(parameters, FUN = function(x) {
  
  # Import rasters from either Wallace server or a local folder
  if (externalImport == TRUE) {
    targetDatabase <- "environmental_covariates"
    source("utils/initiateWallaceConnection.R")
    dataRaw <- dbReadTable(con, x)
    rasterisedVersion <- rast(rasterFromXYZ(dataRaw))
  } else {
    # rasterisedVersion2 <- raster(paste0("data/external/environmentalCovariates/",x, ".tiff"))
    rasterisedVersion <- rast(paste0("data/external/environmentalCovariates/",x, ".tiff"))
  }
  
  # Need to project, crop and scale the raster
  # note1: nesting arguments to avoid writing large objects to memory
  # note2: projecting raster takes a long time and is best done last
  scale(
    crop(rasterisedVersion,
         project(regionGeometry_buffer, rasterisedVersion), 
         snap = "out", mask = T)
  )
  # dataFinal <- scale(
  #   mask(
  #     crop(
  #       project(rasterisedVersion, regionGeometry_buffer), 
  #       regionGeometry_buffer),
  #     regionGeometry_buffer)
  #   ) 
  # foo <- scale(
  #   crop(project(rasterisedVersion, regionGeometry_buffer),
  #        regionGeometry_buffer, snap = "out", mask = T)
  # )
  # crs(rasterisedVersion) <- "+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs"
  # dataProjected <- projectRaster(rasterisedVersion2, crs=newproj, res=0.1)
  # dataCropped <- crop(dataProjected, as_Spatial(st_buffer(regionGeometry, 20000)))
  # dataMasked <- mask(dataCropped, as_Spatial(st_buffer(regionGeometry, 20000)))
  # names(dataMasked) <- x
  # dataFinal <- scale(dataMasked)
  # dataFinal
})
# project all rasters to the one with the highest resolution and combine 
reference <- which.min(sapply(parametersCropped, res)[1,])
parametersCropped <- do.call(c, 
                             lapply(parametersCropped, function(x){
                               project(x, parametersCropped[[reference]])
                             }))
# assign names
names(parametersCropped) <- parameters

###--------------------###
### 3. Dataset Upload ####
###--------------------###

# Save both to temp file for model processing and visualisation folder for mapping
writeRaster(parametersCropped, paste0("data/run_", dateAccessed,"/temp/environmentalDataImported.tiff"), overwrite=TRUE)
writeRaster(parametersCropped, "visualisation/hotspotMaps/data/covariateDataList.tiff", overwrite=TRUE)
# saveRDS(parametersCropped, paste0("data/run_", dateAccessed,"/temp/environmentalDataImported.RDS"))
# saveRDS(parametersCropped, "visualisation/hotspotMaps/data/covariateDataList.RDS")
