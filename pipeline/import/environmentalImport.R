

#### ENVIRONMENTAL DATA IMPORT ####

# The following script imports our various forms of environmental data and processes them, based on the type of data
# and other specifications related to the source.

# NOTE: Before runnign this script, the speciesImport.R script needs to have been run.

library(raster)
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


# Import and correctly project all covariate data selected in the csv file
parametersCropped <- lapply(parameters, FUN = function(x) {
  
  if (externalImport == TRUE) {
    targetDatabase <- "environmental_covariates"
    source("utils/initiateWallaceConnection.R")
    dataRaw <- dbReadTable(con, x)
    rasterisedVersion <- rasterFromXYZ(dataRaw)
  } else {
    rasterisedVersion <- raster(paste0("data/external/environmentalCovariates/",x, ".tiff"))
  }
  crs(rasterisedVersion) <- "+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs"
  dataProjected <- projectRaster(rasterisedVersion, crs=newproj, res=0.1)
  dataCropped <- crop(dataProjected, as_Spatial(st_buffer(regionGeometry, 20000)))
  dataMasked <- mask(dataCropped, as_Spatial(st_buffer(regionGeometry, 20000)))
  names(dataMasked) <- x
  dataFinal <- scale(dataMasked)
  dataFinal
})
names(parametersCropped) <- parameters

###--------------------###
### 3. Dataset Upload ####
###--------------------###

# For now we're just doing this to the data/temp folder, later this will go to Wallace
saveRDS(parametersCropped, paste0("data/run_", dateAccessed,"/temp/environmentalDataImported.RDS"))
saveRDS(parametersCropped, "visualisation/hotspotMaps/data/covariateDataList.RDS")
