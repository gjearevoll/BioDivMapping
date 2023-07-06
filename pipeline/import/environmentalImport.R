

#### ENVIRONMENTAL DATA IMPORT ####

# The following script imports our various forms of environmental data and processes them, based on the type of data
# and other specifications related to the source.

# NOTE: Before runnign this script, the speciesImport.R script needs to have been run.

library(raster)

###-----------------###
### 1. Preparation ####
###-----------------###

# Run script to define geographical region and resolution we are working with 
# Initialise folders for storage of all run data
if (!exists("dateAccessed")) {
  dateAccessed <- as.character(Sys.Date())
}

speciesDataList <- readRDS(paste0("data/run_", dateAccessed, "/temp/speciesDataImported.RDS"))
regionGeometry <- speciesDataList$geometry

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
  dataRaw <- raster(paste0("data/external/environmentalCovariates/", x ,".tiff"))
  dataProjected <- projectRaster(dataRaw, crs=newproj, res=0.1)
  dataCropped <- crop(dataProjected, as_Spatial(regionGeometry))
  dataMasked <- mask(dataCropped, as_Spatial(regionGeometry))
  names(dataMasked) <- x
  dataFinal <- scale(dataMasked)
})
names(parametersCropped) <- parameters

###--------------------###
### 3. Dataset Upload ####
###--------------------###

# For now we're just doing this to the data/temp folder, later this will go to Wallace
saveRDS(parametersCropped, paste0("data/run_", dateAccessed,"/temp/environmentalDataImported.RDS"))

