

#### ENVIRONMENTAL DATA IMPORT ####

# The following script imports our various forms of environmental data and processes them, based on the type of data
# and other specifications related to the source.

# NOTE: Before runnign this script, the speciesImport.R script needs to have been run.

library(raster)
library(terra)
library(sf)
library(fasterize)

# Import local functions
sapply(list.files("functions", full.names = TRUE), source)

###-----------------###
### 1. Preparation ####
###-----------------###

# Run script to define geographical region and resolution we are working with 
# Initialise folders for storage of all run data
if (!exists("dateAccessed")) {
  dateAccessed <- as.character(Sys.Date())
}

speciesDataList <- readRDS(paste0("data/run_", dateAccessed, "/temp/speciesDataImported.RDS"))
regionGeometry <- readRDS(paste0("data/run_", dateAccessed, "/regionGeometry.RDS"))

# The following is a list of the various environmental variables we have available.
parameters <- read.csv("data/external/focalCovariates.csv")
selectedParameters <- parameters$parameters[parameters$selected]

# Check that any parameters we're downloading externally have a source
emptyParameters <- parameters$parameters[parameters$external & parameters$dataSource == ""]
if (length(emptyParameters) > 0) {
  stop(sprintf("You have indicated an external import for %s but have not indicated %s.",
               {
                 vec <- paste0("'", emptyParameters, "'")
                 if (length(vec) == 1) { as.character(vec)
                   } else if (length(vec) == 2) { paste(vec[1], "and", vec[2])
                     } else { paste0(paste(vec[-length(vec)], collapse = ", "), ", and ", vec[length(vec)])
                       }
                 },
               if (length(vec) == 1) "a source" else "sources"))}

# Define folders for storage of all run data
tempFolderName <- paste0("data/run_", dateAccessed, "/temp/")

# Correct crs
newproj <- "+proj=longlat +ellps=WGS84 +no_defs"

###--------------------###
### 2. Dataset Import ####
###--------------------###

# define region with buffer 
if(exists("mesh")){
  regionGeometry_buffer <- vect(st_buffer(st_union(mesh), 20000))
} else {
  regionGeometry_buffer <- vect(st_buffer(regionGeometry, 20000))
}

parameterList <- list()

for (parameter in 1:length(selectedParameters)) {
  focalParameter <- selectedParameters[parameter]
  external <- parameters$external[parameters$parameters == focalParameter]
  
  if (external == FALSE) {
    rasterisedVersion <- rast(paste0("data/external/environmentalCovariates/",focalParameter, ".tiff"))
  } else {
    dataSource <- parameters$dataSource[parameters$parameters == focalParameter]
    source(paste0("pipeline/import/utils/defineEnvSource.R"))
  }
  parameterList[[parameter]] <- rasterisedVersion
}


# Import and correctly project all covariate data selected in the csv file
parametersCropped <- lapply(parameterList, FUN = function(x) {
  scale(
    crop(x,
         project(regionGeometry_buffer, x), 
         snap = "out", mask = T)
  )
})
# project all rasters to the one with the highest resolution and combine 
reference <- which.min(sapply(parametersCropped, res)[1,])
parametersCropped <- do.call(c, 
                             lapply(parametersCropped, function(x){
                               project(x, parametersCropped[[reference]])
                             }))
# assign names
names(parametersCropped) <- selectedParameters

###--------------------###
### 3. Dataset Upload ####
###--------------------###

# Save both to temp file for model processing and visualisation folder for mapping
writeRaster(parametersCropped, paste0("data/run_", dateAccessed,"/environmentalDataImported.tiff"), overwrite=TRUE)
writeRaster(parametersCropped, "visualisation/hotspotMaps/data/covariateDataList.tiff", overwrite=TRUE)
