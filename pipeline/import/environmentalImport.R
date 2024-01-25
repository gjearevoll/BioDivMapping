

#### ENVIRONMENTAL DATA IMPORT ####

# The following script imports our various forms of environmental data and processes them, based on the type of data
# and other specifications related to the source.

# NOTE: Before runnign this script, the speciesImport.R script needs to have been run.

library(raster)
library(terra)
library(sf)
library(fasterize)
library(digest)  # create hash of raster CRS and projection for saving

# Import local functions
sapply(list.files("functions", full.names = TRUE), source)

###-----------------###
### 1. Preparation ####
###-----------------###

# if it is not already, define dateAccessed
if (!exists("dateAccessed")) {
  dateAccessed <- as.character(Sys.Date())
}
# define repo folder names
folderName <- paste0("data/run_", dateAccessed)
tempFolderName <- paste0(folderName, "/temp")

# import project control parameters into the environment
readRDS(paste0(folderName,"/controlPars.RDS")) %>% 
  list2env(envir = .GlobalEnv)

# Import focal covariates
if(file.exists(paste0(folderName, "/focalCovariates.csv"))){
  parameters <- read.csv( paste0(folderName, "/focalCovariates.csv"), header = T)
} else {
  stop("Please source initialiseRepository.R first.")
}

# import regionGeometry list
if(file.exists(paste0(folderName, "/regionGeometry.RDS"))){
  regionGeometry <- readRDS(paste0(folderName, "/regionGeometry.RDS"))
} else {
  stop("Please source defineRegionGeometry.R first.")
}

###--------------------###
### 2. Dataset Import ####
###--------------------###

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

# convert crs to format accepted by sf, terra, and intSDM (& dependencies) 
projCRS <- sf::st_crs(crs)$proj4string

# define region to download as bounding box of buffered and projected mesh/regionGeometry
regionGeometryBuffer <- st_union(if(exists("mesh")) mesh else regionGeometry) |>
  st_buffer(20000) |>
  st_transform(projCRS) |> 
  st_bbox() |> 
  st_as_sfc() |>
  st_segmentize(dfMaxLength = 10000) |> 
  vect() 

# define working project raster 
baseRaster <- terra::rast(extent = ext(regionGeometryBuffer), res = res, crs = projCRS)

# download environmental data
parameterList <- list()

for(parameter in seq_along(selectedParameters)) {
  rasterisedVersion <- NULL
  focalParameter <- selectedParameters[parameter]
  
  ### 1. Check if the data needs to be downloaded externally.
  external <- parameters$external[parameters$parameters == focalParameter]
  
  if(external) {
    dataSource <- parameters$dataSource[parameters$parameters == focalParameter]
    
    ### 2. Check whether we have previously downloaded a version of the external data that encompasses the area we need.
    dataPath <- paste0("data/temp/", dataSource)
    if(dir.exists(dataPath)){
      rasterisedVersion <- checkAndImportRast(focalParameter, regionGeometryBuffer, dataPath)
      # 3. Create new temp folder to download necessary external data.
    } else {
      dir.create(dataPath)
    }
    if(is.null(rasterisedVersion)) {
      # download file
      source(paste0("pipeline/import/utils/defineEnvSource.R"))
    }
  } else {
    rasterisedVersion <- rast(paste0("data/external/environmentalCovariates/",focalParameter, ".tiff"))
  }
  parameterList[[parameter]] <- rasterisedVersion
}

# crop, reproject, and combine raster layers
parametersCropped <- parameterList |> 
  lapply(function(x) {
    # Crop each covariate to extent of regionGeometryBuffer
    out <- terra::crop(x, as.polygons(terra::project(regionGeometryBuffer, x), extent = TRUE), snap = "out", mask = TRUE)
    # Project all rasters to baseRaster and combine
    if(is.factor(x)) {
      # project categorical rasters
      out <- terra::project(out, baseRaster, method = "mode")
      levels(out) <- levels(x)  # reassign levels 
      out
    } else {
      # project & scale continuous rasters
      terra::project(out, baseRaster) |>
        scale()  
    }}) |>  
  rast() |>  # combine raster layers
  setNames(selectedParameters)  # assign names

###--------------------###
### 3. Dataset Upload ####
###--------------------###

# save projCRS
saveRDS(projCRS, paste0(tempFolderName,"/projCRS.RDS"))

# Save both to temp file for model processing and visualisation folder for mapping
writeRaster(parametersCropped, paste0(tempFolderName,"/environmentalDataImported.tiff"), overwrite=TRUE)

# Create aggregated version for all non-land cover visualisation and reference data
agg <- function(x, fact){
  if(is.factor(x))
    terra::aggregate(x, fact, fun = "modal") else
      terra::aggregate(x, fact)
}
parametersAggregated <- sapp(x = parametersCropped, fun = agg, fact = 2) |>
  crop(baseRaster)

writeRaster(parametersAggregated, paste0(folderName,"/environmentalDataImported.tiff"), overwrite=TRUE)



