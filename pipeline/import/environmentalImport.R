

#### ENVIRONMENTAL DATA IMPORT ####

# The following script imports our various forms of environmental data and processes them, based on the type of data
# and other specifications related to the source. Descriptions of all external data sources can be found here:
# https://github.com/gjearevoll/BioDivMapping/tree/main/data/temp

# NOTE: Before running this script, the speciesImport.R script needs to have been run.

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
  stop("Please define a run date for the model first.")
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

# Import focal taxa
if(file.exists(paste0(folderName, "/focalTaxa.csv"))){
  focalTaxa <- read.csv( paste0(folderName, "/focalTaxa.csv"), header = T)
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

# Reduce focalTaxa to focalCovariates
selectedParameters <- colnames(focalTaxa)[colnames(focalTaxa) %in% parameters$parameters]
focalTaxa <- focalTaxa[,c("taxa", selectedParameters)]
selectedParameters <- selectedParameters[apply(focalTaxa[,-1], 2, any)]

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
# regionGeometryBuffer <- st_union(if(exists("myMesh")) {
#   meshTest(myMesh, regionGeometry, print = F, crs = crs) |>
#     inlaMeshToSf()
# }
#   else regionGeometry) |>
#   st_buffer(2000) |>
#   st_transform(projCRS) |> 
#   st_bbox() |> 
#   st_as_sfc() |>
#   st_segmentize(dfMaxLength = 10000) |> 
#   vect() 

regionGeometryBuffer <- st_union(regionGeometry) |>
  st_buffer(2000) |>
  st_transform(projCRS) |> 
  st_bbox() |> 
  st_as_sfc() |>
  st_segmentize(dfMaxLength = 10000) |> 
  vect() 


# define working project raster 
baseRaster <- terra::rast(extent = ext(regionGeometryBuffer), res = res, crs = projCRS)

# rasterise regionGeometry
regionGeometryRast <- regionGeometry |>
  st_as_sf() |>
  st_transform(projCRS) |> 
  vect() |>
  terra::rasterize(baseRaster, FUN = "mode") 

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
    dataPath <- file.path(downloadCovFolder, dataSource)
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
    rasterisedVersion <- rast(file.path(localCovFolder, paste0(focalParameter, ".tiff")))
  }
  parameterList[[parameter]] <- rasterisedVersion
  rm("rasterisedVersion")
  gc()
}


###------------------------###
### 3. Data Consolidation ####
###------------------------###
# Crop, match projections and compile raster layers into one object
parametersCropped <- parameterList |> 
  lapply(function(x) {
    # Crop each covariate to extent of regionGeometryBuffer
    out <- x
    # Project all rasters to baseRaster and combine
    if(is.factor(x)) {
      # project categorical rasters
      out <- terra::project(out, baseRaster, method = "mode")
      levels(out) <- levels(x)  # reassign levels 
      out
    } else {
      # project & scale continuous rasters
      ifel(is.na(regionGeometryRast), NA,
           terra::project(out, baseRaster)) |>
        scale()  
    }}) |>  
  rast() |>  # combine raster layers
  setNames(selectedParameters)  # assign names


###----------------------------###
### 3. Create quadratic terms ####
###----------------------------###

# Check which parameters are needed to make sure we don't take the quadratic of an unwanted term
useParam <- apply(focalTaxa[, colnames(focalTaxa) %in% parameters$parameters], 2, any)
parametersForUse <- names(useParam)[useParam]

quadratics <- parameters[parameters$quadratic & parameters$parameters %in% parametersForUse,]
if (nrow(quadratics) > 0) {
  for(i in seq_along(quadratics$quadratic)) {
    parameter <- quadratics$parameters[i]
    parametersCropped[[paste0(parameter, "_squared")]] <- parametersCropped[[parameter]]^2
  }
}



###--------------------###
### 4. Dataset Upload ####
###--------------------###

# save projCRS
saveRDS(projCRS, paste0(tempFolderName,"/projCRS.RDS"))

# Save both to temp file for model processing and visualisation folder for mapping
writeRaster(parametersCropped, paste0(tempFolderName,"/environmentalDataImported.tiff"), overwrite=TRUE)

# Create aggregated version for all non-land cover visualisation and reference data
agg <- function(x, fact){
  if(is.factor(x))
    # If the variable is a factor, use the most common result as the average
    terra::aggregate(x, fact, fun = "modal") else 
      terra::aggregate(x, fact)
}
parametersAggregated <- sapp(x = parametersCropped, fun = agg, fact = 2) |>
  crop(baseRaster)

writeRaster(parametersAggregated, paste0(folderName,"/environmentalDataImported.tiff"), overwrite=TRUE)

