

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

# Run script to define geographical region and resolution we are working with 
# Initialise folders for storage of all run data
if (!exists("dateAccessed")) {
  dateAccessed <- as.character(Sys.Date())
}

# Add folder name
folderName <- paste0("data/run_", dateAccessed)
tempFolderName <- paste0(folderName, "/temp")

regionGeometry <- readRDS(paste0(folderName, "/regionGeometry.RDS"))

# The following is a list of the various environmental variables we have available.
# Define initial species list.
if(file.exists(paste0(folderName, "/focalCovariates.csv"))){
  parameters <- read.csv( paste0(folderName, "/focalCovariates.csv"), header = T)
} else {
  parameters <- read.csv("data/external/focalCovariates.csv")  # save for reference
  write.csv(parameters, paste0(folderName, "/focalCovariates.csv"), row.names = FALSE)
}

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


###--------------------###
### 2. Dataset Import ####
###--------------------###

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

for (parameter in seq_along(selectedParameters)) {
  focalParameter <- selectedParameters[parameter]
  
  ### 1. Check if the data needs to be downloaded externally.
  external <- parameters$external[parameters$parameters == focalParameter]
  
  if (external) {
    dataSource <- parameters$dataSource[parameters$parameters == focalParameter]
    
    ### 2. Check whether we have previously downloaded a version of the external data that encompasses the area we need.
    raster_found <- FALSE
    if(dir.exists(paste0("data/temp/", dataSource))){
      ## List all files in the directory that match the parameter
      file_list <- list.files(path = paste0("data/temp/", dataSource), 
                              pattern = paste0(focalParameter, "_.*\\.tiff$"), 
                              full.names = TRUE)
      # Check files
      for (file in file_list) {
        rast <- rast(file)
        if (isSubset(regionGeometryBuffer, rast)) {
          rasterisedVersion <- rast
          raster_found <- TRUE
          cat(sprintf("Raster for '%s' encompassing region found and imported successfully!\n",
                      focalParameter))
          break
        } 
      }   
      # 3. Create new temp folder to download necessary external data.
    } else {
      dir.create(paste0("data/temp/", dataSource))
    }
    if (!raster_found) {
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
    out <- crop(x, as.polygons(project(regionGeometryBuffer, x), extent = TRUE), snap = "out", mask = TRUE)
    # Project all rasters to baseRaster and combine
    if(is.factor(x)) {
      # project categorical rasters
      out <- project(out, baseRaster, method = "mode")
      levels(out) <- levels(x)  # reassign levels 
      out
    } else {
      # project & scale continuous rasters
      project(out, baseRaster) |>
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

writeRaster(parametersAggregated, "visualisation/hotspotMaps/data/covariateDataList.tiff", overwrite=TRUE)
writeRaster(parametersAggregated, paste0("data/run_", dateAccessed,"/environmentalDataImported.tiff"), overwrite=TRUE)



