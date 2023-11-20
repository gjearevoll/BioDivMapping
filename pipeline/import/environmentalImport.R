

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

speciesDataList <- readRDS(paste0("data/run_", dateAccessed, "/temp/speciesDataImported.RDS"))
regionGeometry <- readRDS(paste0("data/run_", dateAccessed, "/regionGeometry.RDS"))

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

# Define folders for storage of all run data
tempFolderName <- paste0("data/run_", dateAccessed, "/temp/")

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
  external <- parameters$external[parameters$parameters == focalParameter]
  
  if (external) {
    dataSource <- parameters$dataSource[parameters$parameters == focalParameter]
    # check if data has already been downloaded
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
          cat("Raster encompassing region found and imported successfully!\n")
          break
        } 
      }   
    } else {
      dir.create(paste0("data/temp/", dataSource))
    }
    if (!raster_found) {
      # download file
      source(paste0("pipeline/import/utils/defineEnvSource.R"))
      # get info to save
      info <- crs(rasterisedVersion, describe = T)
      if(!is.na(info$authority)){
        ext <- ext(rasterisedVersion)
        info <- sprintf("%s%s_X%s_%s_Y%s_%s",
                        info$authority, info$code, 
                        ext[1], ext[2], ext[3], ext[4])
      } else {
        info <- digest(list(crs(rasterisedVersion), ext(rasterisedVersion)))
      }
      #  Construct the filename and save with raster information
      file_path <- sprintf("data/temp/%s/%s_%s.tiff",
                           dataSource,focalParameter, info)
      x <- rasterisedVersion
      if(!file.exists(file_path)){
        writeRaster(x, filename = file_path, overwrite = TRUE)
      }
    }
  } else {
    rasterisedVersion <- rast(paste0("data/external/environmentalCovariates/",focalParameter, ".tiff"))
  }
  parameterList[[parameter]] <- rasterisedVersion
}

# crop each covariate to extent of regionGeometryBuffer (exclusively for reduced computation)
parametersCropped <- lapply(parameterList, FUN = function(x) {
  scale(
    crop(x,
         as.polygons(terra::project(regionGeometryBuffer, x),  extent = T), 
         snap = "out", mask = T)
  )
})

# project all rasters to the one with the highest resolution and combine 
parametersCropped <- do.call(c, 
                             lapply(parametersCropped, function(x){
                               terra::project(x, baseRaster)
                             }))
# assign names
names(parametersCropped) <- selectedParameters

###--------------------###
### 3. Dataset Upload ####
###--------------------###

# save projCRS
saveRDS(projCRS, paste0(tempFolderName,"/projCRS.RDS"))

# Save both to temp file for model processing and visualisation folder for mapping
writeRaster(parametersCropped, paste0(tempFolderName,"/environmentalDataImported.tiff"), overwrite=TRUE)

# Create aggregated version for visualisation and reference data
parametersAggregated <- terra::aggregate(parametersCropped, fact = 6)
writeRaster(parametersAggregated, "visualisation/hotspotMaps/data/covariateDataList.tiff", overwrite=TRUE)
writeRaster(parametersAggregated, paste0("data/run_", dateAccessed,"/environmentalDataImported.tiff"), overwrite=TRUE)
