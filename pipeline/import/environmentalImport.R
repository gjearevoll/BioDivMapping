
#### ENVIRONMENTAL DATA IMPORT ####

# The following script imports our various forms of environmental data and processes them, based on the type of data
# and other specifications related to the source. Descriptions of all external data sources can be found here:
# https://github.com/gjearevoll/BioDivMapping/tree/main/data/temp

# NOTE: Before running this script, the speciesImport.R script needs to have been run.

# library(raster)
library(terra)
library(sf)
library(fasterize)
library(dplyr)
library(digest)  # create hash of raster CRS and projection for saving
library(ncdf4)

# Import local functions
sapply(list.files("functions", full.names = TRUE), source)

###----------------------###
### 0. Bash preparation ####
###----------------------###

args <- commandArgs(TRUE)

# THis should only run if the script is being run from the command line
if (length(args) != 0 & !exists("dateAccessed")) {
  # Set arguments
  dateAccessed <- args[1]
  # Set the working directory
  setwd("~/BioDivMapping")
}

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
  parameters <- read.csv(paste0(folderName, "/focalCovariates.csv"), header = T)
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

# import baseRaster
baseRaster <- rast(file.path(folderName, "baseRaster.tiff"))

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

# download environmental data
parameterList <- list()
covariate_meta <- list()

for(parameter in seq_along(selectedParameters)) {
  rasterisedVersion <- NULL
  focalParameter <- selectedParameters[parameter]
  temporalFactor <- if (temporal) parameters$temporal[parameters$parameters == focalParameter] else FALSE
  
  
  ### 1. Check if the data needs to be downloaded externally.
  external <- parameters$external[parameters$parameters == focalParameter]
  
  if(external) {
    dataSource <- parameters$dataSource[parameters$parameters == focalParameter]
    # Should this covariate be re-downloaded 
    # regulated with 'update' column in focalCovariates.csv.
    update <- isTRUE(as.logical(parameters$update[parameters$parameters == focalParameter]))

    ### 2. Check whether we have previously downloaded a version of the external data that encompasses the area we need.
    dataPath <- file.path(downloadCovFolder, dataSource)
    if(dir.exists(dataPath)){
      if (update) {
        # Force re-download (existing copy will be overwritten
        message(sprintf("'update = TRUE' for '%s'; forcing re-download from '%s'.",
                        focalParameter, dataSource))
        rasterisedVersion <- NULL
      } else {
        rasterisedVersion <- checkAndImportRast(focalParameter, baseRaster, dataPath,
                                                temporalFactor, yearInterval)
      }
      # 3. Create new temp folder to download necessary external data.
    } else {
      dir.create(dataPath)
    }
    if(is.null(rasterisedVersion)) {
      # download file if still missing (or if update == TRUE)
      source(paste0("pipeline/import/utils/defineEnvSource.R"))
    }
  } else {
    dataSource        <- "local"
    dataPath          <- localCovFolder
    rasterisedVersion <- rast(file.path(localCovFolder, paste0(focalParameter, ".tiff")))
  }
  
  ### 4. Build covariate metadata from in-memory raster.
  param_row <- parameters[parameters$parameters == focalParameter, ]
  
  r_ext <- ext(rasterisedVersion)
  r_res <- res(rasterisedVersion)
  
  # temporal metadata
  has_time  <- nlyr(rasterisedVersion) > 1 || (
    !is.null(time(rasterisedVersion)) & 
      isTRUE(!is.na(time(rasterisedVersion))))
  time_info <- if (has_time) {
    t_dates <- as.character(time(rasterisedVersion))
    list(
      temporal         = TRUE,
      temporal_extent  = c(t_dates[1], t_dates[length(t_dates)]),
      temporal_n_steps = nlyr(rasterisedVersion)
    )
  } else {
    list(temporal = FALSE)
  }
  
  # data type
  r_type <- c("integer", "numeric", "factor")[
    which(c(terra::is.int(rasterisedVersion),
            terra::is.num(rasterisedVersion),
            terra::is.factor(rasterisedVersion)))[1]]
  # combine metadata
  covariate_meta[[focalParameter]] <- c(
    list(
      covariate  = focalParameter,
      longname   = gsub("_", " ", focalParameter),
      source     = dataSource,
      citation   = if ("citation" %in% names(param_row)) param_row$citation else NA,
      file       = sources(rasterisedVersion),
      type       = "raster",
      extent     = list(xmin = r_ext$xmin, xmax = r_ext$xmax,
                        ymin = r_ext$ymin, ymax = r_ext$ymax),
      resolution = list(x = r_res[1], y = r_res[2]),
      crs        = crs(rasterisedVersion, proj = TRUE),
      crs_wkt    = crs(rasterisedVersion, proj = FALSE),
      units      = units(rasterisedVersion),
      typeof     = r_type
    ),
    time_info
  )
  
  rm("rasterisedVersion")
  gc()
}

###--------------------###
### 5. Update JSON    ####
###--------------------###

json_ls <- jsonlite::fromJSON(file.path(extFolderName, "metadata.json"))

json_ls$step_2a <- covariate_meta

jsonlite::write_json(
  json_ls,
  file.path(extFolderName, "metadata.json"),
  pretty      = TRUE,
  auto_unbox  = TRUE
)