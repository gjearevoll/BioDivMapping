

#### ENVIRONMENTAL DATA IMPORT ####

# The following script imports our various forms of environmental data and processes them, based on the type of data
# and other specifications related to the source. Descriptions of all external data sources can be found here:
# https://github.com/gjearevoll/BioDivMapping/tree/main/data/temp

# NOTE: Before running this script, the speciesImport.R script needs to have been run.

library(raster)
library(terra)
library(sf)
library(fasterize)
library(dplyr)
library(digest)  # create hash of raster CRS and projection for saving

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
  temporalFactor <- if (temporal) parameters$temporal[parameters$parameters == focalParameter] else FALSE
 
  
  ### 1. Check if the data needs to be downloaded externally.
  external <- parameters$external[parameters$parameters == focalParameter]
  
  if(external) {
    dataSource <- parameters$dataSource[parameters$parameters == focalParameter]
    
    ### 2. Check whether we have previously downloaded a version of the external data that encompasses the area we need.
    dataPath <- file.path(downloadCovFolder, dataSource)
    if(dir.exists(dataPath)){
      
      if (focalParameter == "cs_density") {
        rasterisedVersion <- NULL
      } else {
        rasterisedVersion <- checkAndImportRast(focalParameter, regionGeometryBuffer, dataPath, 
                                                temporalFactor, yearInterval)
      }
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
  # save as nc-file in dataSet folder
  nc_path <- file.path(dataPath, paste0(focalParameter, ".nc"))
  writeCDF(
    x         = rasterisedVersion,
    filename  = nc_path,
    varname   = focalParameter,
    longname  = gsub("_", " ", focalParameter),
    unit      = units(rasterisedVersion),
    zname     = if (nlyr(rasterisedVersion) > 1) "time" else NULL,
    atts      = list(
      layer_names = paste(names(rasterisedVersion), collapse = ","),
      source_file = sources(rasterisedVersion),
      dataSource = dataSource
    ),
    overwrite = TRUE
  )
  rm("rasterisedVersion")
  gc()
}

###--------------------###
### 5. update JSON    ####
###--------------------###
browser()
# read existing json
json_ls <- fromJSON(file.path(extFolderName, "metadata.json"))

# Build a list entry per covariate from the saved nc files
covariate_meta <- lapply(selectedParameters, function(focalParameter) {
  param_row  <- parameters[parameters$parameters == focalParameter, ]
  dataSource <- param_row$dataSource
  nc_path    <- file.path(downloadCovFolder, dataSource, paste0(focalParameter, ".nc"))
  
  # Open nc to extract spatial/temporal metadata
  nc  <- nc_open(nc_path)
  on.exit(nc_close(nc))
  
  # spatial summaries
  crs_str <- ncatt_get(nc, "crs", "proj4")$value
  # Detect spatial dimension names (handles x/y, easting/northing, lon/lat, etc.)
  dim_names  <- names(nc$dim)
  x_dim_name <- dim_names[grepl("east|^x$|^lon", dim_names, ignore.case = TRUE)]
  y_dim_name <- dim_names[grepl("north|^y$|^lat", dim_names, ignore.case = TRUE)]
  
  x_vals <- ncvar_get(nc, x_dim_name)
  y_vals <- ncvar_get(nc, y_dim_name)
  
  # geotransform: xmin, xres, 0, ymax, 0, -yres  (GDAL convention)
  gt <- as.numeric(strsplit(ncatt_get(nc, "crs", "geotransform")$value, " ")[[1]])
  # gt[1]=xmin, gt[2]=xres, gt[4]=ymax, gt[6]=-yres
  
  extent_info <- list(
    xmin = gt[1],
    xmax = gt[1] + gt[2] * length(x_vals),
    ymin = gt[4] + gt[6] * length(y_vals),   # gt[6] is negative
    ymax = gt[4]
  )
  resolution_info <- list(x = gt[2], y = abs(gt[6]))
  
  # temporal summaries
  has_time  <- "time" %in% names(nc$dim)
  time_info <- if (has_time) {
    t_vals    <- ncvar_get(nc, "time")
    t_units   <- ncatt_get(nc, "time", "units")$value   # e.g. "days since 1970-01-01"
    t_origin  <- as.Date(sub("days since ", "", t_units))
    t_dates   <- as.character(t_origin + t_vals)
    list(
      temporal           = TRUE,
      temporal_extent    = c(t_dates[1], t_dates[length(t_dates)]),
      temporal_n_steps   = length(t_vals),
      temporal_units     = t_units
    )
  } else {
    list(temporal = FALSE)
  }
 
  # combine to dataSet summary list
  c(
    list(
      covariate   = focalParameter,
      longname    = gsub("_", " ", focalParameter),
      source      = dataSource,
      citation    = if (!is.null(param_row$citation)) param_row$citation else NA,
      file        = nc_path,
      type        = "raster",
      extent      = list(
        xmin = min(x_vals), xmax = max(x_vals),
        ymin = min(y_vals), ymax = max(y_vals)
      ),
      resolution  = resolution_info,
      crs         = ncatt_get(nc, "crs", "crs_wkt")$value,
      units       = nc$var[[focalParameter]]$units
    ),
    time_info
  )
})

names(covariate_meta) <- selectedParameters

# define json content
json_ls$step_2a <- covariate_meta

# write json
jsonlite:::write_json(json_ls,
                      file.path(extFolderName, "metadata.json"), 
                      pretty = TRUE)

