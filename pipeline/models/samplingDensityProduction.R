

#### MODEL RUNS ####

###-----------------###
### 1. Preparation ####
###-----------------###

library(terra)
library(sf)
library(spatstat)
library(ggplot2)
library(dplyr)
library(tidyterra)
# Import local functions
sapply(list.files("functions", full.names = TRUE), source)

args <- commandArgs(TRUE)

# THis should only run if the script is being run from the command line
if (length(args) != 0) {
  # Set arguments
  dateAccessed <- args[1]
  # Set the working directory
  setwd("~/BioDivMapping")
}

# Ensure that modelRun and dateAccessed are specified
if (!exists("dateAccessed")) stop("You need to specify the variable dateAccessed")

# Specify folders for storage of all run data
folderName <- paste0("data/run_", dateAccessed)
tempFolderName <- paste0(folderName, "/temp")

if (!dir.exists(paste0(folderName, "/samplingDensities"))) {
  dir.create(paste0(folderName, "/samplingDensities"))
}

# Import threatened species and ansvarsArterLists
threatenedSpeciesList <- readRDS(paste0(folderName, "/redList.RDS"))
ansvarsArterList <- readRDS("data/external/ansvarsArterList.RDS")


# import project control parameters into the environment
readRDS(paste0(folderName,"/controlPars.RDS")) %>% 
  list2env(envir = .GlobalEnv)

# Get list of all relevant taxa
regionGeometry <- readRDS(paste0(folderName, "/regionGeometry.RDS"))
environmentalDataList <- rast(paste0(folderName, "/environmentalDataImported.tiff"))
focalTaxa <- read.csv(paste0(folderName, "/focalTaxa.csv"))
processedData <- readRDS(paste0(folderName, "/speciesDataProcessed.RDS"))

predRast <- rast(ext(environmentalDataList), res = c(res, res), crs = crs(environmentalDataList))

# Also need to mask out areas outside of Norway
regionToMap <- terra::project(vect(regionGeometry), predRast)
cityOwin <- as.owin(sf::st_as_sf(regionToMap))

# Import country border
norwayBorder2 <- sf::read_sf("data/external/norge_border/Noreg_polygon.shp")
norwayBorderProjected2 <- terra::project(vect(norwayBorder2), predRast)

###-------------------------###
### 2. Run individual taxa ####
###-------------------------###

# FOR GROUPED VERSION #
for (t in unique(focalTaxa$taxa)) {
  
  cat("Processing ", t, " data for sampling density modelling.\n")
  processedDataSamples <- lapply(processedData,  FUN  = function(x) {
    if ("individualCount" %in% colnames(x)) {
      x <- x[x$individualCount == 1,]
    }
    
      x %>%
        filter(taxa %in% t) %>%
        dplyr::select("dataType", "geometry")
  })
  dataCombined <- do.call(rbind, processedDataSamples)
  
  
  cat("Converting ", t, " data to density plot.\n")
  vectorised <- terra::project(vect(dataCombined), predRast)
  points <- terra::crds(vectorised)
  p <- ppp(points[,1], points[,2], window = cityOwin)
  KF <- 0.04
  
  # Need to set dimensions to get 500 by 500m picture
  dimensions <- c(round((ext(regionToMap)[4] - ext(regionToMap)[3])/200), round((ext(regionToMap)[2] - ext(regionToMap)[1])/200))
  
  cat("Rasterising ", t, " density plot.\n")
  ds <- density(p, adjust = KF, dimyx = dimensions)
  loggedDensity <- log(ds+0.0000001)
  rastDensity <- rast(loggedDensity)
  crs(rastDensity) <- crs(predRast)
  scaledLoggedDensity <- (loggedDensity - min(loggedDensity))/(max(loggedDensity) - min(loggedDensity))
  
  # Now project this onto our pred rast
  cat("Reprojecting and saving ", t, " density plot.\n")
  finalDensity <- terra::project(rastDensity, predRast)
  biasRasterMasked <- crop(finalDensity, norwayBorderProjected2, mask = T)
  
  writeRaster(biasRasterMasked, file = paste0(folderName, "/samplingDensities/density_", t, ".tiff"), overwrite = TRUE)
  rm("ds", "p", "vectorised", "processedDataSamples", "dataCombined")
  gc()
}

###-------------------------###
### 2. Run aggregated taxa ####
###-------------------------###

if (nrow(focalTaxa) > 1) {
  cat("Finished producing individual taxa density plots. Creating aggregated density plot.\n")
  # FOR GROUPED VERSION #
  for (t in focalTaxa$taxa) {
    
    processedDataSamples <- lapply(processedData,  FUN  = function(x) {
      if ("individualCount" %in% colnames(x)) {
        x <- x[x$individualCount == 1,]
      }
      x %>%
        dplyr::select("dataType", "geometry")
    })
    
    dataCombined <- do.call(rbind, processedDataSamples)
    
    
    cat("Converting aggregated data to density plot.\n")
    vectorised <- terra::project(vect(dataCombined), predRast)
    points <- terra::crds(vectorised)
    p <- ppp(points[,1], points[,2], window = cityOwin)
    KF <- 0.04
    
    # Need to set dimensions to get 500 by 500m picture
    dimensions <- c(round((ext(regionToMap)[4] - ext(regionToMap)[3])/200), round((ext(regionToMap)[2] - ext(regionToMap)[1])/200))
    
    cat("Rasterising ", t, " density plot.\n")
    ds <- density(p, adjust = KF, dimyx = dimensions)
    loggedDensity <- log(ds+0.0000001)
    rastDensity <- rast(loggedDensity)
    crs(rastDensity) <- crs(predRast)
    scaledLoggedDensity <- (loggedDensity - min(loggedDensity))/(max(loggedDensity) - min(loggedDensity))
    
    # Now project this onto our pred rast
    cat("Reprojecting and saving ", t, " density plot.\n")
    finalDensity <- terra::project(rastDensity, predRast)
    biasRasterMasked <- crop(finalDensity, norwayBorderProjected2, mask = T)
    
    writeRaster(biasRasterMasked, file = paste0(folderName, "/samplingDensity/aggregated_density.tiff"), overwrite = TRUE)
    
  }
}

