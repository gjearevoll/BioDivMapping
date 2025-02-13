

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
  dateStored <- args[2]
  aggregated <- args[3]
  # Set the working directory
  setwd("~/BioDivMapping")
}

# Ensure that modelRun and dateAccessed are specified
if (!exists("dateAccessed")) stop("You need to specify the variable dateAccessed")
if (!exists("dateStored")) stop("You need to specify the variable dateStored")

# Specify folders for storage of all run data
folderName <- paste0("data/run_", dateAccessed)
destFolderName <- paste0("data/run_", dateStored, "/samplingDensities")
tempFolderName <- paste0(folderName, "/temp")

if (!dir.exists(destFolderName)) {
  dir.create(destFolderName)
}

# Import threatened species and ansvarsArterLists
threatenedSpeciesList <- readRDS(paste0(folderName, "/redList.RDS"))
ansvarsArterList <- readRDS("data/external/ansvarsArterList.RDS")


# import project control parameters into the environment
readRDS(paste0(folderName,"/controlPars.RDS")) %>% 
  list2env(envir = .GlobalEnv)

# Get list of all relevant taxa
regionGeometry <- readRDS(paste0(folderName, "/regionGeometry.RDS"))
projCRS <- "+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=km +no_defs"
environmentalDataList <- terra::project(rast(paste0(folderName, "/environmentalDataImported.tiff")), crs(projCRS))
focalTaxa <- read.csv(paste0(folderName, "/focalTaxa.csv"))
processedData <- readRDS(paste0(folderName, "/processedPresenceData.RDS"))

predRast <- rast(ext(environmentalDataList), res = c(0.5, 0.5), crs = crs(projCRS))

# Also need to mask out areas outside of Norway
regionToMap <- terra::project(vect(regionGeometry), predRast)
cityOwin <- as.owin(sf::st_as_sf(regionToMap))

# Import country border
norwayBorder2 <- sf::read_sf("data/external/norge_border/Noreg_polygon.shp")
norwayBorderProjected2 <- terra::project(vect(norwayBorder2), crs(projCRS))

# Define taxa list (depends on birds)
if (focalTaxa$taxa == "birds" & aggregated != TRUE) {
  taxaToRun <- c("groundNestingBirds", "waders", "woodpeckers")
} else {
  taxaToRun <- focalTaxa$taxa
}

###-------------------------###
### 2. Run individual taxa ####
###-------------------------###

# FOR GROUPED VERSION #
for (t in taxaToRun) {
  
  if (t %in% c("woodpeckers")){
    birdsToImport <- unique(processedData$acceptedScientificName)
    woodpeckers <- getGbifBackbone(birdsToImport) %>% filter(family =="Picidae")
    processedData$taxa <- ifelse(processedData$acceptedScientificName %in% woodpeckers$scientificName, 
                                      "woodpeckers", "birds")
  } else if (t %in% c("groundNestingBirds", "waders")) {
    birdChart <- read.csv("data/external/birdTypeList.csv", sep = ",") %>%
      filter(group == t)
    processedData$taxa <- ifelse(processedData$simpleScientificName %in% gsub(" ","_",birdChart$simpleName), 
                                      t, "birds")
    }
      
  
  cat("Processing", t, "data for sampling density modelling.\n")
  dataCombined <- processedData %>%
    filter(taxa %in% t) %>%
    dplyr::select("dataType", "geometry", "simpleScientificName")
  
  for (type in c("allspecies", "threatenedspecies", "ansvarsarter")) {
    
    if (type == "threatenedspecies") {
      dataCombined2 <- dataCombined %>%
        filter(simpleScientificName %in% gsub(" ","_",threatenedSpeciesList$species)) %>%
        dplyr::select("dataType", "geometry")
    } else if (type == "ansvarsarter") {
      dataCombined2 <- dataCombined %>%
        filter(simpleScientificName %in% ansvarsArterList$simpleScientificName) %>%
        dplyr::select("dataType", "geometry")
    } else {
      dataCombined2 <- dataCombined %>%
        dplyr::select("dataType", "geometry")
    }
    
    
    cat("Converting", t, "data to density plot.\n")
    vectorised <- terra::project(vect(dataCombined2), crs(projCRS))
    points <- terra::crds(vectorised)
    p <- ppp(points[,1], points[,2], window = cityOwin)
    KF <- 0.04
    
    # Need to set dimensions to get 500 by 500m picture
    dimensions <- c(round((ext(regionToMap)[4] - ext(regionToMap)[3])/.200), round((ext(regionToMap)[2] - ext(regionToMap)[1])/.200))
    
    cat("Rasterising", t, "density plot.\n")
    ds <- density(p, adjust = KF, dimyx = dimensions)
    loggedDensity <- log(ds+0.0000001)
    rastDensity <- rast(loggedDensity)
    crs(rastDensity) <- crs(predRast)
    scaledLoggedDensity <- (loggedDensity - min(loggedDensity))/(max(loggedDensity) - min(loggedDensity))
    
    # Now project this onto our pred rast
    cat("Reprojecting and saving ", t, " density plot.\n")
    finalDensity <- terra::project(rastDensity, predRast)
    biasRasterMasked <- crop(finalDensity, norwayBorderProjected2, mask = T)
    biasRasterMasked$skalertInnsamlingsIntensitet <- (biasRasterMasked$lyr.1 - minmax(biasRasterMasked$lyr.1)[1])/
      (minmax(biasRasterMasked$lyr.1)[2] - minmax(biasRasterMasked$lyr.1)[1])
    
    writeRaster(biasRasterMasked$skalertInnsamlingsIntensitet, file = paste0(destFolderName, "/density_", type ,"_",t, ".tiff"), overwrite = TRUE)
    
  }
  rm("ds", "p", "vectorised", "dataCombined")
  gc()
}

###-------------------------###
### 2. Run aggregated taxa ####
###-------------------------###

if (aggregated == TRUE) {
  
  aggName <- "insects"
  
  if (nrow(focalTaxa) > 1) {
    cat("Finished producing individual taxa density plots. Creating aggregated density plot.\n")
    
    for (type in c("allspecies", "threatenedspecies", "ansvarsarter")) {
      
      if (type == "threatenedspecies") {
        dataCombined2 <- processedData %>%
          filter(simpleScientificName %in% gsub(" ","_",threatenedSpeciesList$species)) %>%
          dplyr::select("dataType", "geometry")
      } else if (type == "ansvarsarter") {
        dataCombined2 <- processedData %>%
          filter(simpleScientificName %in% ansvarsArterList$simpleScientificName) %>%
          dplyr::select("dataType", "geometry") 
      } else {
        dataCombined2 <- processedData %>%
          dplyr::select("dataType", "geometry") 
      }
      
      
      cat("Converting aggregated", aggName,"data to density plot.\n")
      vectorised <- terra::project(vect(dataCombined2), crs(projCRS))
      points <- terra::crds(vectorised)
      p <- ppp(points[,1], points[,2], window = cityOwin)
      KF <- 0.04
      
      # Need to set dimensions to get 500 by 500m picture
      dimensions <- c(round((ext(regionToMap)[4] - ext(regionToMap)[3])/.2), round((ext(regionToMap)[2] - ext(regionToMap)[1])/.2))
      
      cat("Rasterising", aggName,"density plot.\n")
      ds <- density(p, adjust = KF, dimyx = dimensions)
      loggedDensity <- log(ds+0.0000001)
      rastDensity <- rast(loggedDensity)
      crs(rastDensity) <- crs(predRast)
      scaledLoggedDensity <- (loggedDensity - min(loggedDensity))/(max(loggedDensity) - min(loggedDensity))
      
      # Now project this onto our pred rast
      cat("Reprojecting and saving", aggName,"density plot.\n")
      finalDensity <- terra::project(rastDensity, predRast)
      biasRasterMasked <- crop(finalDensity, norwayBorderProjected2, mask = T)
      biasRasterMasked$skalertInnsamlingsIntensitet <- (biasRasterMasked$lyr.1 - minmax(biasRasterMasked$lyr.1)[1])/
        (minmax(biasRasterMasked$lyr.1)[2] - minmax(biasRasterMasked$lyr.1)[1])
      
      writeRaster(biasRasterMasked$skalertInnsamlingsIntensitet, file = paste0(destFolderName, "/aggregated_density_",type, "_", aggName ,".tiff"), overwrite = TRUE)
      
    }
  }
}
