#### ENVIRONMENTAL DATA PROCESSINg ####


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

# imprt rasters
# reclassify categorical rasters ------------------------------------------ ####
parameterList[[parameter]] <- rasterisedVersion
names(parameterList) <- selectedParameters

if(focalParameter == "land_cover_corine") {
  browser()
  # If we want, we can now reclassifiy CORINE's layers. We do this using a csv file name "corineReclassification" that should be
  # uploaded to the same data/temp/corine folder you are storing corine rasters. The file should have two columns, one names 
  # corineCategory (with existing corine categories) and the second (with your reclassifications) called newCategory.
  if(!file.exists("data/temp/corine/corineReclassification.csv")) {
    warning("No CORINE reclassification system has been provided. Using all 52 potential categories.")
  } else if (reclassify == TRUE) {
    corineReclassification <- read.csv("data/temp/corine/corineReclassification.csv", header = TRUE)
    reclassTable <- levels(corine)[[1]]
    names(reclassTable) <- c("value", "label")
    reclassTable$newLabel <- corineReclassification$newCategory[match(reclassTable$label, corineReclassification$corineCategory)]
    reclassTable <- reclassTable[,c("value", "newLabel")] %>% rename(label = newLabel)
    for (co in 1:nlyr(corine)) {
      levels(corine)[[co]][,2] <- reclassTable[,2]
    }
    if (temporal) {names(corine) <- yearInterval}
  }
  
  
  browser()
  corineReclassification <- read.csv("data/temp/corine/corineReclassification.csv", header = TRUE)
  reclassTable <- levels(rasterisedVersion)[[1]]
  names(reclassTable) <- c("value", "label")
  reclassTable$newLabel <- corineReclassification$newCategory[match(reclassTable$label, corineReclassification$corineCategory)]
  reclassTable <- reclassTable[,c("value", "newLabel")] %>% rename(label = newLabel)
  for (co in 1:nlyr(rasterisedVersion)) {
    levels(rasterisedVersion)[[co]][,2] <- reclassTable[,2]
  }
}
###--------------------------------------###
### 3. Expansion of categorical rasters ####
###--------------------------------------###

contList <- list()
catParams <- parameters$parameters[parameters$categorical]
for (par in catParams) {
  focalCatParameter <- parameterList[[par]]
  levelTable <- levels(focalCatParameter)[[1]]
  allCats <- unique(levelTable[,2])
  if (par == "land_cover_corine") {
    allCats <- allCats[(!allCats %in% c(NA, "Sclerophyllous vegetation"))]
    #allCats <- c("Built up area", "Coniferous forest", "Transitional woodland-shrub", "Moors and heathland")
  }
  catList <- lapply(allCats, FUN = function(cat1) {
    if (par == "kalkinnhold" & cat1 == "no data") {return(NA)}
    catLevels <- levelTable$value[levelTable[,2] %in% cat1]
    print(paste0("Aggregating: ",cat1))
    catRaster <- ifel(focalCatParameter %in% catLevels, 1, 0)
    contRaster <- terra::project(catRaster, baseRaster, method="average")
    contRaster
  }) |> setNames(allCats)
  contList[[par]] <- catList
}

fullCatList <- unlist(contList)[!is.na(unlist(contList))]
names(fullCatList) <- gsub(" ","_",stringr::str_replace_all(names(fullCatList), "[[:punct:]]", "_")) 

parameterListCont <- parameterList[!(names(parameterList) %in% catParams)]
parameterListCont <- c(parameterListCont, fullCatList)
parameterNames <- removeAccents(names(parameterListCont))


###------------------------###
### 3. Data Consolidation ####
###------------------------###
# Crop, match projections and compile raster layers into one object
parametersCropped <- parameterListCont |> 
  lapply(function(x) {
    # Crop each covariate to extent of regionGeometryBuffer
    out <- x
    # Project all rasters to baseRaster and combine
    if(unique(is.factor(x))) {
      # project categorical rasters
      out <- terra::project(out, baseRaster, method = "mode")
      levels(out) <- levels(x)  # reassign levels 
      out
    } else if (nlyr(x) == 1) {
      # project & scale continuous rasters
      ifel(is.na(regionGeometryRast), NA,
           terra::project(out, baseRaster)) |>
        scale()  
    } else {
      projVersion <- ifel(is.na(regionGeometryRast), NA,
                          terra::project(out, baseRaster))
      totalMean <- global( mean(projVersion), "mean", na.rm = TRUE)
      rr <- projVersion - totalMean[,1]
      rms <- global(mean(rr), "rms", na.rm = TRUE)
      rr / rms[,1]
    }
  })

if (!temporal) {
  parametersCropped <- parametersCropped |>  
    rast() |>  # combine raster layers
    setNames(parameterNames)  # assign names
}


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
if (temporal) {
  parametersCropped <- lapply(parametersCropped, terra::wrap)
  saveRDS(parametersCropped, paste0(tempFolderName,"/environmentalDataImported.RDS"))
} else {
  writeRaster(parametersCropped, paste0(tempFolderName,"/environmentalDataImported.tiff"), overwrite=TRUE)
}


# Create aggregated version for all non-land cover visualisation and reference data
agg <- function(x, fact){
  if(is.factor(x))
    # If the variable is a factor, use the most common result as the average
    terra::aggregate(x, fact, fun = "modal") else 
      terra::aggregate(x, fact)
}

# Aggregate and save raster
if (!temporal) {
  parametersAggregated <- sapp(x = parametersCropped, fun = agg, fact = 2) |>
    crop(baseRaster)
  writeRaster(parametersAggregated, paste0(folderName,"/environmentalDataImported.tiff"), overwrite=TRUE)
} else {
  parametersAggregated <- lapply(parametersCropped, FUN = function(x){
    if(unique(is.factor(unwrap(x))))
      # If the variable is a factor, use the most common result as the average
      terra::aggregate(unwrap(x), 2, fun = "modal") |> crop(baseRaster) else 
        terra::aggregate(unwrap(x), 2) |> crop(baseRaster)
  }) 
  parametersAggregated <- lapply(parametersAggregated, terra::wrap)
  saveRDS(parametersAggregated, paste0(folderName,"/environmentalDataImported.RDS"))
}

###--------------------###
### 5. update JSON    ####
###--------------------###

# read existing json
json_ls <- fromJSON(file.path(extFolderName, "metadata.json"))

# define json content
json_ls$step_2b <- list(
  foo = "x"
)

# write json
jsonlite:::write_json(json_ls,
                      file.path(extFolderName, "metadata.json"), 
                      pretty = TRUE)



