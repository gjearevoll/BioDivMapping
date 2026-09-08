

###----------------------###
### 0. Bash preparation ####
###----------------------###
library(intSDM)
library(terra)
library(dplyr)
# Conditional library loading based on operating system
if (Sys.info()["sysname"] == "Linux") {
  # Load qs from cluster project directory on Linux server
  library(qs, lib.loc = "/cluster/projects/nn11017k/BioDivMapping/R")
  
  # Specify script parameters
  args <- commandArgs(trailingOnly = TRUE)
  dateAccessed <- as.character(args[1])
  # get segment number
  nSegment <- as.numeric(args[2])
  
} else if (Sys.info()["sysname"] == "Windows") {
  # Load qs from default library on Windows
  library(qs)
  
  nSegment <- 10
}


start <- Sys.time()
cat(dateAccessed)

# Define the folder to find our results (use most recent one created)
if (!exists("dateAccessed")) {
  dateAccessed <- stringr::str_remove(tail(list.files("data", "run_"), 1), "^run_")
  warning("'dateAccessed' was not defined, using most recent run in 'data' folder.")
}

# define repo folder names
folderName <- paste0("data/run_", dateAccessed)
tempFolderName <- paste0(folderName, "/temp")

# load the control parameters
readRDS(paste0(folderName,"/controlPars.RDS")) %>%
  list2env(envir = .GlobalEnv)

prior.range[1] <- prior.range[1] /1000

###-----------------###
### 1. Preparation ####
###-----------------###
cat("\nPreparing data for model run for date", dateAccessed)

# Import local functions
sapply(list.files("functions", full.names = TRUE), source)

# model output folder
modelFolderName <- paste0(folderName, "/modelOutputs")

# create folder for workspaces
if (!dir.exists(paste0(folderName, "/workspaces"))) {
  dir.create(paste0(folderName, "/workspaces"))
}

# Use 10000m grid for practice predictions
res <- 10

# Import species list
focalTaxa <- read.csv(paste0(folderName, "/focalTaxa.csv"), header = T)

# Import datasets
regionGeometry <- readRDS(paste0(folderName, "/regionGeometry.RDS"))
focalCovariates <- read.csv(paste0(folderName, "/focalCovariates.csv"), header= T)
environmentalDataList <- rast(paste0(tempFolderName, "/environmentalDataImported.tiff"))
speciesData <- qread(paste0(folderName, "/speciesDataProcessed.qs"))

crs <- '+proj=utm +zone=33 +datum=WGS84 +units=km +no_defs'
environmentalDataList <- project(environmentalDataList, crs)
speciesData <- lapply(speciesData, FUN = function(x) {
  st_transform(x, crs)
})
regionGeometry <- st_transform(regionGeometry, crs)


#changing names with norwegian texts
speciesDatanameToChange <- names(speciesData)[which(grepl("[^\x01-\x7F]+", names(speciesData)))]
print(paste("Changing name of this dataset:", speciesDatanameToChange))
speciesDatanameChanged <- gsub('[^\x01-\x7F]+', ' ', speciesDatanameToChange)
names(speciesData)[names(speciesData) %in% speciesDatanameToChange] <- speciesDatanameChanged

cat("\nAll data loaded.", length(speciesData), "species datasets successfully loaded.")


# Import bird data from TOV and remove all no-relevant birds from other datasets
if ("birds" %in% focalTaxa$taxa) {
  TOVData <- st_transform(readRDS("data/temp/birdDataTOV.RDS"), st_crs(speciesData[[1]])) |>
    st_crop(st_transform(regionGeometry, st_crs(speciesData[[1]])))
  # filter only birds imported from other datasets
  if (!"Aves" %in% focalTaxa$scientificName) {
    speciesData[["TOVData"]] <- TOVData[TOVData$simpleScientificName %in%
                                          unique(bind_rows(speciesData)$simpleScientificName),]
  } else {
    speciesData[["TOVData"]] <- TOVData
  }
  
  focalTaxa$predictionDataset[focalTaxa$taxa %in% c("birds", "groundNestingBirds", "woodpeckers")] <- "TOVData"
  speciesData <- lapply(speciesData, FUN = function(x) {
    x <- x[!(x$taxa %in% c("birds", "woodpeckers", "groundNestingBirds") &
               !(x$acceptedScientificName %in% unique(speciesData$TOVData$acceptedScientificName))),]
  })
  
  cat("Birds data filtered on TOV species.")
}

# Save the prepared species data for the fitting script and for post-hoc analysis.
qsave(speciesData, paste0(folderName, "/speciesDataProcessedPrepped.qs"))

# save prepped focalTaxa since bird block above overwrites predictionDataset with "TOVData"
write.csv(focalTaxa, paste0(folderName, "/focalTaxa_prepped.csv"), row.names = FALSE)
# Define speciesData based on run type and create predictionData
predictionData <- createPredictionData(c(res, res), regionGeometry, proj = crs)

cat("\nPrediction data and model species data successfully created. Starting to create segments of", nSegment, "species each.")

# Create list of taxa run
listSegments <- list()

# Prepare models 
# One iteration per taxonomic group, not per row of focalTaxa. 
for(focalTaxon in unique(focalTaxa$taxa)){
  focalTaxaGroup <- focalTaxa[focalTaxa$taxa == focalTaxon, ]
  predictorSpecies <- unique(focalTaxaGroup$predictorSpecies)
  workflowList <- modelPreparation(focalTaxaGroup, focalCovariates, speciesData,
                                   regionGeometry = regionGeometry,
                                   modelFolderName = modelFolderName,
                                   environmentalDataList = environmentalDataList,
                                   crs = crs,
                                   segmentation = TRUE,
                                   nSegment = nSegment,
                                   speciesOccurrenceThreshold = speciesOccurrenceThreshold,
                                   datasetOccurrenceThreshold = datasetOccurrenceThreshold,
                                   mergeAllDatasets = TRUE,
                                   richness = TRUE, predictorSpecies = predictorSpecies)
  focalTaxaRun <- names(workflowList)
  
  
  cat("Finished creating workflows.")
  
  
  # Get bias fields
  if (file.exists(paste0(folderName, "/metadataSummary.csv"))) {
    dataTypes <- read.csv(paste0(folderName, "/metadataSummary.csv"))
    biasFieldList <- defineBiasFields(focalTaxaRun, dataTypes[!is.na(dataTypes$processing),], speciesData, NULL)
  } else {
    biasFieldList <- rep(list(NULL), length(focalTaxaRun))
  }
  
  
  modelOutputs <- "Richness"
  
  listSegments[[focalTaxon]] <- focalTaxaRun
  if (grepl("vascularPlants", focalTaxon)) {saveRDS(focalTaxaRun, paste0(folderName, "/segmentList", focalTaxon ,".RDS"))}
  # Save the workflows 
  qsave(workflowList, paste0(folderName, "/workspaces/", focalTaxon, "_workflowList.qs"))
}


# Combination of response and environmental variables
