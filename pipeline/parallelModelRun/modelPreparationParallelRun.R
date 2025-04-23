

###----------------------###
### 0. Bash preparation ####
###----------------------###
.libPaths(c("/cluster/projects/nn11017k/R"))
library(intSDM)
library(rgbif)
library(terra)
library(dplyr)
library(foreach)

args <- commandArgs(trailingOnly = TRUE)

start <- Sys.time()

dateAccessed <- as.character(args[1])
newSegmentNumber <- as.numeric(args[2])
cat(dateAccessed)

if (!exists("dateAccessed")) stop("You need to specify the variable dateAccessed")

# define repo folder names
folderName <- paste0("data/run_", dateAccessed)
tempFolderName <- paste0(folderName, "/temp")

# load the control parameters
readRDS(paste0(folderName,"/controlPars.RDS")) %>% 
  list2env(envir = .GlobalEnv)

###-----------------###
### 1. Preparation ####
###-----------------###
modelRun <- "richness" 
cat("\nPreparing data for model run for date", dateAccessed, " and model run", modelRun)

# Import local functions
sapply(list.files("functions", full.names = TRUE), source)

# Ensure that modelRun and dateAccessed are specified

# model output folder
modelFolderName <- paste0(folderName, "/modelOutputs")

# create folder for workspaces
if (!dir.exists(paste0(folderName, "/workspaces"))) {
  dir.create(paste0(folderName, "/workspaces"))
}

# Use 10000m grid for practice predictions
res <- 10000

# Import species list
focalTaxa <- read.csv(paste0(folderName, "/focalTaxa.csv"), header = T)
redList <- readRDS(paste0(folderName, "/redList.RDS"))


# Import datasets
regionGeometry <- readRDS(paste0(folderName, "/regionGeometry.RDS"))
focalCovariates <- read.csv(paste0(folderName, "/focalCovariates.csv"), header= T)
environmentalDataList <- rast(paste0(tempFolderName, "/environmentalDataImported.tiff"))
speciesData <- readRDS(paste0(folderName, "/speciesDataProcessed.RDS"))

#changing names with norwegian texts
speciesDatanameToChange <- names(speciesData)[which(grepl("[^\x01-\x7F]+", names(speciesData)))]
print(paste("Changing name of this dataset:", speciesDatanameToChange))
speciesDatanameChanged <- gsub('[^\x01-\x7F]+', ' ', speciesDatanameToChange)
names(speciesData)[names(speciesData) %in% speciesDatanameToChange] <- speciesDatanameChanged

# # Proj CRS needs to match SSB's Rutenett
projCRS <- paste0("EPSG:", crs)

cat("\nAll data loaded.", length(speciesData), "species datasets successfully loaded.")


# Import bird data from TOV and remove all no-relevant birds from other datasets
if ("birds" %in% focalTaxa$taxa) {
  speciesData[["TOVData"]] <- st_transform(readRDS("data/run_2025-01-04/temp/birdDataTOV.RDS"), st_crs(speciesData[[1]])) |>
    st_crop(st_transform(regionGeometry, st_crs(speciesData[[1]])))
  focalTaxa$predictionDataset[focalTaxa$taxa %in% c("birds", "groundNestingBirds", "woodpeckers")] <- "TOVData"
  speciesData <- lapply(speciesData, FUN = function(x) {
    x <- x[!(x$taxa %in% c("birds", "woodpeckers", "groundNestingBirds") &
               !(x$acceptedScientificName %in% unique(speciesData$TOVData$acceptedScientificName))),]
  })
  cat("Birds data filtered on TOV species.")
}



# Define speciesData based on run type and create predictionData
modelSpeciesData <- refineSpeciesData(speciesData, redList, "allSpecies")
predictionData <- createPredictionData(c(res, res), regionGeometry, proj = crs)

# # Split up data for vascular plants
# if (focalTaxa$taxa == "vascularPlants") {
#   if (nrow(focalTaxa) > 1) {stop("cannot divide taxa data for more than one taxa simultaneously")}
#   speciesDivisions <- 6
#   # Get taxa species list
#   modelSpeciesDataBasic <- do.call(rbind, lapply(modelSpeciesData, FUN = function(ds1) {
#     st_drop_geometry(ds1[,c("simpleScientificName")])
#   }))
#   talliedData <- arrange(tally(group_by(modelSpeciesDataBasic, simpleScientificName)),-n)
#   cleanedData <- talliedData[talliedData$n > 1 & !is.na(talliedData$simpleScientificName),]
#   newTaxaNames <- paste0("vascularPlants", LETTERS[1:speciesDivisions])
#   cleanedData$names <- rep(newTaxaNames, nrow(cleanedData)/speciesDivisions)[1:nrow(cleanedData)]
# 
#   modelSpeciesData <- lapply(modelSpeciesData, FUN = function(ds2) {
#     ds2$taxa <- cleanedData$names[match(ds2$simpleScientificName, cleanedData$simpleScientificName)]
#     ds2
#   })
#   focalTaxa <- do.call("rbind", replicate(
#     speciesDivisions, focalTaxa, simplify = FALSE))
#   focalTaxa$taxa <- newTaxaNames
#   cat("Plant data is being split up into ", speciesDivisions, " sections. New taxa names are ", focalTaxa$taxa)
# }

cat("\nPrediction data and model species data successfully created. Starting to create segments of", newSegmentNumber, "species each.")

# Create list of taxa run
listSegments <- list()

# Prepare models
for(iter in 1:1){
  workflowList <- modelPreparation(focalTaxa[iter, ], focalCovariates, modelSpeciesData, 
                                   redListModelled = redList$GBIFName[redList$valid], 
                                   regionGeometry = regionGeometry,
                                   modelFolderName = modelFolderName, 
                                   environmentalDataList = environmentalDataList, 
                                   crs = projCRS, 
                                   segmentation = TRUE,
                                   nSegment = newSegmentNumber,
                                   speciesOccurenceThreshold = speciesOccurenceThreshold,
                                   datasetOccurreneThreshold = datasetOccurreneThreshold, 
                                   mergeAllDatasets = TRUE,
                                   richness = TRUE)
  focalTaxaRun <- names(workflowList)
  
  
  cat("Finished creating workflows.")
  
  
  # Get bias fields
  if (file.exists(paste0(folderName, "/metadataSummary.csv"))) {
    dataTypes <- read.csv(paste0(folderName, "/metadataSummary.csv"))
    biasFieldList <- defineBiasFields(focalTaxaRun, dataTypes[!is.na(dataTypes$processing),], modelSpeciesData, NULL)
  } else {
    biasFieldList <- rep(list(NULL), length(focalTaxonRun))
  }
  
  
  modelOutputs <- "Richness"
  
  listSegments[[iter]] <- focalTaxaRun
  if (grepl("vascularPlants", focalTaxa$taxa[iter])) {saveRDS(focalTaxaRun, paste0(folderName, "/segmentList", focalTaxa$taxa[iter] ,".RDS"))}
  save.image(file = paste0(folderName,"/workspaces/",  focalTaxa[iter, 1], "workflowWorkspace.RData"))
}

saveRDS(unlist(listSegments), paste0(folderName, "/segmentList.RDS"))
