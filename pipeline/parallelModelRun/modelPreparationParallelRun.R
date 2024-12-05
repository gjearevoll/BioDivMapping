#### MODEL RUNS ####

###----------------------###
### 0. Bash preparation ####
###----------------------###

# The lib paths is for the cluster
.libPaths(c("/cluster/projects/nn11017k/R"))
library(foreach)
library(parallel)
library(doParallel)
library(dplyr)

#### MODEL RUNS ####

###----------------------###
### 0. Bash preparation ####
###----------------------###

# define repo folder names
dateAccessed <- "2024-12-04" 
folderName <- paste0("data/run_", dateAccessed)
tempFolderName <- paste0(folderName, "/temp")

# load the control parameters
readRDS(paste0(folderName,"/controlPars.RDS")) %>% 
  list2env(envir = .GlobalEnv)

###-----------------###
### 1. Preparation ####
###-----------------###
modelRun <- "richness" 
cat("Preparing data for model run for date", dateAccessed, " and model run", modelRun)

library(intSDM)
library(rgbif)
library(terra)
library(dplyr)



# Import local functions
sapply(list.files("functions", full.names = TRUE), source)

# Ensure that modelRun and dateAccessed are specified
if (!exists("modelRun")) stop("You need to specify the variable modelRun")
if (!exists("dateAccessed")) stop("You need to specify the variable dateAccessed")

# model output folder
modelFolderName <- paste0(folderName, "/modelOutputs")

# Use 500m grid
res <- 500

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

# Proj CRS needs to match SSB's Rutenett
projCRS <- "+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=km +no_defs"

cat("All data loaded.", length(speciesData), "species datasets successfully loaded.")


# Define speciesData based on run type and create predictionData
modelSpeciesData <- refineSpeciesData(speciesData, redList, modelRun)
segmentation <- modelRun %in% c("richness", "redListRichness")
predictionData <- createPredictionData(c(res, res), regionGeometry)

cat("Prediction data and model species data successfully created.")

# Prepare models
for(iter in 1:nrow(focalTaxa)){
  workflowList <- modelPreparation(focalTaxa[iter, ], focalCovariates, modelSpeciesData, 
                                   redListModelled = redList$GBIFName[redList$valid], 
                                   regionGeometry = regionGeometry,
                                   modelFolderName = modelFolderName, 
                                   environmentalDataList = environmentalDataList, 
                                   crs = projCRS, 
                                   segmentation = segmentation,
                                   nSegment = nSegment,
                                   speciesOccurenceThreshold = speciesOccurenceThreshold,
                                   datasetOccurreneThreshold = datasetOccurreneThreshold, 
                                   mergeAllDatasets = TRUE,
                                   richness = TRUE)
  focalTaxaRun <- names(workflowList)
  
  
  cat("Finished creating workflows.")
  
  
  # Get bias fields
  if (file.exists(paste0(folderName, "/metadataSummary.csv"))) {
    dataTypes <- read.csv(paste0(folderName, "/metadataSummary.csv"))
    redListUsed <- if (modelRun %in% c("richness","allSpecies")) NULL else redList
    biasFieldList <- defineBiasFields(focalTaxaRun, dataTypes[!is.na(dataTypes$processing),], modelSpeciesData, redListUsed)
  } else {
    biasFieldList <- rep(list(NULL), length(focalTaxonRun))
  }
  
  
  modelOutputs <- if(modelRun == "richness") 
    c('Richness') else if (modelRun == "redListRichness") 
      'Richness' else
        c('Predictions', 'Model')
  
  
  save.image(file = paste0(focalTaxa[iter, 1], "workflowWorkspace.RData"))}
