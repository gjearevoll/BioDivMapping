

#### MODEL RUNS ####

###----------------------###
### 0. Bash preparation ####
###----------------------###

args <- commandArgs(TRUE)

# THis should only run if the script is being run from the command line
if (length(args) != 0) {
  # Set arguments
  dateAccessed <- args[1]
  modelRun <- args[2]
  # Set the working directory
  setwd("~/BioDivMapping")
}

# You can run this from the command line using for example
# Rscript filePath/speciesModelRuns.R 2024-02-08 allSPecies


###-----------------###
### 1. Preparation ####
###-----------------###

print("Preparing data for model run.")

library(intSDM)
library(rgbif)
library(terra)
library(dplyr)

# Import local functions
sapply(list.files("functions", full.names = TRUE), source)

# Ensure that modelRun and dateAccessed are specified
if (!exists("modelRun")) stop("You need to specify the variable modelRun")
if (!exists("dateAccessed")) stop("You need to specify the variable dateAccessed")

# Specify folders for storage of all run data
folderName <- paste0("data/run_", dateAccessed)
tempFolderName <- paste0(folderName, "/temp")

# model output folder
modelFolderName <- paste0(folderName, "/modelOutputs")

# import project control parameters into the environment
readRDS(paste0(folderName,"/controlPars.RDS")) %>% 
  list2env(envir = .GlobalEnv)

# Redefine modelRun after controlPars import using args if necessary
if (length(args) != 0) {
  modelRun <- args[2]
}

# Import species list
focalTaxa <- read.csv(paste0(folderName, "/focalTaxa.csv"), header = T)
redList <- if(modelRun %in% c("allSpecies")) NULL else
  readRDS(paste0(folderName, "/redList.RDS"))

# Import datasets
regionGeometry <- readRDS(paste0(folderName, "/regionGeometry.RDS"))
focalCovariates <- read.csv(paste0(folderName, "/focalCovariates.csv"), header= T)
environmentalDataList <- rast(paste0(tempFolderName, "/environmentalDataImported.tiff"))
speciesData <- readRDS(paste0(folderName, "/speciesDataProcessed.RDS"))
projCRS <- readRDS(paste0(tempFolderName,"/projCRS.RDS"))

# Define speciesData based on run type and create predictionData
modelSpeciesData <- refineSpeciesData(speciesData, modelRun)
segmentation <- modelRun %in% c("richness", "redListRichness")
predictionData <- createPredictionData(c(res, res), regionGeometry, projCRS)


# Prepare models
workflowList <- modelPreparation(focalTaxa, focalCovariates, modelSpeciesData, 
                                 redListModelled = redList$GBIFName[redList$valid], 
                                 regionGeometry = regionGeometry,
                                 modelFolderName = modelFolderName, 
                                 environmentalDataList = environmentalDataList, 
                                 crs = projCRS, segmentation)
focalTaxaRun <- names(workflowList)

# Get bias fields
if (file.exists(paste0(folderName, "/metadataSummary.csv"))) {
  dataTypes <- read.csv(paste0(folderName, "/metadataSummary.csv"))
  redListUsed <- if (modelRun %in% c("richness","allSpecies")) NULL else redList
  biasFieldList <- defineBiasFields(focalTaxaRun, dataTypes[!is.na(dataTypes$processing),], modelSpeciesData, redListUsed)
} else {
  biasFieldList <- rep(list(NULL), length(focalTaxonRun))
}

# Set model outputs
modelOutputs <- if(modelRun == "richness") 
  c('Richness', 'Bias') else if (modelRun == "redListRichness") 
    'Richness' else
    c('Predictions', 'Model')


###----------------###
### 2. Run models ####
###----------------###

print("Starting model run.")

# Begin running different species groups
for (i in seq_along(workflowList)) {
  
  # Define species group to create
  focalGroup <- names(workflowList)[i]
  workflow <- workflowList[[focalGroup]]
  
  # Find prediction dataset
  predictionDataset <- focalTaxa$predictionDataset[focalTaxa$taxa == gsub('[[:digit:]]+', '', focalGroup)]
  predictionDatasetShort <- gsub(" ", "", gsub("[[:punct:]]", "", predictionDataset))

  # Add model characteristics (mesh, priors, output)
  workflow$addMesh(cutoff= myMesh$cutoff, max.edge=myMesh$max.edge, offset= myMesh$offset)
  workflow$specifySpatial(prior.range = prior.range,
                          prior.sigma = prior.sigma)
  
  # spatial field
  workflow$modelOptions(ISDM = list(pointsSpatial = spatialField))
  
  # workflow$workflowOutput(c('Predictions', 'Bias', 'Model', 'Maps'))
  workflow$workflowOutput(modelOutputs)
  workflow$modelOptions(INLA = list(num.threads = 12, control.inla=list(int.strategy = 'eb', cmin = 0),safe = TRUE),
                        Richness = list(predictionIntercept = predictionDatasetShort))
  
  # Add bias fields if necessary
  if (!is.null(biasFieldList[[i]])) {
    workflow$biasFields(biasFieldList[[i]], shareModel = TRUE)
  }
  
  # Run model (this directly saves output to folder specified above)
  sdmWorkflow(workflow, predictionData = predictionData)
  
  # Change model name to ensure no overwrite of richness data
  if (modelRun %in% c("richness", "redListRichness")) {
    file.rename(paste0(folderName, "/modelOutputs/", focalGroup, "/richnessPredictions.rds"), 
                paste0(folderName, "/modelOutputs/", focalGroup, "/", modelRun, "Preds.rds"))
  }
}



