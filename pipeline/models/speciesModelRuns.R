

#### MODEL RUNS ####

# The following script imports our various forms of environmental data and processes them, based on the type of data
# and other specifications related to the source.

###-----------------###
### 1. Preparation ####
###-----------------###

print("Preparing data for model run.")

library(intSDM)
library(rgbif)
library(terra)
library(dplyr)

if (!exists("modelRun")) stop("You need to specify the variable modelRun")

# Initialise folders for storage of all run data
if (!exists("dateAccessed")) {
  dateAccessed <- as.character(Sys.Date())
}
folderName <- paste0("data/run_", dateAccessed)
tempFolderName <- paste0(folderName, "/temp")
modelFolderName <- paste0(folderName, "/modelOutputs")
if (!file.exists(modelFolderName)) {
  dir.create(modelFolderName)
}

# Import local functions
sapply(list.files("functions", full.names = TRUE), source)

# Import species list
focalTaxa <- read.csv(paste0(folderName, "/focalTaxa.csv"), header = T)
focalTaxa <- focalTaxa[focalTaxa$include,]
redList <- readRDS(paste0(folderName, "/redList.RDS"))

# Import datasets
regionGeometry <- readRDS(paste0(folderName, "/regionGeometry.RDS"))
environmentalDataList <- rast(paste0(tempFolderName, "/environmentalDataImported.tiff"))
speciesData <- readRDS(paste0(folderName, "/speciesDataProcessed.RDS"))
projCRS <- readRDS(paste0(tempFolderName,"/projCRS.RDS"))

# Define speciesData based on run type and create predictionData
modelSpeciesData <- refineSpeciesData("redListRichness", speciesData)
predictionData <- createPredictionData(c(res/1000, res/1000), regionGeometry)

# Prepare models
workflowList <- modelPreparation(focalTaxa, modelSpeciesData, 
                                 redListModelled = redList$GBIFName[redList$valid], 
                                 regionGeometry = regionGeometry,
                                 modelFolderName = modelFolderName, 
                                 environmentalDataList = environmentalDataList, 
                                 crs = projCRS)
focalTaxaRun <- names(workflowList)

# Get bias fields
if ("metadataSummary.csv" %in% list.files("data/external")) {
  dataTypes <- read.csv("data/external/metadataSummary.csv")
  biasFieldList <- defineBiasFields(focalTaxaRun, dataTypes[!is.na(dataTypes$processing),], modelSpeciesData, redList)
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
for (i in 1:length(names(workflowList))) {
  
  # Define species group to create
  focalGroup <- names(workflowList)[i]
  workflow <- workflowList[[focalGroup]]

  # Add model characteristics (mesh, priors, output)
  workflow$addMesh(cutoff= myMesh$cutoff, max.edge=myMesh$max.edge, offset= myMesh$offset)
  workflow$specifySpatial(prior.range = c(300000, 0.05),
                          prior.sigma = c(500, 0.2)) #100
  workflow$workflowOutput(modelOutputs)
  workflow$modelOptions(INLA = list(num.threads = 12, control.inla=list(int.strategy = 'eb', cmin = 0),safe = TRUE),
                        Richness = list(predictionIntercept = 'ANOData'))
  
  # Add bias fields if necessary
  if (!is.null(biasFieldList[[i]])) {
    workflow$biasFields(biasFieldList[[i]], shareModel = TRUE)
  }
  
  # Run model (this directly saves output to folder specified above)
  sdmWorkflow(workflow, predictionData = predictionData)
  
  # Change model name to ensure no overwrite of richness data
  if (modelRun %in% c("richness", "redListRichness")) {
    file.rename(paste0(folderName, "/modelOutputs/", focalGroup, "/richnessPredictions.rds"), 
                paste0(folderName, "/modelOutputs/", focalGroup, "/", modelRun, "Predictions.rds"))
  }
}



