

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
focalTaxon <- read.csv("data/external/focalTaxa.csv")
focalTaxa <- unique(focalTaxon[focalTaxon$include, "taxa"])
redList <- readRDS(paste0(folderName, "/redList.RDS"))
redListFull <- readRDS(paste0(tempFolderName, "/speciesDataImported.RDS"))[["redList"]]

# Import datasets
regionGeometry <- readRDS(paste0(folderName, "/regionGeometry.RDS"))
environmentalDataList <- rast(paste0(tempFolderName, "/environmentalDataImported.tiff"))
speciesData <- readRDS(paste0(folderName, "/speciesDataProcessed.RDS"))
projCRS <- readRDS(paste0(tempFolderName,"/projCRS.RDS"))

# Prepare models
workflowList <- modelPreparation(focalTaxa, speciesData, redList$validSpecies, 
                                 regionGeometry, modelFolderName, environmentalDataList, projCRS)
focalTaxaRun <- names(workflowList)

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
  workflow$workflowOutput('Maps')
  workflow$modelOptions(INLA = list(control.inla=list(int.strategy = 'eb', cmin = 0),
                                    safe = TRUE))
  
  # Run model (this directly saves output to folder specified above)
  sdmWorkflow(workflow)
}

###------------------------------###
### 3. Get biodiversity metrics ####
###------------------------------###

# Create list to save data in for easy access for visualisations
outputList <- list()
source("pipeline/models/utils/modelResultsCompilation.R")

# Save visualisation data with species data
saveRDS(outputList, file=paste0(folderName, "/outputData.RDS"))
saveRDS(outputList, file="visualisation/hotspotMaps/data/outputData.RDS")

