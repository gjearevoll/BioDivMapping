

#### MODEL RUNS ####

# The following script imports our various forms of environmental data and processes them, based on the type of data
# and other specifications related to the source.

###-----------------###
### 1. Preparation ####
###-----------------###

print("Preparing data for model run.")

library(intSDM)
library(rgbif)

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

# Import species list
focalSpecies <- read.csv("data/external/focalSpecies.csv", header = T)
focalSpecies <- focalSpecies[focalSpecies$selected,]
focalGroups <- unique(focalSpecies$taxonomicGroup)

# Import datasets
importedDataList <- readRDS(paste0(tempFolderName, "/speciesDataImported.RDS"))
speciesData <- readRDS(paste0(tempFolderName, "/speciesDataProcessed.RDS"))
regionGeometry <- importedDataList[["geometry"]]
environmentalDataList <- readRDS("data/temp/environmentalDataImported.RDS")

source("utils/modelPreparation.R")

###----------------###
### 2. Run models ####
###----------------###

print("Starting model run.")

# Begin running different species groups
for (i in 1:length(focalGroups)) {
  
  # Define species group to create
  focalGroup <- focalGroups[i]
  workflow <- workflowList[[focalGroup]]

  # Add model characteristics (mesh, priors, output)
  workflow$addMesh(cutoff= 2000, max.edge=c(5000, 8000), offset= 100000)
  workflow$specifySpatial(prior.range = c(300000, 0.05),
                          prior.sigma = c(500, 0.2)) #100
  workflow$workflowOutput('Maps')
  workflow$modelOptions(INLA = list(control.inla=list(int.strategy = 'eb'),
                                    safe = TRUE))
  
  # Run model (this directly saves output to folder specified above)
  sdmWorkflow(workflow)
}

