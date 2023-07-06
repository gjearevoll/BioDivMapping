

### BIODIVERSITY METRIC ESTIMATION ###

# This script takes our data for different species groups and creates a biodiversity metric for the same regions

###-----------------###
### 1. Preparation ####
###-----------------###

# Define the folder where we find our data
if (!exists("dateAccessed")) {
  dateAccessed <- as.character(Sys.Date())
}
folderName <- paste0("data/run_", dateAccessed, "/modelOutputs")

# Define species groups and import focal species
speciesGroups <- gsub(paste0(folderName, "/"), "", list.dirs(path = folderName, recursive = FALSE))
focalSpecies <- read.csv("data/external/focalSpecies.csv", header = T)
focalSpecies <- focalSpecies[focalSpecies$selected,]

# Create list to save data in for easy access for visualisations
outputList <- list()

###-----------------------###
### 2. Compiling Species ####
###-----------------------###

# Begin compilation
for (i in 1:length(speciesGroups)) {
  
  # Define species group folder
  focalGroup <- speciesGroups[i]
  groupFolderLocation <- paste0(folderName, "/", focalGroup)
  
  # Print warning if there are any species listed for which we don't have a model
  speciesRun <- gsub(paste0(groupFolderLocation, "/"), "", list.dirs(path = groupFolderLocation, recursive = FALSE))
  speciesMissed <- focalSpecies$species[!(focalSpecies$species %in% speciesRun)]
  if (length(speciesMissed) > 0) {
    warning(paste0("The following species were not calculated but were in your original list: ", speciesMissed))
  }
  
  # Extract individual species intensities
  speciesIntensities <- lapply(1:length(speciesRun), FUN = function(x) {
    dataset <- readRDS(paste0(list.dirs(path = groupFolderLocation, recursive = FALSE)[x], "/Predictions.rds"))
    dataset
  })
  names(speciesIntensities) <- speciesRun
  
  # Calculate metric for species intensities
  speciesIntensityList <- as.data.frame(sapply(1:length(speciesRun), FUN = function(x) {
    dataset <- speciesIntensities[[x]]
    dataset$predictions$mean
  }))
  speciesIntensityList$metric <- scale(apply(speciesIntensityList, 1, sum))
  
  # Now we scale all columns based on all numbers
  meanForScaling <- mean(as.vector(as.matrix(speciesIntensityList)))
  sdForScaling <- sd(as.vector(as.matrix(speciesIntensityList)))
  speciesIntensitiesScaled <- lapply(1:length(speciesIntensities), FUN = function(x) {
    intensityList <- speciesIntensities[[x]]
    intensityScaled <- (intensityList$predictions$mean - meanForScaling)/sdForScaling
    intensityList$predictions$mean <- intensityScaled
    intensityList
  })
  
  # Name columns and take mean for biodiversity metric
  names(speciesIntensitiesScaled) <- speciesRun
  
  # Save all relevant lists
  biodivPredictions <- readRDS(paste0(list.dirs(path = groupFolderLocation, recursive = FALSE)[1], "/Predictions.rds"))
  biodivPredictions$predictions$mean <- speciesIntensityList$metric
  saveRDS(biodivPredictions, file = paste0(groupFolderLocation, "/biodiversityMetric.RDS"))
  outputList[[focalGroup]] <- list(biodiversity = biodivPredictions, speciesIntensities = speciesIntensitiesScaled)
}

# Save visualisation data with species data
saveRDS(outputList, file=paste0(folderName, "/outputData.RDS"))
saveRDS(outputList, file="visualisation/hotspotMaps/data/outputData.RDS")
