

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
  speciesMissed <- focalSpecies$species[!(focalSpecies$species %in% speciesRun) & focalSpecies$taxonomicGroup == focalGroup]
  if (length(speciesMissed) > 0) {
    warning(paste0("The following species were not calculated but were in your original list: ", speciesMissed))
  }
  
  # Extract individual species intensities
  speciesIntensities <- lapply(1:length(speciesRun), FUN = function(x) {
    dataset <- readRDS(paste0(list.dirs(path = groupFolderLocation, recursive = FALSE)[x], "/Predictions.rds"))
    dataset
  })
  names(speciesIntensities) <- speciesRun
  
  # Scale all columns for each species between 0 and 1
  speciesIntensitiesScaled <- lapply(1:length(speciesIntensities), FUN = function(x) {
    intensityList <- speciesIntensities[[x]]
    intensityVector <- intensityList$predictions$mean
    intensityScaled <- (intensityVector - min(intensityVector))/(max(intensityVector)-min(intensityVector))
    intensityList$predictions$mean <- intensityScaled
    reprojectedIntensity <- reproject(intensityList$predictions, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    reprojectedIntensity
  })
  
  # Name columns and rescale aggregate for biodiversity metric
  names(speciesIntensitiesScaled) <- speciesRun
  
  # Calculate total metric for biodiversity
  speciesIntensityList <- as.data.frame(sapply(1:length(speciesIntensitiesScaled), FUN = function(x) {
    dataset <- speciesIntensitiesScaled[[x]]
    dataset$mean
  }))
  colnames(speciesIntensityList) <- speciesRun
  
  # Get total biodiversity
  totalIntensity <- rowSums(speciesIntensityList)
  totalIntensityScaled <- (totalIntensity - min(totalIntensity, na.rm = TRUE))/
    (max(totalIntensity, na.rm = TRUE)-min(totalIntensity, na.rm = TRUE))
  
  # And red-listed biodiversity
  redListedSpecies <- focalSpecies$species[focalSpecies$taxonomicGroup == focalGroup & 
                                             focalSpecies$threatened == TRUE]
  redListedSpecies <- redListedSpecies[redListedSpecies %in% colnames(speciesIntensityList)]
  redListedSpeciesIntensityList <- speciesIntensityList[,redListedSpecies]
  redListedIntensity <- rowSums(redListedSpeciesIntensityList)
  redListedIntensityScaled <- (redListedIntensity - min(redListedIntensity, na.rm = TRUE))/
    (max(redListedIntensity, na.rm = TRUE)-min(redListedIntensity, na.rm = TRUE))
  
  # Save all relevant lists
  biodivPredictions <- readRDS(paste0(list.dirs(path = groupFolderLocation, recursive = FALSE)[1], "/Predictions.rds"))
  biodivPredictions <- reproject(biodivPredictions$predictions, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  biodivPredictions$mean <- totalIntensityScaled
  redListPredictions <- readRDS(paste0(list.dirs(path = groupFolderLocation, recursive = FALSE)[1], "/Predictions.rds"))
  redListPredictions <- reproject(redListPredictions$predictions, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  redListPredictions$mean <- redListedIntensityScaled
  saveRDS(biodivPredictions, file = paste0(groupFolderLocation, "/biodiversityMetric.RDS"))
  saveRDS(redListPredictions, file = paste0(groupFolderLocation, "/biodiversityMetric.RDS"))
  outputList[[focalGroup]] <- list(biodiversity = biodivPredictions, redListBiodiversity = redListPredictions,
                                   speciesIntensities = speciesIntensitiesScaled)
}

# Save visualisation data with species data
saveRDS(outputList, file=paste0(folderName, "/outputData.RDS"))
saveRDS(outputList, file="visualisation/hotspotMaps/data/outputData.RDS")
