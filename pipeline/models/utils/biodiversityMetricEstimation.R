


###-----------------------###
### 2. Compiling Species ####
###-----------------------###

library(plotKML)

# Begin compilation
for (i in 1:length(focalTaxa)) {
  
  # Define species group folder
  focalGroup <- focalTaxa[i]
  groupFolderLocation <- paste0(modelFolderName, "/", focalGroup)
  
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
  
  # And red-listed biodiversity (if at least 1)
  nRedList <- sum(focalSpecies$taxonomicGroup == focalGroup & focalSpecies$threatened == TRUE)
  if(nRedList > 0){
    redListedSpecies <- focalSpecies$species[focalSpecies$taxonomicGroup == focalGroup & 
                                              focalSpecies$threatened == TRUE]
    redListedSpecies <- redListedSpecies[redListedSpecies %in% colnames(speciesIntensityList)]
    redListedSpeciesIntensityList <- speciesIntensityList[,redListedSpecies]
    redListedIntensity <- if(nRedList > 1){
      rowSums(redListedSpeciesIntensityList)
    } else {
      redListedSpeciesIntensityList
    }
    redListedIntensityScaled <- (redListedIntensity - min(redListedIntensity, na.rm = TRUE))/
      (max(redListedIntensity, na.rm = TRUE)-min(redListedIntensity, na.rm = TRUE))
    
    # Save all relevant redlist lists
    redListPredictions <- readRDS(paste0(list.dirs(path = groupFolderLocation, recursive = FALSE)[1], "/Predictions.rds"))
    redListPredictions <- reproject(redListPredictions$predictions, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    redListPredictions$mean <- redListedIntensityScaled
    saveRDS(redListPredictions, file = paste0(groupFolderLocation, "/biodiversityMetric.RDS"))
  } else {
    redListPredictions <- NA
  }
  # Save all relevant redlist lists
  biodivPredictions <- readRDS(paste0(list.dirs(path = groupFolderLocation, recursive = FALSE)[1], "/Predictions.rds"))
  biodivPredictions <- reproject(biodivPredictions$predictions, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  biodivPredictions$mean <- totalIntensityScaled
  saveRDS(biodivPredictions, file = paste0(groupFolderLocation, "/biodiversityMetric.RDS"))
  outputList[[focalGroup]] <- list(biodiversity = biodivPredictions, redListBiodiversity = redListPredictions,
                                   speciesIntensities = speciesIntensitiesScaled)
  
  print(paste0("Finished running ", focalGroup))
  
}
