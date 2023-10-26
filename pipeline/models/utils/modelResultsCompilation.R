


###-----------------------###
### 2. Compiling Species ####
###-----------------------###

library(plotKML)

# Firstly, figure out which species were and weren't


# Begin compilation
for (i in 1:length(focalTaxaRun)) {
  
  # Define species group folder
  focalGroup <- focalTaxaRun[i]
  groupFolderLocation <- paste0(modelFolderName, "/", focalGroup)
  
  # Print warning if there are any species listed for which we don't have a model
  speciesRun <- gsub("_", " ", gsub(paste0(groupFolderLocation, "/"), "", list.dirs(path = groupFolderLocation, recursive = FALSE)))
  speciesStarted <- redListFull$GBIFName[redListFull$taxa == focalGroup]
  speciesPrepped <- redList$validSpecies[redList$validSpecies %in% speciesStarted]
  speciesMissing <- speciesPrepped[!(speciesPrepped %in% speciesRun)]
  if (length(speciesMissing) > 0) {
    warning(paste0("The following species were not calculated but were in your original list: ", paste0(c(speciesMissing), collapse = ", ")))
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
  outputList[[focalGroup]] <- speciesIntensitiesScaled
  
  print(paste0("Finished running ", focalGroup))
  
}
