


###-----------------------###
### 2. Compiling Species ####
###-----------------------###

library(plotKML)

# Firstly, figure out which species were and weren't


# Begin compilation
for (i in seq_along(focalTaxaRun)) {
  
  # Define species group folder
  focalGroup <- focalTaxaRun[i]
  groupFolderLocation <- paste0(modelFolderName, "/", focalGroup)
  
  # Print warning if there are any species listed for which we don't have a model
  speciesRun <- gsub("_", " ", gsub(paste0(groupFolderLocation, "/"), "", list.dirs(path = groupFolderLocation, recursive = FALSE)))
  speciesStarted <- redList$species[redList$taxa == focalGroup & redList$valid]
  speciesMissing <- speciesStarted[!(speciesStarted %in% speciesRun)]
  if (length(speciesMissing) > 0) {
    warning(paste0("The following species were not calculated but were in your original list: ", paste0(c(speciesMissing), collapse = ", ")))
  }
  
  # Extract individual species intensities
  predictions <- list.files(groupFolderLocation, 
                            pattern = "Predictions.rds", 
                            full.names = T, recursive = T)
  biases <- list.files(groupFolderLocation, 
                       pattern = "biasPreds.rds", 
                       full.names = T, recursive = T)
  speciesIntensities <- lapply(predictions, readRDS)
  taxaBias <- lapply(biases, readRDS)
  
  # Scale all columns for each species between 0 and 1
  speciesIntensitiesScaled <- lapply(seq_along(speciesIntensities), FUN = function(x) {
    intensityList <- speciesIntensities[[x]]
    intensityVector <- intensityList$predictions$mean
    intensityScaled <- (intensityVector - min(intensityVector))/(max(intensityVector)-min(intensityVector))
    intensityList$predictions$mean <- intensityScaled
    reprojectedIntensity <- suppressMessages(st_transform(intensityList$predictions, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    reprojectedIntensity
  })
  
  # Get all biases for datasets
  taxaBiasesScaled <- lapply(seq_along(taxaBias), FUN = function(x) {
    biasList <- taxaBias[[x]]
    biasVector <- biasList$biasFields$sharedBias$mean
    biasScaled <- (biasVector - min(biasVector))/(max(biasVector)-min(biasVector))
    biasList$biasFields$sharedBias$mean <- biasScaled
    reprojectedBias <- suppressMessages(st_transform(biasList$biasFields$sharedBias, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    reprojectedBias
  })
  biasNames <- gsub("/biasPreds.rds", "", list.files(groupFolderLocation, pattern = "biasPreds.rds", recursive = T))
  
  # Name columns and rescale aggregate for biodiversity metric
  names(speciesIntensitiesScaled) <- speciesRun
  names(taxaBiasesScaled) <- gsub("_", " ", biasNames)
  outputList[[focalGroup]] <- list(speciesIntensities = speciesIntensitiesScaled, bias = taxaBiasesScaled)
  
  print(paste0("Finished running ", focalGroup))
  
}
