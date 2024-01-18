

#### MODEL RESULTS COMPILATION ####

# The following script processes our model results, so that they are ready for use in the Hotspots App. It 
# also transfers all the relevant data into the visualisation folder.

library(plotKML)
library(sf)

# Initialise a list for our model outputs to live in
outputList <- list()

# Define the folder to find our results
if (!exists("dateAccessed")) {
  dateAccessed <- as.character(Sys.Date())
}
folderName <- paste0("data/run_", dateAccessed)
taxaRun <- list.dirs(paste0(folderName, "/modelOutputs"), recursive = FALSE, full.names = FALSE)

# Import red list
redList <- readRDS(paste0(folderName, "/redList.RDS"))

# Begin compilation
for (i in seq_along(taxaRun)) {
  
  # Define species group folder
  focalGroup <- taxaRun[i]
  groupFolderLocation <- paste0(folderName, "/modelOutputs/", focalGroup)
  
  # Print warning if there are any species listed for which we don't have a model
  speciesRun <- gsub("_", " ", gsub(paste0(groupFolderLocation, "/"), "", list.dirs(path = groupFolderLocation, recursive = FALSE)))
  
  # If no species, skip and register NULL
  if (length(speciesRun) == 0) {
    warning(sprintf("No species were above the given threshold for %s. NULL value will be returned", {focalGroup}))
    speciesIntensitiesScaled <- NULL
  } else {
    
    speciesStarted <- redList$species[redList$taxa == focalGroup & redList$valid]
    speciesMissing <- speciesStarted[!(speciesStarted %in% speciesRun)]
    if (length(speciesMissing) > 0) {
      warning(paste0("The following species were not calculated but were in your original list: ", paste0(c(speciesMissing), collapse = ", ")))
    }
    
    # Extract individual species intensities
    predictions <- list.files(groupFolderLocation, 
                              pattern = "Predictions.rds", 
                              full.names = T, recursive = T)
    speciesIntensities <- lapply(predictions, readRDS)
    
    # Scale all columns for each species between 0 and 1 if there are species modelled
    speciesIntensitiesScaled <- lapply(seq_along(speciesIntensities), FUN = function(x) {
      intensityList <- speciesIntensities[[x]]$predictions
      intensityVector <- intensityList$mean
      intensityScaled <- (intensityVector - min(intensityVector))/(max(intensityVector)-min(intensityVector))
      intensityList$scaledMean <- intensityScaled
      reprojectedIntensity <- suppressMessages(st_transform(intensityList, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
      reprojectedIntensity
    })
    names(speciesIntensitiesScaled) <- speciesRun
  }
  
  # Scale bias
  if (!file.exists(paste0(groupFolderLocation, "/biasRichnessPreds.rds"))) {
    warning(sprintf("A bias predictions file does not exist for %s. NULL value will be returned", {focalGroup}))
    reprojectedBias <- NULL
  } else {
    taxaBias <- readRDS(paste0(groupFolderLocation, "/biasRichnessPreds.rds"))
    biasVector <- taxaBias$biasFields$sharedBias$mean
    biasScaled <- (biasVector - min(biasVector))/(max(biasVector)-min(biasVector))
    taxaBias$biasFields$sharedBias$mean <- biasScaled
    reprojectedBias <- suppressMessages(st_transform(taxaBias$biasFields$sharedBias, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  }
  
  # Scale red-listed richness predictions
  if (!file.exists(paste0(groupFolderLocation, "/redListRichnessPreds.rds"))) {
    warning(sprintf("A red-listed richness predictions file does not exist for %s. NULL value will be returned", {focalGroup}))
    reprojectedRedListRichness <- NULL
  } else {
    taxaRedListRichness <- readRDS(paste0(groupFolderLocation, "/redListRichnessPreds.rds"))
    reprojectedRedListRichness <- suppressMessages(st_transform(taxaRedListRichness, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  }
  
  # Scale richness predictions
  if (!file.exists(paste0(groupFolderLocation, "/richnessPreds.rds"))) {
    warning(sprintf("A richness predictions file does not exist for %s. NULL value will be returned", {focalGroup}))
    reprojectedRichness <- NULL
  } else {
    taxaRichness <- readRDS(paste0(groupFolderLocation, "/richnessPreds.rds"))
    reprojectedRichness <- suppressMessages(st_transform(taxaRichness, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  }
  
  # Name columns and rescale aggregate for biodiversity metric
  outputList[[focalGroup]] <- list(speciesIntensities = speciesIntensitiesScaled, bias = reprojectedBias, 
                                   richness = reprojectedRichness, redListRichness = reprojectedRedListRichness)
  
  print(paste0("Finished running ", focalGroup))
  
}

# Save outputted data
saveRDS(outputList, file="visualisation/hotspotMapsNorsk/data/outputData.RDS")
saveRDS(outputList, file=paste0(folderName, "/outputData.RDS"))

# Create list of files which need to be transferred to visualisation folder
fileList <- c("redList.RDS", "environmentalDataImported.tiff", "environmentalDataImported.tiff.aux.xml",
              "focalTaxa.csv", "downloadKey.RDS", "redListRichnessData.tiff", "regionGeometry.RDS",
              "processedPresenceData.RDS", "speciesRichnessData.tiff")
sapply(fileList, FUN= function(x) {
  fileLocation <- paste0(folderName, "/", x)
  file.copy(fileLocation, "visualisation/hotspotMapsNorsk/data", overwrite = TRUE)
})

# And copy over metadata too
file.copy(paste0(folderName, "/speciesMetadata.html"), "visualisation/hotspotMaps", overwrite = TRUE)
