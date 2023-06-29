

#### SPECIES DATA PROCESSING ####

# The following script processes our different forms of data based on rules for different datasets, so that
# they are ready for use in our integrated SDMs.

# First thing is to bring in imported data
if (!exists("dateAccessed")) {
  dateAccessed <- as.character(Sys.Date())
}

# Import species list
focalSpecies <- read.csv("data/external/focalSpecies.csv", header = T)
focalSpecies <- focalSpecies[focalSpecies$selected,]
focalGroups <- unique(focalSpecies$taxonomicGroup)

# Import datasets
folderName <- paste0("data/run_", dateAccessed)
tempFolderName <- paste0(folderName, "/temp")
speciesDataList <- readRDS(paste0(tempFolderName, "/speciesDataImported.RDS"))
speciesData <- speciesDataList[["species"]]
metadata <- speciesDataList$metadata$metadata

# Loop through and apply different processing scripts to various data types based on rules
# For now we only have three rules
# 1. Is it presence/absence? (but not ANO data)
# 2. Is it ANO Data?
# 3. Is it presence only?

# Start a processed data list
processedData <- list()
namesProcessedData <- c()
for (ds in 1:length(speciesData)) {
  focalData <- speciesData[[ds]]
  
  # If the dataset is empty, skip it
  if (nrow(focalData) == 0) next
  
  dataType <- unique(focalData$dataType)
  datasetName <- names(speciesData)[ds]
  if (dataType == "PA" & datasetName != "ANOData") { 
    # Here we apply our conversion script for presence/absence data
    source("utils/presenceAbsenceConversion.R")
  } else if (datasetName == "ANOData") {
    # ANO data has a different nedpoint and has already been taken care of in the utils/ANOIntegration scropt
    newDataset <- focalData[,c("simpleScientificName", "geometry", "individualCount", "dataType")]
  } else {
    # No need to do anything to presence only data (yet)
    newDataset <- focalData[,c("simpleScientificName", "geometry", "dataType")]
  }
  processedData[[ds]] <- newDataset
  namesProcessedData[ds] <- datasetName
}
names(processedData) <- namesProcessedData

# Save for use in model construction
processedData <- processedData[lapply(processedData,length)>0]
saveRDS(processedData, paste0(folderName, "/temp/speciesDataProcessed.RDS"))
