

#### SPECIES DATA PROCESSING ####

# The following script processes our different forms of data based on rules for different datasets, so that
# they are ready for use in our integrated SDMs.

# First thing is to bring in imported data
if (!exists("dateAccessed")) {
  dateAccessed <- as.character(Sys.Date())
}

# Import species list
focalSpecies <- read.csv("data/external/focalSpecies.csv", header = T)
focalGroups <- unique(focalSpecies$taxonomicGroup)

# Import datasets
folderName <- paste0("data/run_", dateAccessed)
tempFolderName <- paste0(folderName, "/temp")
speciesDataList <- readRDS(paste0(tempFolderName, "/speciesDataImported.RDS"))
speciesData <- speciesDataList[["species"]]
metadata <- speciesDataList$metadata$metadata

# Loop through and find PA datasets
processedData <- list()
namesProcessedData <- c()
for (ds in 1:length(speciesData)) {
  focalData <- speciesData[[ds]]
  
  if (nrow(focalData) == 0) next
  
  dataType <- unique(focalData$dataType)
  datasetName <- names(speciesData)[ds]
  if (dataType == "PA" & datasetName != "ANOData") {
    source("utils/presenceAbsenceConversion.R")
  } else if (datasetName == "ANOData") {
    newDataset <- focalData[,c("simpleScientificName", "geometry", "individualCount", "dataType")]
  } else {
    newDataset <- focalData[,c("simpleScientificName", "geometry", "dataType")]
  }
  processedData[[ds]] <- newDataset
  namesProcessedData[ds] <- datasetName
}
names(processedData) <- namesProcessedData

processedData <- processedData[lapply(processedData,length)>0]
saveRDS(processedData, paste0(folderName, "/temp/speciesDataProcessed.RDS"))
