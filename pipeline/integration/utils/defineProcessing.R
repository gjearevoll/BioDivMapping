
### Define Dataset Processing ####

# This script assigns different functions to the datasets that they were created for. The functions herein
# reside in functions and begin with "process".

# 0. Check whether a pre-processed version of this dataset exists
dataFileName <- paste0(tempFolderName,"/", datasetName ,"/processedDataset.RDS")
if (file.exists(dataFileName)) {
  cat("\tPre-processed version used\n")
  newDataset <- readRDS(dataFileName)
  
# 1. The national insect Monitoring in Norway dataset
} else if (dataType == "insectMonitoring") {
  focalEndpoint <- metadata$DWCEndpoint[metadata$name == datasetName]
  newDataset <- processNationalInsectMonitoring(focalData, focalEndpoint, tempFolderName)
  
# 2. ANO Data
} else if (dataType == "ANO") {
  newDataset <- processANOData(focalData)
  
# 3. Field note data  
} else if (dataType == "fieldNotes"){
  focalEndpoint <- metadata$DWCEndpoint[metadata$name == datasetName]
  newDataset <- processFieldNotes(focalEndpoint, tempFolderName, datasetName, regionGeometry, focalTaxon, crs)
  
# 4. Field note data  - Oslo and Agder
} else if (dataType == "fieldNotesOslo"){
  focalEndpoint <- metadata$DWCEndpoint[metadata$name == datasetName]
  newDataset <- processFieldNotesOslo(focalEndpoint, tempFolderName, datasetName, regionGeometry, focalTaxon, crs)
  
  
# 5. Field note data (with events table)
} else if (dataType == "fieldNotesEvent") {
  focalEndpoint <- metadata$DWCEndpoint[metadata$name == datasetName]
  newDataset <- processFieldNotesEvent(focalEndpoint, tempFolderName, datasetName, regionGeometry, focalTaxon, crs)
  
# No need to do anything to presence only data (yet) except add individualCount column
} else if (dataType == "mareano") {
  focalEndpoint <- metadata$DWCEndpoint[metadata$name == datasetName]
  newDataset <- processMareano(focalEndpoint, tempFolderName, datasetName, regionGeometry, focalTaxon, crs)
  
  # No need to do anything to presence only data (yet) except add individualCount column
} else if (dataType == "marine1") {
  focalEndpoint <- metadata$DWCEndpoint[metadata$name == datasetName]
  newDataset <- processMarine1(focalEndpoint, tempFolderName, datasetName, regionGeometry, focalTaxon, crs)
  
  # No need to do anything to presence only data (yet) except add individualCount column
} else if (dataType == "marine2") {
  focalEndpoint <- metadata$DWCEndpoint[metadata$name == datasetName]
  newDataset <- processMarine2(focalEndpoint, tempFolderName, datasetName, regionGeometry, focalTaxon, crs)
  
  # No need to do anything to presence only data (yet) except add individualCount column
}else if (dataType == "presenceOnly") {
  focalData$dataType <- "PO"
  newDataset <- focalData[,c("acceptedScientificName", "geometry", "dataType", "taxa", "year", "taxonKeyProject")]
  newDataset <- st_transform(newDataset, crs)
} else {
  focalData$dataType <- "PO"
  newDataset <- focalData[,c("acceptedScientificName", "geometry", "dataType", "taxa", "year", "taxonKeyProject")]
  newDataset <- st_transform(newDataset, crs)
}
