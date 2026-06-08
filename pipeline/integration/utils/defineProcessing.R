
### Define Dataset Processing ####

# This script assigns different functions to the datasets that they were created for. The functions herein
# reside in functions and begin with "process".

# 0. Check whether a pre-processed version of this dataset exists
focalEndpoint <- unique(focalData$DWCEndpoint)
dataFileName <- paste0(tempFolderName,"/", datasetName ,"/processedDataset.RDS")
if (file.exists(dataFileName)) {
  cat("\tPre-processed version used\n")
  newDataset <- readRDS(dataFileName)
  
  # 1. The national insect Monitoring in Norway dataset
} else if (dataType == "insectMonitoring") {
  newDataset <- processNationalInsectMonitoring(focalData, focalEndpoint, tempFolderName, crs, coordUncertainty)
  
  # 1b. The national insect Monitoring in Norway dataset sturcture, but for other similar datasets
} else if (dataType == "insectMonitoringStandard") {
  newDataset <- processInsectMonitoring(focalData, focalEndpoint, tempFolderName, crs, coordUncertainty)
  
  # 2. ANO Data
} else if (dataType == "ANOData") {
  newDataset <- processANOData(focalEndpoint, tempFolderName, datasetName, regionGeometry, focalTaxon, crs, yearToStart) 
  
  # 3. Field note data  
} else if (dataType == "fieldNotes"){
  newDataset <- processFieldNotes(focalEndpoint, tempFolderName, datasetName, regionGeometry, focalTaxon, crs, coordUncertainty, yearToStart)
  
  # 4. Field note data  - Oslo and Agder
} else if (dataType == "fieldNotesOslo"){
  newDataset <- processFieldNotesOslo(focalEndpoint, tempFolderName, datasetName, regionGeometry, focalTaxon, crs, 
                                      coordUncertainty, yearToStart)
  
  
  # 5. Field note data (with events table)
} else if (dataType == "fieldNotesEvent") {
  newDataset <- processFieldNotesEvent(focalEndpoint, tempFolderName, datasetName, regionGeometry, focalTaxon, crs, coordUncertainty, yearToStart)
  
  # 6. NTNU freshwater collection
} else if (dataType == "NTNUFreshwater") {
  newDataset <- processNTNUFreshwater(focalEndpoint, tempFolderName, datasetName, regionGeometry, focalTaxon, crs, coordUncertainty, yearToStart)
  
  # 7. Freshwater fish inventory Norway
} else if (dataType == "freshwaterFishInventory") {
  newDataset <- processFreshwaterFishInventory(focalEndpoint, tempFolderName, datasetName, regionGeometry, focalTaxon, crs, coordUncertainty, yearToStart)
  
  # No need to do anything to presence only data (yet) except add individualCount column
}else if (dataType == "mareano") {
  newDataset <- processMareano(focalEndpoint, tempFolderName, datasetName, regionGeometry, focalTaxon, crs)
  
  # No need to do anything to presence only data (yet) except add individualCount column
} else if (dataType == "marine1") {
  newDataset <- processMarine1(focalEndpoint, tempFolderName, datasetName, regionGeometry, focalTaxon, crs)
  
  # No need to do anything to presence only data (yet) except add individualCount column
} else if (dataType == "marine2") {
  newDataset <- processMarine2(focalEndpoint, tempFolderName, datasetName, regionGeometry, focalTaxon, crs)
  
  # No need to do anything to presence only data (yet) except add individualCount column
}else if (dataType == "vannmiljo") {
  newDataset <- processVannmiljo(focalEndpoint, tempFolderName, datasetName, regionGeometry, focalTaxon, crs, coordUncertainty, yearToStart)

  # No need to do anything to presence only data (yet) except add individualCount column
}else if (dataType == "presenceOnly") {
  focalData$dataType <- "PO"
  newDataset <- focalData[,c("acceptedScientificName", "geometry", "dataType", "taxa", "year", "taxonKeyProject", "redListStatus")]
  newDataset <- st_transform(newDataset, crs)
  
} else if (dataType == "citizenScience") {
  focalData$dataType <- "PO"
  newDataset <- processCitizenScience(focalData, regionGeometry, '+proj=utm +zone=33 +datum=WGS84 +units=km +no_defs', tempFolderName, datasetName)
  
} else {
  focalData$dataType <- "PO"
  newDataset <- focalData[,c("acceptedScientificName", "geometry", "dataType", "taxa", "year", "taxonKeyProject")]
  newDataset <- st_transform(newDataset, crs)
}

if (!("redListStatus" %in% colnames(newDataset))) {
  newDataset$redListStatus <- focalData$redListStatus[match(newDataset$acceptedScientificName, 
                                                                           focalData$acceptedScientificName)]
}

if ("data.frame" %in% class(newDataset)) {
  # Redefine years IF we're using temporal data to match year interval
  newDataset$year <- as.integer(newDataset$year)
  if (temporal) {
    negative_number <- function(i) i[i <=0]
    newDataset$year <- sapply(newDataset$year, function(x) {x + max(negative_number(yearInterval - x))})
  }
  
  if (dataType %in% c("presenceOnly", "PO", "citizenScience")) {
    arrangedData <- newDataset %>%
      arrange(-year)
  } else {
    arrangedData <- newDataset %>%
      arrange(-individualCount, -year)
  }
  
  
  if (temporal) {
    # If it's a temporal model, we remove duplicates from the same species, year and geometry, assuming the 
    # most recent presence is correct
    newDataset <- arrangedData[!duplicated(arrangedData[,c("geometry", "year", "acceptedScientificName")]),]
  } else {
    # If it's a non-temporal model, we just keep the most recent observation, so only remove duplicates
    # of the same species and geometry
    newDataset <- arrangedData[!duplicated(arrangedData[,c("geometry", "acceptedScientificName")]),]
  }
}
