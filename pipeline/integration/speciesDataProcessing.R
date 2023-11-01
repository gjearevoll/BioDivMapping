

#### SPECIES DATA PROCESSING ####

# The following script processes our different forms of data based on rules for different datasets, so that
# they are ready for use in our integrated SDMs.

library(dplyr)

###-----------------###
### 1. Preparation ####
###-----------------###

# Defiine whether or not we want to upload this data to Wallace
uploadToWallace <- FALSE

# First thing is to bring in imported data
if (!exists("dateAccessed")) {
  dateAccessed <- as.character(Sys.Date())
}

# Import taxa list
focalTaxon <- read.csv("data/external/focalTaxa.csv")
focalTaxon <- focalTaxon[focalTaxon$include,]

# Import datasets
folderName <- paste0("data/run_", dateAccessed)
tempFolderName <- paste0(folderName, "/temp")
speciesDataList <- readRDS(paste0(tempFolderName, "/speciesDataImported.RDS"))
speciesData <- speciesDataList[["species"]]
redList <- speciesDataList[["redList"]]
metadata <- speciesDataList$metadata$metadata
regionGeometry <- readRDS(paste0(folderName, "/regionGeometry.RDS"))

# Get list of processing util scripts
processingScripts <- gsub(".R", "", gsub("process", "", list.files("functions")[grepl("process", list.files("functions"))]))

# Import local functions
sapply(list.files("functions", full.names = TRUE), source)

###----------------###
### 2. Processing ####
###----------------###

# Loop through and apply different processing scripts to various data types based on rules
# For now we only have three rules
# 1. Is there a pre-set script for its processing?
# 2. Is it presence/absence?
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
  
  # 1. First condition - is there a pre-set script for this dataset?
  if (gsub(" ","",datasetName) %in% processingScripts) {
    source("pipeline/integration/utils/defineProcessing.R")
  } else if (dataType == "PA") { 
  # 2. Second condition - is it presence-absence?
    # Here we apply our conversion script for presence/absence data - tryCatch is for if any links to endpoints are broken
    focalEndpoint <- metadata$DWCEndpoint[metadata$name == datasetName]
    tryCatch(
      {
        newDataset <- NULL
        newDataset <- presenceAbsenceConversion(focalEndpoint, tempFolderName, datasetName, focalData, regionGeometry, focalTaxon)
      },
      error=function(e) {
        message(paste0('An error occurred and dataset ', datasetName, ' was not produced.'))
      }
    )
  } else {
  # 3. Doesn't satisfy the other two, which means it must be presence only 
    
    # No need to do anything to presence only data (yet) except add individualCount column
    newDataset <- focalData[,c("acceptedScientificName", "geometry", "dataType", "taxa", "year")]
  }
  processedData[[ds]] <- newDataset
  namesProcessedData[ds] <- datasetName
}

names(processedData) <- namesProcessedData

# Save for use in model construction
processedData <- processedData[lapply(processedData,length)>0]
saveRDS(processedData, paste0(folderName, "/speciesDataProcessed.RDS"))
saveRDS(processedData, "visualisation/hotspotMaps/data/processedDataList.RDS")


###--------------------------------###
### 3. COmpile into one dataframe ####
###--------------------------------###

# Edit data frames to have the same number of columns
processedDataForCompilation <- lapply(1:length(processedData), FUN = function(x) {
  dataset <- processedData[[x]]
  datasetName <- names(processedData)[x]
  datasetType <- unique(dataset$dataType)
  if (datasetType == "PO") {
    dataset$individualCount <- 1
  }
  datasetShort <- dataset[, c("acceptedScientificName", "individualCount", "geometry", "taxa", "year", "dataType")]
  datasetShort$dsName <- datasetName
  datasetShort
})

# Remove absences, combine into one data frame and add date accessed
processedDataCompiled <- do.call(rbind, processedDataForCompilation)
processedPresenceData <- processedDataCompiled[processedDataCompiled$individualCount > 0,]
processedRedListPresenceData <- processedPresenceData[processedPresenceData$acceptedScientificName %in% redList$GBIFName,]


###-----------------------###
### 4. Upload to Wallace ####
###-----------------------###

if (uploadToWallace == TRUE) {
  source("pipeline/integration/uploadToWallace.R")
}

###----------------------------###
### 5. Produce red list check ####
###----------------------------###

# Here we see which species have sufficient presence/count data to actually run an individual species model
redListSpecies <- filterByRedList(redList$GBIFName, processedPresenceData, 5)
saveRDS(redListSpecies, paste0(folderName, "/redList.RDS"))

###-----------------------------------###
### 6. Produce species richness data ####
###-----------------------------------###

# Provide empty raster
blankRaster <- project(rast(paste0(folderName, "/environmentalDataImported.tiff"))[[1]],
                       "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
allSpeciesRichness <- speciesRichnessConverter(regionGeometry, processedPresenceData, blankRaster)
writeRaster(allSpeciesRichness$rasters, "visualisation/hotspotMaps/data/speciesRichnessData.tiff", overwrite=TRUE)
saveRDS(allSpeciesRichness$richness, paste0(folderName, "/speciesRichnessData.RDS"))

if(nrow(processedRedListPresenceData) > 0){
  redListRichness <- speciesRichnessConverter(regionGeometry, processedRedListPresenceData, blankRaster)
  writeRaster(redListRichness$rasters, "visualisation/hotspotMaps/data/redListRichnessData.tiff", overwrite=TRUE)
  saveRDS(redListRichness$richness, paste0(folderName, "/redListRichnessData.RDS"))
}

###----------------------###
### 7. Produce metadata ####
###----------------------###

# To add metadata we need to reformat the data as one data frame, as opposed to the list format it is currently in.
rmarkdown::render("pipeline/integration/utils/metadataProduction.Rmd", output_file = paste0("../../../",folderName, "/speciesMetadata.html"))

