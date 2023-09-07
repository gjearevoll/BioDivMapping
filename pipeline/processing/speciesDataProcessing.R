

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

# Import species list
focalSpecies <- read.csv("data/external/focalSpecies.csv", header = T)
focalSpecies <- focalSpecies[focalSpecies$selected,]

# Import datasets
folderName <- paste0("data/run_", dateAccessed)
tempFolderName <- paste0(folderName, "/temp")
speciesDataList <- readRDS(paste0(tempFolderName, "/speciesDataImported.RDS"))
speciesData <- speciesDataList[["species"]]
metadata <- speciesDataList$metadata$metadata
regionGeometry <- readRDS(paste0(folderName, "/regionGeometry.RDS"))

###----------------###
### 2. Processing ####
###----------------###

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
    # Here we apply our conversion script for presence/absence data - tryCatch is for if any links to endpoints are broken
    tryCatch(
      {
        newDataset <- NULL
        source("utils/presenceAbsenceConversion.R")
      },
      error=function(e) {
        message(paste0('An error occurred and dataset ', datasetName, ' was not produced.'))
      }
    )
  } else if (datasetName == "ANOData") {
    # ANO data has a different nedpoint and has already been taken care of in the utils/ANOIntegration scropt
    newDataset <- focalData[,c("simpleScientificName", "SHAPE", "individualCount", "dataType")]
    newDataset$taxa <- focalSpecies$taxonomicGroup[match(newDataset$simpleScientificName, focalSpecies$species)]
    newDataset <- rename(newDataset, geometry = SHAPE)
  } else {
    # No need to do anything to presence only data (yet) except add individualCount column
    newDataset <- focalData[,c("simpleScientificName", "geometry", "dataType", "taxa")]
  }
  processedData[[ds]] <- newDataset
  namesProcessedData[ds] <- datasetName
}
names(processedData) <- namesProcessedData

# Save for use in model construction
processedData <- processedData[lapply(processedData,length)>0]
saveRDS(processedData, paste0(tempFolderName, "/speciesDataProcessed.RDS"))
saveRDS(processedData, "visualisation/hotspotMaps/data/processedDataList.RDS")

###----------------------###
### 3. Produce metadata ####
###----------------------###

# To add metadata we need to reformat the data as one data frame, as opposed to the list format it is currently in.

# Edit data frames to have same number of columns
processedDataForCompilation <- lapply(1:length(processedData), FUN = function(x) {
  dataset <- processedData[[x]]
  datasetName <- names(processedData)[x]
  datasetType <- unique(dataset$dataType)
  if (datasetType == "PO") {
    dataset$individualCount <- 1
  }
  datasetShort <- dataset[,c("simpleScientificName", "dataType", "individualCount", "geometry", "taxa")]
  datasetShort$datasetName <- datasetName
  datasetShort
}
)

# Combine into one data frame and add date accessed
processedDataCompiled <- do.call(rbind, processedDataForCompilation)
processedDataCompiled$dateAccessed <- Sys.Date()

# Turn geometry column to latitude and longitude
processedDataDF <- st_drop_geometry(processedDataCompiled)
processedDataDF[,c("decimalLongitude", "decimalLatitude")] <- st_coordinates(processedDataCompiled)

rmarkdown::render("utils/metadataProduction.Rmd", output_file = paste0("../",folderName, "/speciesMetadata.html"))


###-----------------------###
### 4. Upload to Wallace ####
###-----------------------###

if (uploadToWallace == TRUE) {
  
  # Connect to database
  targetDatabase <- "species_occurrences"
  source("utils/initiateWallaceConnection.R")
  
  # Upload
  dbAppendTable(con, name = "processed_species_data", value = processedDataDF)
}

