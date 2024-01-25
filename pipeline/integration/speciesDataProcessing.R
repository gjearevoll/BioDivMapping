

#### SPECIES DATA PROCESSING ####

# The following script processes our different forms of data based on rules for different datasets, so that
# they are ready for use in our integrated SDMs.

library(dplyr)
library(stringr)
library(sf)
library(rgdal)
library(terra)

# Import local functions
sapply(list.files("functions", full.names = TRUE), source)

###-----------------###
### 1. Preparation ####
###-----------------###

# Defiine whether or not we want to upload this data to Wallace
uploadToWallace <- FALSE

# if it is not already, define dateAccessed
if (!exists("dateAccessed")) {
  dateAccessed <- as.character(Sys.Date())
}
# define repo folder names
folderName <- paste0("data/run_", dateAccessed)
tempFolderName <- paste0(folderName, "/temp")

# import regionGeometry list
if(file.exists(paste0(folderName, "/regionGeometry.RDS"))){
  regionGeometry <- readRDS(paste0(folderName, "/regionGeometry.RDS"))
} else {
  stop("Please source defineRegionGeometry.R first.")
}

# Import datasets
speciesDataList <- readRDS(paste0(tempFolderName, "/speciesDataImported.RDS"))
speciesData <- speciesDataList[["species"]]
redList <- speciesDataList[["redList"]]
metadata <- speciesDataList$metadata$metadata

# Import taxa list and polyphyletic species
focalTaxon <- read.csv(paste0(folderName, "/focalTaxa.csv"), header = T)
focalTaxon <- focalTaxon[focalTaxon$include,]

# import polyphyletic groups
if(file.exists(paste0(folderName, "/polyphyleticSpecies.csv"))){
  polyphyleticSpecies <- read.csv(paste0(folderName, "/polyphyleticSpecies.csv"), header = T)
} 

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
for (ds in seq_along(speciesData)) {
  focalData <- speciesData[[ds]]
  
  # If the dataset is empty, skip it
  if (nrow(focalData) == 0) next
  
  dataType <- unique(focalData$processing)
  datasetName <- names(speciesData)[ds]
  newDataset <- NULL
  
  source("pipeline/integration/utils/defineProcessing.R")
  if (is.null(newDataset)) {break}
  
  # add simpleScientificName column
  newDataset <- newDataset %>%
    mutate(
      simpleScientificName = coalesce(
        redList$species[match(acceptedScientificName, redList$GBIFName)],  # Match redList species
        str_extract(acceptedScientificName, "^[A-Za-z]+\\s+[a-z]+")        # Extract binomial name
      ),
      # Replace space with underscore in simpleScientificName
      simpleScientificName = gsub("×","", gsub(" ", "_", simpleScientificName))
    )
  
  # Add in polyphyletic taxa
  newDataset$taxa <- ifelse(newDataset$acceptedScientificName %in% polyphyleticSpecies$acceptedScientificName, 
                            polyphyleticSpecies$taxa[match(newDataset$acceptedScientificName, polyphyleticSpecies$acceptedScientificName)], 
                            newDataset$taxa)
  
  # Save and name new dataset
  processedData[[ds]] <- newDataset
  namesProcessedData[ds] <- datasetName
}

names(processedData) <- namesProcessedData

# Save for use in model construction
processedData <- processedData[lapply(processedData,nrow)>0]
saveRDS(processedData, paste0(folderName, "/speciesDataProcessed.RDS"))


###--------------------------------###
### 3. COmpile into one dataframe ####
###--------------------------------###

# Edit data frames to have the same number of columns
processedDataCompiled <- do.call(rbind, lapply(1:length(processedData), FUN = function(x) {
  dataset <- processedData[[x]]
  datasetName <- names(processedData)[x]
  datasetType <- unique(dataset$dataType)
  if (datasetType == "PO") {
    dataset$individualCount <- 1
  }
  datasetShort <- dataset[, c("acceptedScientificName", "individualCount", "geometry", "taxa", "year", "dataType", 
                              "taxonKeyProject", "simpleScientificName")]
  datasetShort$dsName <- datasetName
  datasetShort
}))

# Remove absences, combine into one data frame and add date accessed
processedPresenceData <- processedDataCompiled[processedDataCompiled$individualCount > 0,]
processedRedListPresenceData <- processedPresenceData[processedPresenceData$acceptedScientificName %in% redList$GBIFName,]
saveRDS(processedPresenceData, paste0(folderName, "/processedPresenceData.RDS"))

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
redListSpecies <- filterByRedList(redList$GBIFName, processedPresenceData, redListThreshold)
redList$valid <- redList$GBIFName %in% redListSpecies$validSpecies
saveRDS(redList, paste0(folderName, "/redList.RDS"))

###-----------------------------------###
### 6. Produce species richness data ####
###-----------------------------------###

# Provide empty raster
blankRaster <- terra::project(rast(paste0(folderName, "/environmentalDataImported.tiff"))[[1]],
                              "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
allSpeciesRichness <- speciesRichnessConverter(regionGeometry, processedPresenceData, blankRaster)
writeRaster(allSpeciesRichness$rasters, paste0(folderName, "/speciesRichnessData.tiff"), overwrite=TRUE)
saveRDS(allSpeciesRichness$richness, paste0(folderName, "/speciesRichnessData.RDS"))

if(nrow(processedRedListPresenceData) > 0){
  redListRichness <- speciesRichnessConverter(regionGeometry, processedRedListPresenceData, blankRaster)
  writeRaster(redListRichness$rasters, paste0(folderName, "/redListRichnessData.tiff"), overwrite=TRUE)
  saveRDS(redListRichness$richness, paste0(folderName, "/redListRichnessData.RDS"))
}

###----------------------###
### 7. Produce metadata ####
###----------------------###

# To add metadata we need to reformat the data as one data frame, as opposed to the list format it is currently in.
rmarkdown::render("pipeline/integration/utils/metadataProduction.Rmd", output_file = paste0("../../../",folderName, "/speciesMetadata.html"))


###---------------------###
### 8. Download photos ####
###---------------------###

# We need to download photos from iNaturalist, including their URL and the user name of the individual
# who took the photo to give appropriate credit

# Check if there is already an image credit file
if (file.exists("visualisation/hotspotMaps/data/imageCredit.RDS")) {
  imageCredit <- readRDS("visualisation/hotspotMaps/data/imageCredit.RDS")
} else {
  imageCredit <- data.frame(species = redList$species[redList$valid],
                            credit = NA,
                            url = NA,
                            stringsAsFactors = FALSE)
}

for (taxa in unique(focalTaxon$taxa)) {
  focalRedListSpecies <- redList$species[redList$taxa == taxa & redList$valid]
  taxaFolder <- paste0("visualisation/hotspotMaps/data/photos/",taxa)
  if (!file.exists(taxaFolder)) {
    dir.create(taxaFolder)
  }
  for (species in focalRedListSpecies) {
    # Create species folder
    speciesFolder <- paste0(taxaFolder,"/",species)
    if (!file.exists(speciesFolder)) {
      dir.create(speciesFolder)
    }
    # Create species file (if it doesn't already exist)
    if (file.exists(paste0(speciesFolder, "/speciesImage.jpg"))) {next}
    tryCatch({inatAttempt <- rinat::get_inat_obs(taxon_name = species, maxresults = 10)
    imageRow <- inatAttempt[!is.na(inatAttempt$image_url) & inatAttempt$image_url != "",]
    imageCredit$url[imageCredit$species == species] <- imageRow$url[1]
    imageCredit$credit[imageCredit$species == species] <- imageRow$user_login[1]
    download.file(imageRow$image_url[1], destfile = paste0(speciesFolder,"/speciesImage.jpg" ), mode = 'wb')
    }
    , error = function(x){print(paste0("Error for species ",species, ". ", x))})
  }}

imageCredit$credit[imageCredit$credit == "" | is.na(imageCredit$credit)] <- "User name not provided"
saveRDS(imageCredit, "visualisation/hotspotMaps/data/imageCredit.RDS")

