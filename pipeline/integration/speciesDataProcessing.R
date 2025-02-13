

#### SPECIES DATA PROCESSING ####

# The following script processes our different forms of data based on rules for different datasets, so that
# they are ready for use in our integrated SDMs.

library(dplyr)
library(stringr)
library(sf)
#library(rgdal)
library(terra)

# Import local functions
sapply(list.files("functions", full.names = TRUE), source)

###-----------------###
### 1. Preparation ####
###-----------------###

# if it is not already, define dateAccessed
if (!exists("dateAccessed")) {
  stop("Please define a run date for the model first.")
}
# define repo folder names
folderName <- paste0("data/run_", dateAccessed)
tempFolderName <- paste0(folderName, "/temp")

# import project control parameters into the environment
readRDS(paste0(folderName,"/controlPars.RDS")) %>% 
  list2env(envir = .GlobalEnv)

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
  
  cat("Currently processing dataset '", datasetName,"' \n", sep = "")
  
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
      simpleScientificName = gsub("-", "", gsub("×","", gsub(" ", "_", simpleScientificName)))
    )
  
  # Add in polyphyletic taxa
  newDataset$taxa <- ifelse(newDataset$acceptedScientificName %in% polyphyleticSpecies$acceptedScientificName, 
                            polyphyleticSpecies$taxa[match(newDataset$acceptedScientificName, polyphyleticSpecies$acceptedScientificName)], 
                            newDataset$taxa)
  
  # convert year to numeric
  newDataset$year <- as.numeric(newDataset$year)
  # newDatasetMasked <- st_intersection(newDataset, cityLakeMaskNA)
  # cat("Dataset masked.", (nrow(newDataset) - nrow(newDatasetMasked)), "entries removed.")
  # 
  # Save and name new dataset
  processedData[[ds]] <- newDataset
  namesProcessedData[ds] <- datasetName
}

names(processedData) <- namesProcessedData

# Save for use in model construction
processedData <- processedData[lapply(processedData,nrow)>0]

###-------------------------###
### 3. Mask lake/city data ####
###-------------------------###

# Import mask for removing species data in cities and lakes
cityLakeMask <- rast("localArchive/mask100.tiff")
cityLakeMaskNA <- st_transform(st_as_sf(as.polygons(ifel(cityLakeMask == 1, 1, NA))), crs(speciesData[[1]]))

maskedData <- lapply(processedData, FUN = function(x) {
  newDatasetMasked <- st_intersection(x, cityLakeMaskNA)
  cat("Dataset masked.", (nrow(x) - nrow(newDatasetMasked)), "entries removed.")
  return(newDatasetMasked)
})

maskedData <- maskedData[lapply(maskedData,nrow)>0]
saveRDS(maskedData, paste0(folderName, "/speciesDataProcessed.RDS"))

###--------------------------------###
### 4. Compile into one data.frame ####
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


# ###----------------------------###
# ### 5. Produce red list check ####
# ###----------------------------###
# 
# Here we see which species have sufficient presence/count data to actually run an individual species model
redListSpecies <- filterByRedList(redList$GBIFName, processedPresenceData, 50)
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


