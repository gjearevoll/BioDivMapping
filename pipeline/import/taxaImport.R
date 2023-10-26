

#### SPECIES DATA IMPORT ####

# The following script imports our various forms of species data and processes them, based on the type of data
# (presence-absence/occurrence-only/abundance) and other specifications related to the source.

library(intSDM)
library(rgbif)
library(sf)
library(stringr)
library(dplyr)
library(rinat)

# Import local functions
sapply(list.files("functions/import/species", full.names = TRUE), source)

###-----------------###
### 1. Preparation ####
###-----------------###

# Run script to define geographical region and resolution we are working with 
#extentCoords <- c(4.641979, 57.97976, 31.05787, 71.18488)
#names(extentCoords) <- c("north", "south", "east", "west")
if (!exists("level")) {level <- "county"}  # level can be country, county, municipality, or points (examples of points given below)
if (!exists("region")) {region <- "50"}
regionGeometry <- defineRegion(level, region)

# Define initial species list.
focalTaxon <- read.csv("data/external/focalTaxa.csv")
focalTaxa <- focalTaxon$taxa

# Initialise folders for storage of all run data
if (!exists("dateAccessed")) {
  dateAccessed <- as.character(Sys.Date())
}
folderName <- paste0("data/run_", dateAccessed)
tempFolderName <- paste0(folderName, "/temp")
if (!file.exists(folderName)) {
  dir.create(folderName)
  dir.create(tempFolderName)
}

# Import red list
redListCategories <- c("VU", "EN", "CR")
redList <- importRedList(redListCategories)

# Match to accepted names
redList$taxaKey <- sapply(redList$species, FUN = function(x) {taxaCheck(x, focalTaxon$key)})
redList$taxa <- focalTaxon$taxa[match(redList$taxaKey, focalTaxon$key)]
redList <- redList[!is.na(redList$taxa),]
redList$GBIFName <- sapply(redList$species, FUN = findGBIFName)

# Import relevant datasets
dataTypes <- read.csv("data/external/metadataSummary.csv")

###-----------------###
### 2. GBIF Import ####
###-----------------###


# If you want a scheduled download, the script will stop here (unless you've already run the download on GBIF)
# and you'll have to start again later once the download has completed
if (scheduledDownload == TRUE) {
  
  # Get download key/initialise GBIF download
  if (!file.exists(paste0(folderName, "/downloadKey.RDS"))) {
    downloadKey <- getDownloadKey(focalTaxon$key, regionGeometry)
    saveRDS(downloadKey, file = paste0(folderName, "/downloadKey.RDS"))
    stop(paste0("Download key has been created and your download is being prepared. View the download at ", downloadKey$doi,
                ". Come back and start the download in 5-30 minutes."))
  } else {
    downloadKey <- readRDS(paste0(folderName, "/downloadKey.RDS"))
  }
  
  # Start GBIF Download  
  source("pipeline/import/utils/formatScheduledDownload.R")
  occurrences <- occurrences[,c("acceptedScientificName", "decimalLongitude", "decimalLatitude", "basisOfRecord",
                                "year", "datasetKey", "datasetName", "taxa")] %>%
    filter(!is.na(taxa))
  
  # If you don't want a scheduled download and are only getting small amounts of data, the script
  # will use the following section instead. 
} else {
  # Import GBIF Data
  
  source("pipeline/import/utils/formatInstantDownload.R")
  occurrences <- do.call(rbind, gbifImportsPerTaxa)
  
}


###------------------------------###
### 3. Attach relevant metadata ####
###------------------------------###

# Now we import metadata related to GBIF data
metadataList <- metadataPrep(occurrences, metaSummary = TRUE)

# Import dataset type based on dataset name
GBIFImportCompiled <- merge(occurrences, metadataList$metadata, all.x=TRUE, by = "datasetKey")
GBIFImportCompiled$dataType <- dataTypes$dataType[match(GBIFImportCompiled$datasetKey, dataTypes$datasetKey)]
GBIFImportCompiled <- GBIFImportCompiled[!is.na(GBIFImportCompiled$dataType),]

# Narrow down to known data types and split into data frames
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
GBIFLists <- lapply(unique(GBIFImportCompiled$name), FUN  = function(x) {
  GBIFItem <- GBIFImportCompiled[GBIFImportCompiled$name == x,]
  GBIFItem <- st_as_sf(GBIFItem,                         
                       coords = c("decimalLongitude", "decimalLatitude"),
                       crs = projcrs)
  GBIFcropped <- st_intersection(GBIFItem, st_transform(regionGeometry, crs = projcrs))
  GBIFcropped
})

names(GBIFLists) <- unique(GBIFImportCompiled$name)

###----------------###
### 4. ANO Import ####
###----------------###

GBIFLists[["ANOData"]] <- importANOData(occurrences, tempFolderName, regionGeometry, focalTaxon)

###--------------------###
### 5. Dataset Upload ####
###--------------------###

# For now we're just doing this to the data/temp folder, later this will go to Wallace. A version also needs to be saved in
# the visualisation folder though, as this will go into the occurrence mapping.
dataList <- list(species = GBIFLists, redList = redList, metadata = metadataList, projcrs = projcrs)
attr(dataList, "level") <- level
attr(dataList, "region") <- region
saveRDS(dataList, paste0(folderName, "/temp/speciesDataImported.RDS"))
saveRDS(regionGeometry, paste0(folderName, "/regionGeometry.RDS"))
saveRDS(regionGeometry, "visualisation/hotspotMaps/data/regionGeometry.RDS")
write.csv(focalTaxon, "visualisation/hotspotMaps/data/focalTaxon.csv", row.names = FALSE)
