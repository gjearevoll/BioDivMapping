

#### SPECIES DATA IMPORT ####

# The following script imports our various forms of species data and processes them, based on the type of data
# (presence-absence/occurrence-only/abundance) and other specifications related to the source.

library(intSDM)
library(rgbif)
library(sf)
library(stringr)
library(dplyr)

chooseSpecies <-  TRUE # This will be given int he command line as a first argument

###-----------------###
### 1. Preparation ####
###-----------------###

# Run script to define geographical region and resolution we are working with 
level <- "county"  # level can be country, county, municipality, or points (examples of points given below)
region <- "50"
runBuffer <- FALSE
#points <- c(4.641979, 57.97976, 31.05787, 71.18488)
#names(points) <- c("north", "south", "east", "west")
source("utils/defineRegion.R")

# Define initial species list.
# Here we would normally import a list of species names with associated taxonomic groups. 
focalTaxa <- read.csv("data/external/focalTaxa.csv", header = T)

# Use this command if we want to choose certain species, for now we do
if (chooseSpecies == TRUE) {
  focalSpecies <- read.csv("data/external/focalSpecies.csv", header = T)
  focalSpecies <- focalSpecies[focalSpecies$selected,]
}

# Initialise folders for storage of all run data
if (!exists("dateAccessed")) {
  dateAccessed <- as.character(Sys.Date())
}
folderName <- paste0("data/run_", dateAccessed)
if (!file.exists(folderName)) {
  dir.create(folderName)
  dir.create(paste0(folderName, "/temp"))
}

###-----------------###
### 2. GBIF Import ####
###-----------------###

# Import GBIF Data
GBIFImport <- occ_data(scientificName = focalSpecies$species, hasCoordinate = TRUE, limit = 2000, 
                     geometry = st_bbox(regionGeometry), phylumKey = focalTaxa$key, coordinateUncertaintyInMeters = '0,500')
GBIFImportCompiled <- do.call(rbind, lapply(GBIFImport, FUN = function(x) {
  x$data[,c("scientificName", "decimalLongitude", "decimalLatitude", "basisOfRecord", "year", 
            "datasetKey", "coordinateUncertaintyInMeters", "datasetName")]
}))
GBIFImportCompiled$simpleScientificName <- gsub(" ", "_", word(GBIFImportCompiled$scientificName, 1,2, sep=" "))

###------------------------------###
### 3. Attach relevant metadata ####
###------------------------------###

# Now we import metadata related to GBIF data
fullMeta <- TRUE
metaSummary <- TRUE
source("utils/metadataPrep.R")

# Import dataset type based on dataset name
GBIFImportCompiled <- merge(GBIFImportCompiled, metadataList$metadata, all.x=TRUE, by = "datasetKey")
dataTypes <- read.csv("data/external/metadataSummary.csv")
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

source("utils/importANOData.R")
GBIFLists[["ANOData"]] <- ANOData


###--------------------###
### 5. Dataset Upload ####
###--------------------###

# For now we're just doing this to the data/temp folder, later this will go to Wallace. A version also needs to be saved in
# the visualisation folder though, as this will go into the occurrence mapping.
dataList <- list(species = GBIFLists, metadata = metadataList, geometry = regionGeometry, projcrs = projcrs)
attr(dataList, "level") <- level
attr(dataList, "region") <- region
saveRDS(dataList, paste0(folderName, "/temp/speciesDataImported.RDS"))
saveRDS(dataList, "visualisation/hotspotMaps/data/speciesDataList.RDS")

