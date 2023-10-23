

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
focalSpecies <- read.csv("data/external/focalSpecies.csv", header = T)
focalSpecies <- focalSpecies[focalSpecies$selected,]
focalTaxa <- unique(focalSpecies$taxonomicGroup)

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

###-----------------###
### 2. GBIF Import ####
###-----------------###
  
# Import GBIF Data
gbifImportsPerTaxa <- lapply(focalTaxa, FUN = function(x) {
  focalSpeciesImport <- focalSpecies$species[focalSpecies$taxonomicGroup == x]
  GBIFImport <- occ_data(scientificName = focalSpeciesImport, hasCoordinate = TRUE, limit = 3000, 
                         geometry = st_bbox(regionGeometry), coordinateUncertaintyInMeters = '0,500')
  # compile import 
  if(all(names(GBIFImport) == c("meta", "data"))){  # if only one species selected
    GBIFImportCompiled <- compileGBIFImport(GBIFImport)
  } else if(any(names(GBIFImport) %in% focalSpeciesImport)){  # if multiple species 
    GBIFImportCompiled <- do.call(rbind, lapply(GBIFImport, compileGBIFImport))
  }
  GBIFImportCompiled$simpleScientificName <- gsub(" ", "_", word(GBIFImportCompiled$acceptedScientificName, 1,2, sep=" "))
  GBIFImportCompiled
})
GBIFImportCompiled <- do.call(rbind, gbifImportsPerTaxa)
GBIFImportCompiled$taxa <- focalSpecies$taxonomicGroup[match(GBIFImportCompiled$simpleScientificName, focalSpecies$species)]

###------------------------------###
### 3. Attach relevant metadata ####
###------------------------------###

# Now we import metadata related to GBIF data
metadataList <- metadataPrep(GBIFImportCompiled, metaSummary = TRUE)

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

GBIFLists[["ANOData"]] <- importANOData(focalSpecies, tempFolderName, regionGeometry)

###--------------------###
### 5. Dataset Upload ####
###--------------------###

# For now we're just doing this to the data/temp folder, later this will go to Wallace. A version also needs to be saved in
# the visualisation folder though, as this will go into the occurrence mapping.
dataList <- list(species = GBIFLists, metadata = metadataList, projcrs = projcrs)
attr(dataList, "level") <- level
attr(dataList, "region") <- region
saveRDS(dataList, paste0(tempFolderName, "/speciesDataImported.RDS"))
saveRDS(regionGeometry, paste0(folderName, "/regionGeometry.RDS"))
saveRDS(regionGeometry, "visualisation/hotspotMaps/data/regionGeometry.RDS")
write.csv(focalSpecies, "visualisation/hotspotMaps/data/focalSpecies.csv", row.names = FALSE)


###---------------------###
### 6. Download photos ####
###---------------------###

# We need to download photos from iNaturalist, including their URL and the user name of the individual
# who took the photo to give appropriate credit

# Check if there is already an image credit file
if (file.exists("visualisation/hotspotMaps/data/imageCredit.RDS")) {
  imageCredit <- readRDS("visualisation/hotspotMaps/data/imageCredit.RDS")
} else {
  imageCredit <- data.frame(species = focalSpecies$species,
                            credit = NA,
                            url = NA,
                            stringsAsFactors = FALSE)
}

for (taxa in focalTaxa) {
  speciesImaged <- focalSpecies$species[focalSpecies$taxonomicGroup == taxa]
  taxaFolder <- paste0("visualisation/hotspotMaps/data/photos/",taxa)
  if (!file.exists(taxaFolder)) {
    dir.create(taxaFolder)
  }
  for (species in speciesImaged) {
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
