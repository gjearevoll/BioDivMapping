

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
sapply(list.files("functions", full.names = TRUE), source)

###-----------------###
### 1. Preparation ####
###-----------------###

# if it is not already, define dateAccessed
if (!exists("dateAccessed")) {
  stop("Please define a run date for the model first.")
}

folderName <- paste0("data/run_", dateAccessed)
tempFolderName <- paste0(folderName, "/temp")


# Import project control parameters into the environment
readRDS(paste0(folderName,"/controlPars.RDS")) %>% 
  list2env(envir = .GlobalEnv)

# Import red list
if (file.exists(paste0(tempFolderName, "/redList.RDS"))) {
  redList <- readRDS(paste0(tempFolderName, "/redList.RDS"))
} else {
  stop("Please source initialiseRepository.R first.")
}

# import species list
if(file.exists(paste0(folderName, "/focalTaxa.csv"))){
  focalTaxon <- read.csv(paste0(folderName, "/focalTaxa.csv"), header = T)
} else {
  stop("Please source initialiseRepository.R first.")
}

# import polyphyletic groups
if(file.exists(paste0(folderName, "/polyphyleticSpecies.csv"))){
  polyphyleticSpecies <- read.csv(paste0(folderName, "/polyphyleticSpecies.csv"), header = T)
} 

# import regionGeometry list
if(file.exists(paste0(folderName, "/regionGeometry.RDS"))){
  regionGeometry <- readRDS(paste0(folderName, "/regionGeometry.RDS"))
} else {
  stop("Please source defineRegionGeometry.R first.")
}

# Import metadata information
if(file.exists(paste0(folderName, "/metadataSummary.csv"))){
  dataTypes <- read.csv(paste0(folderName, "/metadataSummary.csv"))
} 

###---------------------###
### 2. Filter Red list ####
###---------------------###

# Match to accepted names
speciesBackbones <- getGbifBackbone(redList$species)
redList$taxaKey <- matchBackboneKeys(speciesBackbones, focalTaxon$key)
focalTaxonCondensed <- focalTaxon[!is.na(focalTaxon$key),]
redList$taxa <- focalTaxonCondensed$taxa[match(redList$taxaKey, focalTaxonCondensed$key)]
redList$GBIFName <- speciesBackbones$scientificName

# Add polyphyletic taxa
polyphylaTaxaVector <-  polyphyleticSpecies$taxa[match(redList$GBIFName, polyphyleticSpecies$acceptedScientificName)]
redList$taxa <- ifelse(redList$GBIFName %in% polyphyleticSpecies$acceptedScientificName, 
                       polyphylaTaxaVector, redList$taxa)

# Cut out NAs and save redList
redList <- redList[!is.na(redList$taxa),]


###-----------------###
### 3. GBIF Import ####
###-----------------###

# First we need to buffer the regionGeometry to make sure we get everything
# regionGeometryExpanded <- st_buffer(regionGeometry, 0.01)

# If you want a scheduled download, the script will stop here (unless you've already run the download on GBIF)
# and you'll have to start again later once the download has completed
if (scheduledDownload) {
  
  # Get download key/initialise GBIF download
  if (file.exists(paste0(folderName, "/downloadKey.RDS"))) {
    downloadKey <- readRDS(paste0(folderName, "/downloadKey.RDS"))
  } else {
    downloadKey <- getDownloadKey(focalTaxon$key[!is.na(focalTaxon$key)], regionGeometry)
    if(waitForGbif){
      message("Download key has been created and will download once it is ready (5-30 minutes). ",
              "View the download status at https://www.gbif.org/occurrence/download/", 
              downloadKey)
      downloadKey <- occ_download_wait(downloadKey, curlopts = list(), quiet = FALSE)
      attr(downloadKey,'doi') <- downloadKey$doi
      saveRDS(downloadKey, file = paste0(folderName, "/downloadKey.RDS"))
    } else {
      downloadKey <- occ_download_meta(downloadKey) 
      saveRDS(downloadKey, file = paste0(folderName, "/downloadKey.RDS"))
      stop(paste0("Download key has been created and your download is being prepared. View the download at https://www.gbif.org/occurrence/download/",
                  downloadKey$key, ". Come back and start the download in 5-30 minutes."))
    }
  }
  
  # Start GBIF Download  
  source("pipeline/import/utils/formatScheduledDownload.R")
  occurrences <- occurrences[,c("acceptedScientificName", "decimalLongitude", "decimalLatitude", "basisOfRecord",
                                "year", "month", "datasetKey", "datasetName", "taxa", "taxonKeyProject", "taxonRank")] %>%
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

# Import dataset type based on dataset name. If no dataset information is provided, all data will be downloaded and assumed to
# be presence only data
GBIFImportCompiled <- merge(occurrences, metadataList$metadata, all.x=TRUE, by = "datasetKey")

# Import relevant datasets
if (file.exists(paste0(folderName, "/metadataSummary.csv"))) {
  GBIFImportCompiled$processing <- dataTypes$processing[match(GBIFImportCompiled$datasetKey, dataTypes$datasetKey)]
} else {
  GBIFImportCompiled$processing <- "presenceOnly"  
}
GBIFImportCompiled <- GBIFImportCompiled[!is.na(GBIFImportCompiled$processing),]

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

# Import data from external sources using specialised scripts. For now, the only external data imported
# is from ANO.
if ("vascularPlants" %in% focalTaxon$taxa) {
  GBIFLists[["ANOData"]] <- importANOData(tempFolderName, regionGeometry, focalTaxon, download = downloadANOData)
}

###--------------------###
### 5. Dataset Upload ####
###--------------------###

# For now we're just doing this to the data/temp folder, later this will go to Wallace. A version also needs to be saved in
# the visualisation folder though, as this will go into the occurrence mapping.
dataList <- list(species = GBIFLists, redList = redList, metadata = metadataList, projcrs = projcrs)
attr(dataList, "level") <- attr(regionGeometry, "level")
attr(dataList, "region") <- attr(regionGeometry, "region")
saveRDS(dataList, paste0(folderName, "/temp/speciesDataImported.RDS"))
