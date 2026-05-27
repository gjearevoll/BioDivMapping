

#### SPECIES DATA PROCESSING ####

# The following script processes our different forms of data based on rules for different datasets, so that
# they are ready for use in our integrated SDMs.

library(dplyr)
library(stringr)
library(sf)
#library(rgdal)
library(terra)
library(qs)

# Import local functions
sapply(list.files("functions", full.names = TRUE), source)

###----------------------###
### 0. Bash preparation ####
###----------------------###

args <- commandArgs(TRUE)

# THis should only run if the script is being run from the command line
if (length(args) != 0) {
  # Set arguments
  dateAccessed <- args[1]
  # Set the working directory
  setwd("~/BioDivMapping")
}

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
speciesData <- readRDS(paste0(tempFolderName, "/speciesDataImported.RDS"))

# Import taxa list and data types for processing
focalTaxon <- read.csv(paste0(folderName, "/focalTaxa.csv"), header = T)
focalTaxon <- focalTaxon[focalTaxon$include,]

# import polyphyletic groups
if(file.exists(paste0(folderName, "/polyphyleticSpecies.csv"))){
  polyphyleticSpecies <- read.csv(paste0(folderName, "/polyphyleticSpecies.csv"), header = T)
} 

# import baseRaster
baseRaster <- rast(file.path(folderName, "baseRaster.tiff"))

###------------------------------###
### 3. Preparing for processing ####
###------------------------------###

# Import metadata summary
if (file.exists(paste0(folderName, "/metadataSummary.csv"))) {
  dataTypes <- read.csv(paste0(folderName, "/metadataSummary.csv"))
  speciesData$processing <- dataTypes$processing[match(speciesData$datasetKey, dataTypes$datasetKey)]
} else {
  speciesData$processing <- "presenceOnly"  
}
speciesData <- speciesData[!is.na(speciesData$processing),]

# Narrow down to known data types and split into data frames
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
speciesData2 <- lapply(unique(speciesData$name), FUN  = function(x) {
  GBIFItem <- speciesData[speciesData$name == x,]
  GBIFItem <- st_as_sf(GBIFItem,                         
                       coords = c("decimalLongitude", "decimalLatitude"),
                       crs = projcrs)
  GBIFcropped <- st_intersection(GBIFItem, st_transform(regionGeometry, crs = projcrs))
  GBIFcropped
})
names(speciesData2) <- unique(speciesData$name)

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
for (ds in seq_along(speciesData2)) {
  focalData <- speciesData2[[ds]]
  
  # Remove invalid months if we have bird data
  if ("birds" %in% focalData$taxa) {
    focalData <- focalData[!(focalData$taxa == "birds" & focalData$month %in% c(1,2,3,4,9,10,11,12,NA,"")),]
  }
  
  # If the dataset is empty, skip it
  if (nrow(focalData) == 0) next
  
  dataType <- unique(focalData$processing)
  datasetName <- names(speciesData2)[ds]
  newDataset <- NULL
  
  cat("Currently processing dataset '", datasetName,"' \n", sep = "")
  
  source("pipeline/integration/utils/defineProcessing.R")
  if (is.null(newDataset)) {
    processedData[[ds]] <- NA
    namesProcessedData[ds] <- datasetName
    next
  }
  
  # add simpleScientificName column
  if ("acceptedScientificName" %in% colnames(newDataset)) {
    newDataset <- newDataset %>%
      mutate(
        simpleScientificName =  str_extract(acceptedScientificName, "^[A-Za-z]+\\s+[a-z]+") ,
        # Replace space with underscore in simpleScientificName
        simpleScientificName = gsub("-", "", gsub("×","", gsub(" ", "_", simpleScientificName)))
      )} else {
        newDataset <- newDataset %>%
          mutate(simpleScientificName = gsub("-", "", gsub("×","", gsub(" ", "_", scientificName))))
      }
  
  # Add in polyphyletic taxa
  if(file.exists(paste0(folderName, "/polyphyleticSpecies.csv"))){
    newDataset$taxa <- ifelse(newDataset$acceptedScientificName %in% polyphyleticSpecies$acceptedScientificName, 
                              polyphyleticSpecies$taxa[match(newDataset$acceptedScientificName, polyphyleticSpecies$acceptedScientificName)], 
                              newDataset$taxa)
  } 
  
  
  # convert year to numeric
  newDataset$year <- as.numeric(newDataset$year)
  
  # Save and name new dataset
  processedData[[ds]] <- newDataset
  namesProcessedData[ds] <- datasetName
}

names(processedData) <- namesProcessedData

# Remove empty datasets
processedData <- processedData[!(unlist(lapply(processedData,is.null)))]
processedData <- processedData[!(unlist(lapply(processedData,FUN = function(x) {is.null(nrow(x))})))]
processedData <- processedData[unlist(lapply(processedData,nrow)) > 0]


###------------------------------------###
### 3. Remove alien species from data ####
###------------------------------------###

alienSpeciesList <- readRDS("data/external/alienSpeciesList.RDS")
processedDataNative <- lapply(1:length(processedData), FUN = function(ds) {
  focalDataset <- processedData[[ds]]
  startingRows <- nrow(focalDataset)
  noAliens <- focalDataset[!(focalDataset$simpleScientificName %in% alienSpeciesList$simpleScientificName),]
  cat("Removed",startingRows - nrow(noAliens),"observations of alien species from", names(processedData)[ds],"\n")
  noAliens
})
names(processedDataNative) <- namesProcessedData

###-------------------------###
### 4. Mask lake/city data ####
###-------------------------###

# Import mask for removing species data in cities and lakes

if (maskCityData) {
  if (!file.exists("localArchive/mask100.tiff")) {
    maskedCats <-  c("Airports", "Continuous urban fabric", "Discontinuous urban fabric", "Industrial or commercial units",
                     "Green urban areas", "Sport and leisure facilities")
    cityMask <- produceLandscapeMask("data/temp/CORINE/EEA.zip", maskedCats, regionGeometry, crs, res)
    # save mask
    make_path("localArchive") # ensure path exists
    writeRaster(cityMask, "localArchive/mask100.tiff", overwrite = TRUE)
  } else {
    cityMask <- rast("localArchive/mask100.tiff")
  }
  cityMaskNA <- st_transform(st_as_sf(as.polygons(ifel(cityMask == 1, 1, NA))), crs= "+proj=longlat +ellps=WGS84")
  
  maskedData <- lapply(processedData, FUN = function(x) {
    newDatasetLongLat <- st_transform(x, crs = "+proj=longlat +ellps=WGS84")
    newDatasetMasked <- st_intersection(newDatasetLongLat, cityMaskNA)
    cat("\nDataset masked.", (nrow(x) - nrow(newDatasetMasked)), "entries removed.")
    newDatasetMasked2 <- st_transform(newDatasetMasked, crs = crs)
    return(newDatasetMasked2)
  })
} else {
  maskedData <- processedDataNative
}

maskedData <- maskedData[lapply(maskedData,nrow)>0]
qsave(maskedData, paste0(folderName, "/speciesDataProcessed.qs"))
#saveRDS(maskedData, paste0(folderName, "/speciesDataProcessed.RDS"))


###--------------------------------###
### 5. Compile into one data.frame ####
###--------------------------------###

# Edit data frames to have the same number of columns
processedDataCompiled <- do.call(rbind, lapply(1:length(maskedData), FUN = function(x) {
  dataset <- maskedData[[x]]
  datasetName <- names(maskedData)[x]
  datasetType <- unique(dataset$dataType)
  if (!("individualCount" %in% colnames(dataset))) {
    dataset$individualCount <- 1
  }
  datasetShort <- dataset[, c("acceptedScientificName", "individualCount", "geometry", "taxa", "year", "dataType", 
                              "taxonKeyProject", "simpleScientificName", "redListStatus")]
  datasetShort$dsName <- datasetName
  datasetShort
}))


###--------------------###
### 9. update JSON    ####
###--------------------###

finalDataSummary <- st_drop_geometry(processedDataCompiled) %>%
  group_by(dsName, taxa) %>%
  summarise(totalObs = n(),
            totalRec = sum(individualCount > 0),
            totalSpecies = n_distinct(acceptedScientificName)) %>%
  as.data.frame()

finalDataSummary2 <- st_drop_geometry(processedDataCompiled) %>%
  group_by(dsName, dataType) %>%
  tally()
finalDataSummary2$processing <- unlist(lapply(finalDataSummary2$dsName, FUN = function(x) {
  unique(speciesData$processing[speciesData$name == x])
}))

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
finalDataSummary2$processingScript <- ifelse(finalDataSummary2$processing == "presenceOnly", NA,
                                             paste0("https://github.com/gjearevoll/BioDivMapping/blob/main/functions/process",firstup(finalDataSummary2$processing),".R"))

# read existing json
json_ls <- fromJSON(file.path(extFolderName, "metadata.json"))

# define json content
json_ls$step_1b <- list(
  n_datasets = length(maskedData),
  n_total_obs = nrow(processedDataCompiled[processedDataCompiled$individualCount > 0,]),
  n_total_rec = nrow(processedDataCompiled),
  n_total_species = length(unique(processedDataCompiled$acceptedScientificName)),
  n_total_redlist_obs = nrow(processedDataCompiled[processedDataCompiled$individualCount > 0 & !is.na(processedDataCompiled$redListStatus),]),
  n_total_redlist_rec = nrow(processedDataCompiled[!is.na(processedDataCompiled$redListStatus),]),
  n_total_redlist_species =  length(unique(processedDataCompiled$acceptedScientificName[!is.na(processedDataCompiled$redListStatus)])),
  dataset_summary = list(
    dataset = finalDataSummary$dsName,
    taxa = finalDataSummary$taxa,
    n_total_obs = finalDataSummary$totalObs,
    n_total_rec = finalDataSummary$totalRec,
    n_total_species = finalDataSummary$totalSpecies
  ),
  dataset_processing = list(
    dataset = finalDataSummary2$dsName,
    dataset_category = finalDataSummary2$dataType,
    dataset_processing = finalDataSummary2$processingScript
    
  )
  
)



# write json
jsonlite:::write_json(json_ls,
                      file.path(extFolderName, "metadata.json"), 
                      pretty = TRUE)


# Remove absences, combine into one data frame and add date accessed
processedPresenceData <- processedDataCompiled[processedDataCompiled$individualCount > 0,]
processedRedListPresenceData <- processedPresenceData[processedPresenceData$acceptedScientificName %in% redList$GBIFName,]
#saveRDS(processedPresenceData, paste0(folderName, "/processedPresenceData.RDS"))
qsave(processedPresenceData, paste0(folderName, "/processedPresenceData.qs"))


# ###----------------------------###
# ### 6. Produce red list check ####
# ###----------------------------###
# 
# Here we see which species have sufficient presence/count data to actually run an individual species model
redListSpecies <- filterByRedList(redList$GBIFName, processedPresenceData, 50)
redList$valid <- redList$GBIFName %in% redListSpecies$validSpecies
saveRDS(redList, paste0(folderName, "/redList.RDS"))

###-----------------------------------###
### 7. Produce species richness data ####
###-----------------------------------###

# create template raster 
allSpeciesRichness <- speciesRichnessConverter(regionGeometry, processedPresenceData, baseRaster)
writeRaster(allSpeciesRichness$rasters, paste0(folderName, "/speciesRichnessData.tiff"), overwrite=TRUE)
saveRDS(allSpeciesRichness$richness, paste0(folderName, "/speciesRichnessData.RDS"))

if(nrow(processedRedListPresenceData) > 0){
  redListRichness <- speciesRichnessConverter(regionGeometry, processedRedListPresenceData, baseRaster)
  writeRaster(redListRichness$rasters, paste0(folderName, "/redListRichnessData.tiff"), overwrite=TRUE)
  saveRDS(redListRichness$richness, paste0(folderName, "/redListRichnessData.RDS"))
}

###----------------------###
### 8. Produce metadata ####
###----------------------###

# To add metadata we need to reformat the data as one data frame, as opposed to the list format it is currently in.
rmarkdown::render("pipeline/integration/utils/metadataProduction.Rmd", output_file = paste0("../../../",folderName, "/speciesMetadata.html"))

