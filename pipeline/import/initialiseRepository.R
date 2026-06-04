
#### INITIALISE HOTSPOTS REPOSITORY ####

# The following script initialises the repository based on dateAccessed 
# filters focalTaxa and assigns missing usageKeys
# and saves a copy of focalTaxa.csv (cleaned), polyphyleticSpecies.csv,
# metadataSummary.csv, and focalCovariates.csv in the working folder

library(intSDM)
library(rgbif)
library(stringr)
library(dplyr)
library(rinat)

# Import local functions
sapply(list.files("functions", full.names = TRUE), source)

###------------------------###
### 1. Initialise folders ####
###------------------------###

# if it is not already, define dateAccessed
if (!exists("dateAccessed")) {
  warning("You have not defined a date to assign this repository today. Pipeline will use today's date.")
  dateAccessed <- as.character(Sys.Date())
}

# create missing folders 
folderName <- paste0("data/run_", dateAccessed)
tempFolderName <- paste0(folderName, "/temp")
extFolderName <- paste0(folderName, "/ext")
outFolderName <- paste0(folderName, "/out")

if (!file.exists(folderName)) {
  dir.create(folderName)
  dir.create(tempFolderName)
  dir.create(extFolderName)
  dir.create(outFolderName)
}

# model output folder
modelFolderName <- paste0(folderName, "/modelOutputs")
if (!file.exists(modelFolderName)) {
  dir.create(modelFolderName)
}

# location where focalTaxa, polyphyleticSpecies, metadataSummary, and focalCovariates CSVs are stored 
if (exists("externalFolder")){
  if (is.null(externalFolder)) {
    # create if defined as NULL
    externalFolder <- "data/external"
  }
} else {
  # create if it is not defined
  externalFolder <- "data/external"
}
# location where local environmental covariates are stored 
if (exists("localCovFolder")){
  if (is.null(localCovFolder)) {
    # create if defined as NULL
    localCovFolder <- "data/external/environmentalCovariates" 
  }
} else {
  # create if it is not defined
  localCovFolder <- "data/external/environmentalCovariates"  
}
# location where externaly downloaded environmental covariates are to be saved
if (exists("downloadCovFolder")){
  if (is.null(downloadCovFolder)) {
    # create if defined as NULL
    downloadCovFolder <- "data/temp" 
  }
} else {
  # create if it is not defined
  downloadCovFolder <- "data/temp" 
}

###-----------------------------###
### 2. Save control parameters ####
###-----------------------------###

if(file.exists(paste0(folderName,"/controlPars.RDS"))){
  controlPars <- readRDS(paste0(folderName,"/controlPars.RDS"))
  # assign controlPars back into the environment
  list2env(controlPars, envir = .GlobalEnv)
} else {
  # combine control parameters
  controlPars <- list(externalFolder = externalFolder,
                      localCovFolder = localCovFolder,
                      downloadCovFolder = downloadCovFolder,
                      extFolderName = extFolderName,
                      level = level,
                      region = region,
                      crs = crs,
                      res = res,
                      coordUncertainty = coordUncertainty,
                      issuesToFlag = issuesToFlag,
                      yearToStart = yearToStart,
                      scheduledDownload = scheduledDownload,
                      waitForGbif = waitForGbif,
                      myMesh = myMesh,
                      redListCategories = redListCategories,
                      prior.range = prior.range,
                      prior.sigma = prior.sigma,
                      nSegment = nSegment,
                      downloadANOData = downloadANOData,
                      speciesOccurrenceThreshold = speciesOccurrenceThreshold,
                      datasetOccurrenceThreshold = datasetOccurrenceThreshold,
                      temporal = temporal,
                      yearInterval = yearInterval,
                      maskCityData = maskCityData)
  # save
  saveRDS(controlPars, paste0(folderName,"/controlPars.RDS"))
}

###----------------------###
### 3. Download redList ####
###----------------------###

# Download raw red list from artsdatabanken to tempFolderName for desired cats
if (!file.exists(paste0(tempFolderName, "/redList.RDS"))) {
  importRedList(redListCategories) |>
    saveRDS(paste0(tempFolderName, "/redList.RDS"))  
}  

###-----------------------###
### 4. Process focalTaxa ####
###-----------------------###

# save copy of focalTaxa.csv
if(file.exists(paste0(folderName, "/focalTaxa.csv"))){
  focalTaxon <- read.csv(paste0(folderName, "/focalTaxa.csv"), header = T)
} else {
  focalTaxon <- read.csv(file.path(externalFolder, "focalTaxa.csv"), header = T)
}

# Refine focal taxon
focalTaxon <- focalTaxon[focalTaxon$include,]

# get missing keys
missingKey <- is.na(focalTaxon$key) & focalTaxon$level != "polyphyla"
if (length(missingKey[missingKey])>0) {
  focalTaxon$key[missingKey] <- getUsageKeys(focalTaxon$scientificName[missingKey], 
                                             rank = focalTaxon$level[missingKey], 
                                             strict = T)
}

# save for reference
write.csv(focalTaxon, paste0(folderName, "/focalTaxa.csv"), row.names = FALSE)

###---------------------------------###
### 5. Process polyphyleticSpecies ####
###---------------------------------###

# save copy of polyphyletic groups
if(!file.exists(paste0(folderName, "/polyphyleticSpecies.csv"))){
  read.csv(file.path(externalFolder, "polyphyleticSpecies.csv")) %>%
    filter(taxa %in% focalTaxon$taxa) %>% 
    # save for reference
    write.csv(paste0(folderName, "/polyphyleticSpecies.csv"), row.names = FALSE)
}

###-----------------------------###
### 6. Process metadataSummary ####
###-----------------------------###

# save copy of metadataSummary groups
if ("metadataSummary.csv" %in% list.files(externalFolder) &
    !file.exists(paste0(folderName, "/metadataSummary.csv"))){
  read.csv(file.path(externalFolder, "metadataSummary.csv")) %>%
    write.csv(paste0(folderName, "/metadataSummary.csv"), row.names = FALSE)
}

###-----------------------------###
### 7. Process focalCovariates ####
###-----------------------------###

# save for reference
if(!file.exists(paste0(folderName, "/focalCovariates.csv"))){
  read.csv(file.path(externalFolder, "focalCovariates.csv")) %>% 
    write.csv(paste0(folderName, "/focalCovariates.csv"), row.names = FALSE)
}

###-----------------------------###
### 8. save JSON ####
###-----------------------------###

# define json content
json_ls <- list(
  step_0 = list(
    dateAccessed = dateAccessed,
    level = level,
    region = region,
    crs = st_crs(crs)$wkt,
    res = res,
    units = "m",
    coordUncertaintyInMeters = coordUncertainty,
    timePeriod = c(as.Date(sprintf("%d-%02d-%02d", 
                                   yearToStart, 1, 1)), 
                   dateAccessed),
    taxa = list(taxa = focalTaxon$taxa,
                key = focalTaxon$key,
                level = focalTaxon$level,
                scientificName = focalTaxon$scientificName
    ),
    speciesCategorisation = list(
      # redListCategories = redListCategories
      # polyphyletic group (one taxa modelled separately)
      # functional groups (multiple species modelled as one)
    )
  )
)

# write json
jsonlite:::write_json(json_ls,
                      file.path(extFolderName, "metadata.json"), 
                      pretty = TRUE)

