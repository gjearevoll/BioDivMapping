

#### TEMPORAL MODEL PREPARATION ####

# The following script imports our various forms of environmental data and processes them, based on the type of data
# and other specifications related to the source. Descriptions of all external data sources can be found here:
# https://github.com/gjearevoll/BioDivMapping/tree/main/data/temp

# NOTE: Before running this script, the speciesImport.R script needs to have been run.

library(dplyr)
library(terra)
library(sf)
library(fmesher)
library(purrr)
library(tidyterra)
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
#dateAccessed <- "2025-12-11"

###-----------------###
### 1. Preparation ####
###-----------------###

# Get all the data that will be in there already
cat("Importing data \n")
folderName <- paste0("data/run_", dateAccessed)
tempFolderName <- paste0("data/run_", dateAccessed, "/temp")
readRDS(paste0(folderName,"/controlPars.RDS")) %>% 
  list2env(envir = .GlobalEnv)

envData <- lapply(readRDS(paste0(tempFolderName, "/environmentalDataImported.RDS")), terra::unwrap)
regionGeometry <- readRDS(paste0(folderName, "/regionGeometry.RDS"))

crs <- '+proj=utm +zone=33 +datum=WGS84 +units=km +no_defs'

cat("Reprojecting data \n")
ennvDataProj <- lapply(envData, FUN = function(x) {
  project(x, crs)
})
regionGeometryProj <- st_transform(regionGeometry, crs)
myMesh$cutoff <- myMesh$cutoff/1000
myMesh$max.edge <- myMesh$max.edge/1000
myMesh$offset <- myMesh$offset/1000
meshToUse <- meshTest(myMesh, regionGeometryProj, crs = crs, print = TRUE)

focalTaxa <- read.csv(paste0(folderName,"/focalTaxa.csv"))
repData <- unique(focalTaxa$predictionDataset)

###----------------------------###
### 2. Create prediction data ####
###----------------------------###

# List with one spatraster for every year
envDataList <- list()
for (year in as.character(yearInterval)) {
  yearPred <- lapply(ennvDataProj, FUN = function(env) {
    if (nlyr(env) == 1) {
      return(env)
    } else {
      return(env[[year]])
    }
  })
  
  envDataList[[year]] <- rast(yearPred)
}

cat("Creating preddata \n")

# Neeed toconvert these spatrasters into appropriate sf data frames
predRast <- rast(ext(ennvDataProj[[1]]), res = c(1, 1), 
                 crs = crs)

predData <- lapply(envDataList, FUN = function(x) {
  predGrid <- terra::project(x, predRast, method = "average") 
  predGrid <- regionGeometryProj %>%
    st_transform(., crs)%>%
    vect( )%>%
    mask(predGrid, .)
  geometries <- xyFromCell(predGrid, seq(ncell(predGrid))) %>% 
    as.data.frame() %>% 
    st_as_sf(coords = c("x", "y"), crs = crs) 
  # Obtain prediction data
  predData2 <- predGrid %>% 
    dplyr::select(names(envDataList[[1]])) %>% 
    as.data.frame(na.rm = FALSE) %>% 
    #  replicate(2  , ., simplify = FALSE) %>% 
    reduce(cbind) %>% 
    bind_cols(geometries) %>% 
    st_sf()%>%
    st_transform(., crs)
  predData3 <- sf::st_intersection(predData2, regionGeometryProj)
  # update names
  names(predData3) <- c(names(envDataList[[1]]), "geometry")
  predData3
}) |>
  setNames(names(envDataList))

#saveRDS(predData, paste0(folderName, "/temp/predData.RDS"))
qsave(predData, paste0(folderName, "/temp/predData.qs"))


###-------------------------------------###
### 2. Create list of species to model ####
###-------------------------------------###

specData <- qread(paste0(folderName, "/speciesDataProcessed.qs"))
focalTaxon <- read.csv(paste0(folderName, "/focalTaxa.csv"), header = T)

if ("birds" %in% focalTaxon$taxa) {
  specData[["TOVData"]] <- st_transform(qread(paste0("localArchive/birdDataTOV_", dateAccessed,".qs")), st_crs(specData[[1]])) |>
    st_crop(st_transform(regionGeometry, st_crs(specData[[1]])))
  #focalTaxon$predictionDataset[focalTaxon$taxa %in% "birds"] <- "TOVData"
  specData <- lapply(specData, FUN = function(x) {
    x <- x[!(x$taxa %in% "birds" &
               !(x$acceptedScientificName %in% unique(specData$TOVData$acceptedScientificName))),]
  })
  cat("Birds data filtered on TOV species.")
  #saveRDS(specData, paste0(folderName, "/speciesDataProcessed.RDS"))
  qsave(specData, paste0(folderName, "/speciesDataProcessed.qs"))
}

repDataSpecies <- unique(specData[[unique(focalTaxon$predictionDataset)]]$simpleScientificName)

specDataOcc <- do.call(rbind, lapply(seq_along(specData), FUN = function(xl) {
  x <- specData[[xl]]
  x <- x[x$simpleScientificName %in% repDataSpecies,]
  if ("individualCount" %in% colnames(x)) {
    x <- x[x$individualCount > 0,]
  }
  x$dataset <- names(specData)[xl]
  st_drop_geometry(x[,c("simpleScientificName", "year","dataset" )])
}))

# Define limit for species data
speciesLimit <- 2*length(envData)

yearTallyCheck <- specDataOcc %>%
  filter(year > 0 & dataset == repData) %>%
  group_by(simpleScientificName, year) %>%
  tally() %>%
  filter(n > speciesLimit)
yearPresenceCheck <- yearTallyCheck %>%
  group_by(simpleScientificName) %>%
  tally() %>%
  filter(n == length(yearInterval))
finalSpeciesList <- yearTallyCheck %>%
  filter(simpleScientificName %in% yearPresenceCheck$simpleScientificName) %>%
  group_by(simpleScientificName) %>%
  summarise(totalOcc = sum(n))
speciesToRun <- finalSpeciesList$simpleScientificName

if ("birds" %in% focalTaxon$taxa) {
  speciesToRun <- unique(c(speciesToRun, "Vanellus_vanellus", "Grus_grus"))
}

saveRDS(speciesToRun, paste0(folderName, "/segmentList.RDS"))
# qsave(specData, paste0(folderName, "/speciesDataProcessed.qs"))


# 
# yearPresenceCheck2 <- yearTallyCheck %>%
#   filter(year %in% c(2012, 2018)) %>%
#   group_by(simpleScientificName) %>%
#   tally() %>%
#   filter(n == 2)

      