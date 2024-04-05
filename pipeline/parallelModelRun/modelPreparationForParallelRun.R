#### MODEL RUNS ####

###----------------------###
### 0. Bash preparation ####
###----------------------###

# The lib paths is for the cluster
#.libPaths(c("/cluster/projects/nn11017k/R", .libPaths()))
library(foreach)
library(parallel)
library(doParallel)

#### MODEL RUNS ####

###----------------------###
### 0. Bash preparation ####
###----------------------###
# iter is the row index for the focalTaxa we are interested in

iter = 1
#.libPaths(c("/cluster/projects/nn11017k/BioDivMapping/R", .libPaths()))
args <- commandArgs(TRUE)

# Date of analysis from which working directory will be create/access
dateAccessed <- "2024-04-04"  
# spatial level on which regionGeometry will be defined as accepted by defineRegion()
level <- "country"  
# specific region to be used as accepted by defineRegion()
region <- "Norway"  
# coordinate reference system to use for project. as accepted by sf::st_crs()
crs <- 25833 
# resolution in units of CRS (eg m in UTM, or degrees in lat/long)
res <- 1000 
# Parameters to define mesh for random fields
#myMesh <- list(cutoff = 250, max.edge=c(10, 20), offset= 800)
#meshTest(myMesh, regionGeometry, crs = crs)
# Defiine whether or not we want to upload this data to Wallace
uploadToWallace <- FALSE
# whether to use schedule download for GBIF data
scheduledDownload <- TRUE
# whether to wait and automatically download GBIF data when it is ready
waitForGbif <- TRUE
# minimum number of points for a species to be retained in the analysis
redListThreshold <- 30
# which categories are to be used for filtering/analysing red list species
redListCategories <- c("VU", "EN", "CR")
# the type of model that will be fitted to the data
modelRun <- "richness" 

nClusters = 20

# This should only run if the script is being run from the command line
#if (length(args) != 0) {
# Set arguments
# dateAccessed <- args[1]
# modelRun <- args[2]
# Set the working directory
#  setwd("~/BioDivMapping")
#}

# You can run this from the command line using for example
# Rscript filePath/speciesModelRuns.R 2024-02-08 allSPecies


###-----------------###
### 1. Preparation ####
###-----------------###

cat("Preparing data for model run for date", dateAccessed, " and model run", modelRun)

library(intSDM)
library(rgbif)
library(terra)
library(dplyr)



# Import local functions
sapply(list.files("functions", full.names = TRUE), source)

# Ensure that modelRun and dateAccessed are specified
if (!exists("modelRun")) stop("You need to specify the variable modelRun")
if (!exists("dateAccessed")) stop("You need to specify the variable dateAccessed")

# Specify folders for storage of all run data
folderName <- paste0("data/run_", dateAccessed)
tempFolderName <- paste0(folderName, "/temp")

# model output folder
modelFolderName <- paste0(folderName, "/modelOutputs")

# import project control parameters into the environment
#readRDS(paste0(folderName,"/controlPars.RDS")) %>% 
 # list2env(envir = .GlobalEnv)

# Redefine modelRun after controlPars import using args if necessary
#if (length(args) != 0) {
#  modelRun <- args[2]
#}

# Import species list
focalTaxa <- read.csv(paste0(folderName, "/focalTaxa.csv"), header = T)
redList <- readRDS(paste0(folderName, "/redList.RDS"))
#head(redList)

# Import datasets
regionGeometry <- readRDS(paste0(folderName, "/regionGeometry.RDS"))
#regionGeometry$weight <- 0.0001 #changes the weight 
focalCovariates <- read.csv(paste0(folderName, "/focalCovariates.csv"), header= T)
environmentalDataList <- rast(paste0(tempFolderName, "/environmentalDataImported.tiff"))
speciesData <- readRDS(paste0(folderName, "/speciesDataProcessed.RDS"))
projCRS <- readRDS(paste0(tempFolderName,"/projCRS.RDS"))

cat("All data loaded.", length(speciesData), "species datasets successfully loaded.")

#focalCovariates$selected_vascularPlants[focalCovariates$parameters == "ndvi_peak"] <- FALSE
#focalCovariates$selected_vascularPlants[focalCovariates$parameters == "elevation"] <- FALSE
#focalCovariates$selected_vascularPlants[focalCovariates$parameters == "distance_roads"] <- FALSE
#focalCovariates$selected_vascularPlants[focalCovariates$parameters == "land_cover_corine"] <- FALSE
#focalCovariates$selected_vascularPlants[focalCovariates$parameters == "losmasse"] <- FALSE
#focalCovariates$selected_vascularPlants[focalCovariates$parameters == "kalkinnhold"] <- FALSE

focalTaxa$forest_line <- FALSE
focalTaxa$aspect <- FALSE
focalTaxa$land_cover_corine <- FALSE
focalTaxa$snow_cover <- FALSE
focalTaxa$kalkinnhold <- FALSE
focalTaxa$net_primary_productivity <- FALSE
focalTaxa$slope <- FALSE
focalTaxa$distance_roads <- FALSE
#focalTaxa <- focalTaxa[-c(1,2), ]
#focalTaxa <- focalTaxa[-c(2,3), ]

# Define speciesData based on run type and create predictionData
modelSpeciesData <- refineSpeciesData(speciesData, redList, modelRun)
segmentation <- modelRun %in% c("richness", "redListRichness")
predictionData <- createPredictionData(c(res/1000, res/1000), regionGeometry)

cat("Prediction data and model species data successfully created.")

#focalTaxa$predictionDataset <- "ANOData"

# Prepare models
workflowList <- modelPreparation(focalTaxa[iter, ], focalCovariates, modelSpeciesData, 
                                 redListModelled = redList$GBIFName[redList$valid], 
                                 regionGeometry = regionGeometry,
                                 modelFolderName = modelFolderName, 
                                 environmentalDataList = environmentalDataList, 
                                 crs = projCRS, 
                                 segmentation = segmentation,
                                 nSegment = 10)
focalTaxaRun <- names(workflowList)


cat("Finished creating workflows.")


# Get bias fields
if (file.exists(paste0(folderName, "/metadataSummary.csv"))) {
  dataTypes <- read.csv(paste0(folderName, "/metadataSummary.csv"))
  redListUsed <- if (modelRun %in% c("richness","allSpecies")) NULL else redList
  biasFieldList <- defineBiasFields(focalTaxaRun, dataTypes[!is.na(dataTypes$processing),], modelSpeciesData, redListUsed)
} else {
  biasFieldList <- rep(list(NULL), length(focalTaxonRun))
}


modelOutputs <- if(modelRun == "richness") 
  c('Richness', 'Bias') else if (modelRun == "redListRichness") 
    'Richness' else
      c('Predictions', 'Model')


save.image(file = "beetleRichness.RData")
saveRDS(workflowList, file = "beetleRichnessWorkflowList.rds")
saveRDS(predictionData, file = "beetleRichnessPredictionData.rds")



