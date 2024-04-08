#### MODEL RUNS ####

###----------------------###
### 0. Bash preparation ####
###----------------------###

# The lib paths is for the cluster
#.libPaths(c("/cluster/projects/nn11017k/R", .libPaths()))
library(foreach)
library(parallel)
library(doParallel)
library(dplyr)

#### MODEL RUNS ####

###----------------------###
### 0. Bash preparation ####
###----------------------###
# iter is the row index for the focalTaxa we are interested in
rm("i")
iter = 1
# define repo folder names
#folderName <- paste0("data/run_", dateAccessed)
#tempFolderName <- paste0(folderName, "/temp")

# load the control parameters
#readRDS(paste0(folderName,"/controlPars.RDS")) %>% 
 # list2env(envir = .GlobalEnv)

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



# Import species list
focalTaxa <- read.csv(paste0(folderName, "/focalTaxa.csv"), header = T)
redList <- readRDS(paste0(folderName, "/redList.RDS"))
#head(redList)

# $`Vascular Plant Herbarium, UiB`
# Import datasets
regionGeometry <- readRDS(paste0(folderName, "/regionGeometry.RDS"))
#regionGeometry$weight <- 0.0001 #changes the weight 
focalCovariates <- read.csv(paste0(folderName, "/focalCovariates.csv"), header= T)
environmentalDataList <- rast(paste0(tempFolderName, "/environmentalDataImported.tiff"))
speciesData <- readRDS(paste0(folderName, "/speciesDataProcessed.RDS"))
#changing names with norwegian texts
speciesDatanameToChange <- names(speciesData)[which(grepl("[^\x01-\x7F]+", names(speciesData)))]
print(paste("Changing name of this dataset:", speciesDatanameToChange))
speciesDatanameChanged <- gsub('[^\x01-\x7F]+', ' ', speciesDatanameToChange)
names(speciesData)[names(speciesData) %in% speciesDatanameToChange] <- speciesDatanameChanged

projCRS <- readRDS(paste0(tempFolderName,"/projCRS.RDS"))

cat("All data loaded.", length(speciesData), "species datasets successfully loaded.")


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
                                 nSegment = 4)
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


save.image(file = "workflowWorkspace.RData")
saveRDS(workflowList, file = "beetleRichnessWorkflowList.rds")
saveRDS(predictionData, file = "beetleRichnessPredictionData.rds")



