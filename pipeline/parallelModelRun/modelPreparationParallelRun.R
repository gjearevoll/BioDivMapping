#### MODEL RUNS ####

###----------------------###
### 0. Bash preparation ####
###----------------------###

# The lib paths is for the cluster
#.libPaths(c("/cluster/projects/nn11017k/R", .libPaths()))
#.libPaths(c("/cluster/projects/nn11017k/R"))
library(foreach)
library(parallel)
library(doParallel)
library(dplyr)

#### MODEL RUNS ####

###----------------------###
### 0. Bash preparation ####
###----------------------###
# iter is the row index for the focalTaxa we are interested in
#rm("i")
#iter = 1

# These can be copied to the masterScript.R file

# define repo folder names
dateAccessed <- "2024-08-26"  
folderName <- paste0("data/run_", dateAccessed)
tempFolderName <- paste0(folderName, "/temp")

# load the control parameters
readRDS(paste0(folderName,"/controlPars.RDS")) %>% 
  list2env(envir = .GlobalEnv)

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
if (!exists("dateAccessed")) stop("You need to specify the variable dateAccessed")

# model output folder
modelFolderName <- paste0(folderName, "/modelOutputs")

# Import species list
focalTaxa <- read.csv(paste0(folderName, "/focalTaxa.csv"), header = T)
redList <- readRDS(paste0(folderName, "/redList.RDS"))
#head(redList)

# Import datasets
regionGeometry <- readRDS(paste0(folderName, "/regionGeometry.RDS"))
focalCovariates <- read.csv(paste0(folderName, "/focalCovariates.csv"), header= T)
environmentalDataList <- rast(paste0(tempFolderName, "/environmentalDataImported.tiff"))
speciesData <- readRDS(paste0(folderName, "/speciesDataProcessed.RDS"))

# Changing names with norwegian texts
speciesDatanameToChange <- names(speciesData)[which(grepl("[^\x01-\x7F]+", names(speciesData)))]
print(paste("Changing name of this dataset:", speciesDatanameToChange))
speciesDatanameChanged <- gsub('[^\x01-\x7F]+', ' ', speciesDatanameToChange)
names(speciesData)[names(speciesData) %in% speciesDatanameToChange] <- speciesDatanameChanged

projCRS <- readRDS(paste0(tempFolderName,"/projCRS.RDS"))

#projCRS <- "+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=km +no_defs"

cat("All data loaded.", length(speciesData), "species datasets successfully loaded.")

# Define speciesData based on run type and create predictionData
modelSpeciesData <- refineSpeciesData(speciesData, redList, "richness")

cat("Prediction data and model species data successfully created.")


# Prepare models
for(iter in 1:nrow(focalTaxa)){
  workflowList <- modelPreparation(focalTaxa[iter, ], focalCovariates, modelSpeciesData, 
                                   redListModelled = redList$GBIFName[redList$valid], 
                                   regionGeometry = regionGeometry,
                                   modelFolderName = modelFolderName, 
                                   environmentalDataList = environmentalDataList, 
                                   crs = projCRS, 
                                   segmentation = TRUE,
                                   nSegment = nSegment,
                                   speciesOccurenceThreshold = speciesOccurenceThreshold,
                                   datasetOccurreneThreshold = datasetOccurreneThreshold, 
                                   mergeAllDatasets = TRUE, mergeDatasets = TRUE,
                                   richness = TRUE)
  focalTaxaRun <- names(workflowList)
  
  
  cat("Finished creating workflows.")
  
  
  # Get bias fields
  if (file.exists(paste0(folderName, "/metadataSummary.csv"))) {
    dataTypes <- read.csv(paste0(folderName, "/metadataSummary.csv"))
    biasFieldList <- defineBiasFields(focalTaxaRun, dataTypes[!is.na(dataTypes$processing),], modelSpeciesData, redList = NULL)
  } else {
    biasFieldList <- rep(list(NULL), length(focalTaxonRun))
  }
  
  
  modelOutputs <- 'Model'
  
  
  #remove the list that are not needed
  # rm(list=ls()[! ls() %in% c("workflowList","predictionData",
  #                            "folderName", "dataAccessed",
  #                            "speciesData", "focalTaxa",
  #                            "biasFieldList", "tempFolderName",
  #                            "modelOutputs", "modelRun")])
  
  save.image(file = paste0(focalTaxa[iter, 1],"workflowWorkspace.RData"))
}
