

#### MODEL RUNS ####

###----------------------###
### 0. Bash preparation ####
###----------------------###

args <- commandArgs(TRUE)

# THis should only run if the script is being run from the command line
if (length(args) != 0) {
  # Set arguments
  dateAccessed <- args[1]
  modelRun <- args[2]
  # Set the working directory
  setwd("~/BioDivMapping")
}

# You can run this from the command line using for example
# Rscript filePath/speciesModelRuns.R 2024-02-08 allSPecies


###-----------------###
### 1. Preparation ####
###-----------------###

print("Preparing data for model run.")

library(intSDM)
library(rgbif)
library(terra)
library(dplyr)

# Import local functions
sapply(list.files("functions", full.names = TRUE), source)

# Ensure that modelRun and dateAccessed are specified
if (!exists("dateAccessed")) stop("You need to specify the variable dateAccessed")

# Specify folders for storage of all run data
folderName <- paste0("data/run_", dateAccessed)
tempFolderName <- paste0(folderName, "/temp")

# model output folder
modelFolderName <- paste0(folderName, "/modelOutputs")

# import project control parameters into the environment
readRDS(paste0(folderName,"/controlPars.RDS")) %>% 
  list2env(envir = .GlobalEnv)

# Import species list
focalTaxa <- read.csv(paste0(folderName, "/focalTaxa.csv"), header = T)
redList <- readRDS(paste0(folderName, "/redList.RDS"))

# Import datasets
regionGeometry <- readRDS(paste0(folderName, "/regionGeometry.RDS"))
focalCovariates <- read.csv(paste0(folderName, "/focalCovariates.csv"), header= T)
environmentalDataList <- rast(paste0(tempFolderName, "/environmentalDataImported.tiff"))
speciesData <- readRDS(paste0(folderName, "/speciesDataProcessed.RDS"))
projCRS <- readRDS(paste0(tempFolderName,"/projCRS.RDS"))

# Import bird data from TOV and remove all no-relevant birds from other datasets
if ("birds" %in% focalTaxa$taxa) {
  speciesData[["TOVData"]] <- st_transform(readRDS("localArchive/birdDataTOV.RDS"), st_crs(speciesData[[1]])) |>
    st_crop(st_transform(regionGeometry, st_crs(speciesData[[1]])))
  focalTaxa$predictionDataset[focalTaxa$taxa %in% c("birds", "groundNestingBirds", "woodpeckers")] <- "TOVData"
  speciesData <- lapply(speciesData, FUN = function(x) {
    x <- x[!(x$taxa %in% c("birds", "woodpeckers", "groundNestingBirds") &
               !(x$acceptedScientificName %in% unique(speciesData$TOVData$acceptedScientificName))),]
  })
}

# Define speciesData based on run type and create predictionData
modelSpeciesData <- refineSpeciesData(speciesData, redList, "richness")

levels(environmentalDataList$land_cover_corine)[[1]][,2][is.na(levels(environmentalDataList$land_cover_corine)[[1]][,2])] <- "Water bodies"
levels(environmentalDataList$land_cover_corine)[[1]][,2][28] <- "Moors and heathland"
landCover <- environmentalDataList$land_cover_corine 

values(environmentalDataList$land_cover_corine)[,1][is.nan(values(environmentalDataList$land_cover_corine)[,1])] <- 48
levels(environmentalDataList$land_cover_corine) <- levels(landCover)

# Prepare models
workflowList <- modelPreparation(focalTaxa, focalCovariates, modelSpeciesData, 
                                 redListModelled = redList$GBIFName[redList$valid], 
                                 regionGeometry = regionGeometry,
                                 modelFolderName = modelFolderName, 
                                 environmentalDataList = environmentalDataList, 
                                 crs = projCRS, 
                                 segmentation = TRUE,
                                 nSegment = nSegment,
                                 speciesOccurenceThreshold = speciesOccurenceThreshold,
                                 datasetOccurreneThreshold = datasetOccurreneThreshold, 
                                 mergeDatasets = TRUE,
                                 mergeAllDatasets = TRUE,
                                 richness = TRUE)
focalTaxaRun <- names(workflowList)

# Get bias fields
if (file.exists(paste0(folderName, "/metadataSummary.csv"))) {
  dataTypes <- read.csv(paste0(folderName, "/metadataSummary.csv"))
  redListUsed <- NULL
  biasFieldList <- defineBiasFields(focalTaxaRun, dataTypes[!is.na(dataTypes$processing),], modelSpeciesData, redListUsed)
} else {
  biasFieldList <- rep(list(NULL), length(focalTaxonRun))
}


###----------------###
### 2. Run models ####
###----------------###

print("Starting model run.")

# Begin running different species groups
for (i in seq_along(workflowList)) {
  
  # Define species group to create
  focalGroup <- names(workflowList)[i]
  workflow <- workflowList[[focalGroup]]
  
  # Find prediction dataset
  predictionDataset <- focalTaxa$predictionDataset[focalTaxa$taxa == gsub('[[:digit:]]+', '', focalGroup)]
  predictionDatasetShort <- gsub(" ", "", gsub("[[:punct:]]", "", predictionDataset))
  
  # Add model characteristics (mesh, priors, output)
  workflow$addMesh(cutoff= myMesh$cutoff, max.edge=myMesh$max.edge, offset= myMesh$offset)
  workflow$specifySpatial(prior.range = c(15, 0.01),
                          prior.sigma = c(0.8, 0.01))
  workflow$workflowOutput("Model")
  workflow$modelOptions(ISDM = list(pointCovariates = NULL,
                                    Offset = NULL, pointsIntercept = TRUE, 
                                    pointsSpatial = NULL))
  workflow$modelOptions(Richness = list(predictionIntercept = predictionDatasetShort, 
                                        speciesSpatial = 'replicate',samplingSize = 0.25))
  # Run model (this directly saves output to folder specified above)
  workflow$specifyPriors(effectNames = c("Intercept"), Mean = 0, Precision = 1,
                         priorIntercept = list(initial = -10, fixed = TRUE),
                         priorGroup = list(model = "iid",
                                           hyper = list(prec = list(initial = 2, fixed = TRUE))))
  
  workflow$modelFormula(covariateFormula = ~ summer_precipitation + summer_temperature + aspect +
                          net_primary_productivity + land_cover_corine + human_density + habitat_heterogeneity +
                          summer_precipitation_squared + summer_temperature_squared, 
                        biasFormula = ~ distance_water + distance_roads)
  
  # Add bias fields if necessary
  if (!is.null(biasFieldList[[i]])) {
    #check the biasFiledList in the dataset
    indx <- biasFieldList[[i]] %in% workflow$.__enclos_env__$private$datasetName
    workflow$biasFields(biasFieldList[[i]][indx], shareModel = TRUE)
  }
  
  # To run a very quick model
  sdmWorkflow(workflow,inlaOptions = list(num.threads = 4, control.inla=list(int.strategy = 'eb', cmin = 0.01,
                                                                             control.vb= list(enable = FALSE)),
                                          safe = TRUE, verbose = FALSE, debug = TRUE,
                                          inla.mode = "experimental"))
  
  # Change model name to ensure no overwrite of richness data
  # if (modelRun %in% c("richness")) {
  #   file.rename(paste0(folderName, "/modelOutputs/", focalGroup, "/richnessPredictions.rds"), 
  #               paste0(folderName, "/modelOutputs/", focalGroup, "/", modelRun, "Preds.rds"))
  # }
}



