
###----------------------###
### 0. Bash preparation ####
###----------------------###
args <- commandArgs(trailingOnly = TRUE)

start <- Sys.time()

i <- as.numeric(args[1])
dateToUse <- args[2]
covariatesSquared <- TRUE
.libPaths(c("/cluster/projects/nn11017k/R"))
#devtools::install_github("skiptoniam/qrbp")
# You can run this from the command line using for example
# Rscript filePath/speciesModelRuns.R 2024-02-08 allSPecies

###-----------------###
### 1. Import data ####
###-----------------###

print("Preparing data for model prediction.")

library(PointedSDMs)
library(intSDM)
library(dplyr)
library(purrr)
library(terra)
library(tidyterra)
library(stringr)
#library(ppmData)

# Load in segment number and interested group name
segmentList <- readRDS(paste0("data/run_", dateToUse, "/segmentList.RDS"))

interestedGroup <- gsub('[[:digit:]]+', '', segmentList[i])
load(paste0("data/run_", dateToUse, "/workspaces/", interestedGroup,"workflowWorkspace.RData"))

# For some reason i changes to 1 after loading the workspace

print(segmentList[i])
#Load packages
#.libPaths(c("/cluster/projects/nn11017k/R"))

library(intSDM)
library(rgbif)
library(terra)
library(dplyr)

# Load the workspace witht the workflowList object
#load("workflowWorkspace.RData")

focalGroup <- segmentList[i]
workflow <- workflowList[[focalGroup]]
print(focalGroup)

sampSize <- focalTaxa$sampleSize[focalTaxa$taxa == interestedGroup]
sampSize <- ifelse(is.na(sampSize), 1, sampSize)

rm("workflowList")

# load the control parameters
readRDS(paste0(folderName,"/controlPars.RDS")) %>% 
  list2env(envir = .GlobalEnv)

# Find prediction dataset
predictionDataset <- focalTaxa$predictionDataset[focalTaxa$taxa == gsub('[[:digit:]]+', '', focalGroup)]
predictionDatasetShort <- gsub(" ", "", gsub("[[:punct:]]", "", predictionDataset))
predictionDatasetShort <- predictionDatasetShort[1]
print(predictionDatasetShort)
# Choose one of the datasets within each segmentation as the prediction data
# I prefer to choose the one with the smallest data points
datasetNames <- workflow$.__enclos_env__$private$datasetName
datasetNames
namesSpeciesData <- names(speciesData)
namesSpeciesDataShort <- gsub(" ", "", gsub("[[:punct:]]", "", datasetNames))


if(!predictionDatasetShort %in% datasetNames){
  predictionDatasetShort <-  namesSpeciesDataShort[!predictionDatasetShort %in% namesSpeciesDataShort][1]
}
cat("Prediction data used is",predictionDatasetShort)


dateAccessed <- dateToUse
modelRun <- "richness"
covariatesSquared <- TRUE
# Import local functions
sapply(list.files("functions", pattern = "\\.R$", full.names = TRUE), source)

# Ensure that dateAccessed is specified
if (!exists("modelRun")) stop("You need to specify the variable modelRun")
if (!exists("dateAccessed")) stop("You need to specify the variable dateAccessed")

# Specify folders for storage of all run data
folderName <- paste0("data/run_", dateAccessed)
tempFolderName <- paste0(folderName, "/temp")

# model output folder
modelFolderName <- paste0(folderName, "/modelOutputs")

# import project control parameters into the environment
readRDS(paste0(folderName,"/controlPars.RDS")) %>% 
  list2env(envir = .GlobalEnv)
cat("Loaded contral pars")


# Ensure that modelRun is specified
if (!exists("modelRun")) stop("You need to specify the variable modelRun")

# Redefine modelRun after controlPars import using args if necessary
#if (length(args) != 0) {
# modelRun <- args[2]
#}

# Prediction resolution in stated in the units used in preparing the data
# That is metres
predRes <- 20000

# Import model objects datasets
regionGeometry <- readRDS(paste0(folderName, "/regionGeometry.RDS"))
focalCovariates <- read.csv(paste0(folderName, "/focalCovariates.csv"), header= T)
environmentalDataList <- rast(paste0(tempFolderName, "/environmentalDataImported.tiff"))


mesh <- meshTest(myMesh, regionGeometry, crs = crs, print = TRUE)
#load("data/meshForProject.RData")
#mesh <- meshToUse
cat("Loaded mesh")

# Get the crs used in preparing the data for the models
projCRS <- "+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# Get the crs used in fitting the models
modelCRS <- "+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#mesh <- mesh%>%
#st_transform(., modelCRS)
#environmentalDataList <- project(environmentalDataList, projCRS )

# Import fitted models
models <- lapply(paste0(modelFolderName, "/", focalGroup), function(x){
  try(list.files(x, pattern = paste0("richnessModel.rds"), recursive = TRUE, full.names = TRUE))
})
cat(modelFolderName, "/", focalGroup)

###-----------------###
### 2. Prep objects ###
###-----------------##

# Define prediction raster grid


types <- sapply(seq(nlyr(environmentalDataList)), function(x){
  environmentalDataList[[x]][,1] %>% unlist %>% class
})
cat("Loaded types")
origCovs <- names(environmentalDataList)

cat("Named env list")

# define template prediction raster 
# convert crs to format accepted by sf, terra, and intSDM (& dependencies) 
predRast <- rast(ext(environmentalDataList), res = c(predRes, predRes), crs = projCRS)
# Define prediction raster grid at target resolution
if(any(types == "factor")){
  predGridfactor <- environmentalDataList[[types == "factor"]] 
  # Define prediction raster grid for continuous covs (interpolate when predRes <= res, else average) 
  predGrid <- terra::project(environmentalDataList[[types != "factor"]], predRast, 
                             method = if(predRes <= res) "bilinear" else "average") 
  # Define prediction raster grid catagorical covs
  factorRasters <- lapply(predGridfactor, function(x){
    out <- x
    res <- terra::project(x, predRast, method = "mode") 
    levels(res) <- levels(out)
    return(res)
  })%>%
    rast()
  #levels(factorRasters) <- levels(predGridfactor) # reassign levels 
  # Combine binary rasters into a SpatRaster object
  predGrid <- c(predGrid, factorRasters) 
  # names(predGrid) <- origCovs
} else {
  # Define prediction raster grid (interpolate when predRes <= res, else average) 
  predGrid <- terra::project(environmentalDataList, predRast, method = if(predRes <= res) "bilinear" else "average") 
}

cat("Defined prediction data")

# The prediction data is in a bounded box, and for landCover, we have values within
# the entire bounded box. We need to mask the covariates by the regionGeometry
predGrid <- regionGeometry %>%
  st_transform(., projCRS)%>%
  vect( )%>%
  mask(predGrid, .)

# define geometries to combine with prediction 
geometries <- xyFromCell(predGrid, seq(ncell(predGrid))) %>% 
  as.data.frame() %>% 
  st_as_sf(coords = c("x", "y"), crs = crs) 



origCovs <- names(environmentalDataList)
cat(origCovs)
# Define model outputs based on modelRun
modelOutputs <- if(modelRun == "richness") {
  c('Richness')
} else if (modelRun == "redListRichness") {
  'Richness' 
} else {
  c('Predictions', 'Bias', 'Covs', 'Spatial')
}

###-------------------------###
### 3. Generate predictions ###
###-------------------------###

for(mod in seq_along(models)){
  # identify focal taxon
  focalTaxon <- strsplit(models[mod][[1]], split = "/")[[1]][[4]]
  # import model
  model <- readRDS(models[mod][[1]])
  # identify species in model
  speciesIn <- model$species$speciesIn %>% unlist %>% unique
  # indentify if bias field
  biasField <- !is.null(model$summary.random$sharedBias_biasField) | !is.null(model$spatCovs$biasFormula)
  # identify covariates used in model 
  # covs <- rownames(model$summary.fixed) %>%
  #     stringr::str_subset(paste0("^(", paste(speciesIn, collapse = "|"), ")")) %>%
  #     stringr::str_remove(paste0("^(", paste(speciesIn, collapse = "|"), ")_")) %>% 
  #     unique
  covs <- model$spatCovs$name
  # identify categorical covariate factors 
  if (any(types == "factor")) {
    catCovCats <- model$summary.random[model$summary.random %>% names %>% 
                                         stringr::str_subset(paste0("^(", paste(speciesIn, collapse = "|"), ")"))]  %>% 
      sapply(function(cov){
        cov[,1]
      }) %>% unlist %>% #names %>% 
      stringr::str_remove(paste0("^(", paste(speciesIn, collapse = "|"), ")_")) %>% unique %>% 
      str_subset(paste0("^(", str_c(names(environmentalDataList[[types == "factor"]]), collapse = "|"), ")"))
    catCovs <- origCovs[sapply(origCovs, function(name) {
      any(str_detect(catCovCats, paste0("^", name)))
    })]
    covs <- unique(c(covs, catCovs))
  }

  
  # get complete list of covariate columns from which to predict

  cat("\nCompleted list of covs")
  
  # Obtain prediction data
  predData <- predGrid %>% 
    dplyr::select(all_of(covs)) %>% 
    as.data.frame(na.rm = FALSE) %>% 
    replicate(length(speciesIn) +1  , ., simplify = FALSE) %>% 
    reduce(cbind) %>% 
    bind_cols(geometries) %>% 
    #filter(rowSums(is.na(.)) != (ncol(.)-1))%>%
    st_sf()%>%
    #na.omit()%>%
    st_transform(., modelCRS)#%>% # transform the prediction data to the units used in model runs
  
  # Transform predGrid
  transformedPredRast <- project(predRast, modelCRS)
  cat("\nTransformed pred rast")
  
  regionGeometry <- regionGeometry %>%
    st_transform(modelCRS)
  
  cat("Starting prediction")
  predData <- sf::st_intersection(predData, regionGeometry)
  # update names
  names(predData) <- c(covs,
                       paste(rep(speciesIn, each = length(covs)), 
                             covs, sep = "_"), "geometry")
  
  cat("Generated pred names")           
  # identify bias covs
  if(!is.null(model$spatCovs$biasFormula)){
    biasCovs <- covs[covs %in% attributes(terms(model$spatCovs$biasFormula))$term.labels]
    # remove bias covariates from list
    covs <- covs[!covs %in% attributes(terms(model$spatCovs$biasFormula))$term.labels]
  }
  
  cat("\nIdentified bias covs")
  
  # Generate & convert & Save model/predicts (currently for all species grouped)
  for(type in modelOutputs){
    ret <- split(1:nrow(predData), seq(1, ceiling(nrow(predData) / 10000)))
    if(type == "Predictions") {
      pred <- lapply(ret, function(x){
        predict(model, data = predData[x, ], predictor = TRUE, mesh = mesh, num.threads = 10)
      })
    } else if(type == "Bias" && biasField) {
      pred <- lapply(ret, function(x){
        predict(model, data = predData[x, ], bias = TRUE, mesh = mesh, num.threads = 10) 
      })
      
      head(pred[[1]])
      #get species information and save
      
      spPred <- lapply(pred, function(x){
        res <-   x[[1]][[1]]#%>%
        # dplyr::filter(speciesName == 1)
      })%>%
        do.call("rbind", .)%>%
        dplyr::select("mean", "sd", "q0.025", "q0.5", "q0.975", "median")
      
      head(spPred)
      
      spPred <- rasterize(spPred, transformedPredRast , names(spPred)[!names(spPred) %in% names(predData)])
      # define species directory & save prediction
      path <- paste(c(strsplit(models[mod][[1]], "/")[[1]][1:4], "Bias"), collapse = "/")  # path
      # make_path(path)
      if(!file.exists(path)){
        dir.create(path)
      }
      saveRDS(spPred, file.path(path, paste0(type, ".rds")))
      
    } else if(type== "Covs") {
      pred <- predict(model, data = predData, covariates = covs, mesh = mesh)  # model$spatCovs$name
    } else if(type == "Spatial") {
      pred <- predict(model, data = predData, spatial = TRUE, mesh = mesh)
    } else if(type == "Richness" ){
      pred <- lapply(ret, function(x){
        richnessEst <- intSDM:::obtainRichness(model, predictionData = predData[x, ], predictionIntercept = predictionDatasetShort, sampleSize = sampSize)
        return(richnessEst$Probabilities)
      })
      
      # species
      species <- names(pred[[1]])
      for(sp in species){
        print(sp)
        spPred <- lapply(pred, function(x){
          res <-   x[[sp]]
        })%>%
          do.call("rbind", .)%>%
          dplyr::select("mean", "sd", "q0.025", "q0.5", "q0.975", "median")
        
        spPred <- rasterize(spPred, transformedPredRast , names(spPred)[!names(spPred) %in% names(predData)])
        # define species directory & save prediction
        path <- paste(c(strsplit(models[mod][[1]], "/")[[1]][1:4], sp), collapse = "/")  # path
        #make_path(path)
        if(!file.exists(path)){
          dir.create(path)
        }
        saveRDS(spPred, file.path(path, paste0(type, ".rds")))
      }
    }
    
    
  }
}


