
###----------------------###
### 0. Bash preparation ####
###----------------------###
args <- commandArgs(trailingOnly = TRUE)

start <- Sys.time()

i <- as.numeric(args[1])
dateAccessed <- args[2]
predRes <- as.numeric(args[3])
#.libPaths(c("/cluster/projects/nn11017k/R"))
#devtools::install_github("skiptoniam/qrbp")
# You can run this from the command line using for example
# Rscript filePath/speciesModelRuns.R 2024-02-08 allSPecies

###-----------------###
### 1. Import data ####
###-----------------###

print("Preparing data for model prediction.")

library(INLA)
library(PointedSDMs)
library(intSDM)
library(dplyr)
library(purrr)
library(terra)
library(tidyterra)
library(stringr)
library(dplyr)
library(qs, lib.loc = "/cluster/projects/nn11017k/BioDivMapping/R")


# Load in segment number and interested group name
segmentList <- readRDS(paste0("data/run_", dateAccessed, "/segmentList.RDS"))
interestedGroup <- gsub('[[:digit:]]+', '', segmentList[i])

# load in workflow list
folderName <- paste0("data/run_", dateAccessed)
workflowList <- qs::qread(paste0(folderName, "/workspaces/", interestedGroup, "_workflowList.qs"))
focalTaxa <- read.csv(paste0(folderName, "/focalTaxa_prepped.csv"), header = T)

print(segmentList[i])

# Define group and workflow
focalGroup <- segmentList[i]
workflow <- workflowList[[focalGroup]]
print(focalGroup)

# Choose one of the datasets within each segmentation as the prediction data
# I prefer to choose the one with the smallest data points
datasetNames <- workflow$.__enclos_env__$private$datasetName
datasetNames

# Read species data
speciesData <- qread(paste0(folderName, "/speciesDataProcessedPrepped.qs"))
namesSpeciesData <- names(speciesData)
namesSpeciesDataShort <- gsub(" ", "", gsub("[[:punct:]]", "", datasetNames))

# Define sample size - hard coded for now
sampSize <- 1

print(paste0("Sample size used for prediction is ", sampSize))

rm("workflowList")

# load the control parameters
readRDS(paste0(folderName,"/controlPars.RDS")) %>% 
  list2env(envir = .GlobalEnv)

# Find prediction dataset
predictionDataset <- focalTaxa$predictionDataset[focalTaxa$taxa == gsub('[[:digit:]]+', '', focalGroup)]
predictionDatasetShort <- gsub(" ", "", gsub("[[:punct:]]", "", predictionDataset))
predictionDatasetShort <- predictionDatasetShort[1]

# Make sure prediction dataset is in the prediction data. If not, need to choose a new one.
if(!predictionDatasetShort %in% datasetNames){
  predictionDatasetShort <-  namesSpeciesDataShort[!predictionDatasetShort %in% namesSpeciesDataShort][1]
}

covariatesSquared <- TRUE
# Import local functions
sapply(list.files("functions", pattern = "\\.R$", full.names = TRUE), source)

# Ensure that dateAccessed is specified
if (!exists("dateAccessed")) stop("You need to specify the variable dateAccessed")

# Specify folders for storage of all run data
tempFolderName <- paste0(folderName, "/temp")
modelFolderName <- paste0(folderName, "/modelOutputs")

# Import model objects datasets
regionGeometry <- readRDS(paste0(folderName, "/regionGeometry.RDS"))
focalCovariates <- read.csv(paste0(folderName, "/focalCovariates.csv"), header= T)
environmentalDataList <- rast(paste0(tempFolderName, "/environmentalDataImported.tiff"))

# Reporject crs
crs <- '+proj=utm +zone=33 +datum=WGS84 +units=km +no_defs'
environmentalDataList <- project(environmentalDataList, crs)
regionGeometry <- st_transform(regionGeometry, crs)

myMesh <- lapply(myMesh, FUN = function(x) {x/1000})
mesh <- meshTest(myMesh, regionGeometry, crs = crs, print = TRUE)

# Get the crs used in preparing the data for the models
projCRS <- modelCRS <-  crs

# Import fitted models
models <- lapply(paste0(modelFolderName, "/", focalGroup), function(x){
  try(list.files(x, pattern = paste0("richnessModel.qs"), recursive = TRUE, full.names = TRUE))
})

print(paste0("Model for ", focalGroup, " loaded. File name ", models[[1]][1]))

###-----------------###
### 2. Prep objects ###
###-----------------##

inla.setOption(inla.call = "inla")
Sys.setenv(TZ = "UTC")

# Define prediction raster grid
types <- sapply(seq(nlyr(environmentalDataList)), function(x){
  environmentalDataList[[x]][,1] %>% unlist %>% class
})

origCovs <- names(environmentalDataList)

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
# Define model outputs based on modelRun
modelOutputs <- "Bias"

###-------------------------###
### 3. Generate predictions ###
###-------------------------###

for(mod in seq_along(models)){
  # identify focal taxon
  focalTaxon <- strsplit(models[mod][[1]], split = "/")[[1]][[4]]
  # import model
  model <- qread(models[mod][[1]])
  # identify species in model
  speciesIn <- model$species$speciesIn %>% unlist %>% unique
  # indentify if bias field
  biasField <- !is.null(model$summary.random$mergedDatasetPO_biasField)
  # identify covariates used in model 
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
  
  # identify bias covs
  if(!is.null(model$spatCovs$biasFormula)){
    biasCovs <- covs[covs %in% attributes(terms(model$spatCovs$biasFormula))$term.labels]
    # remove bias covariates from list
    covs <- covs[!covs %in% attributes(terms(model$spatCovs$biasFormula))$term.labels]
  }
  
  # Obtain prediction data
  predData <- predGrid %>% 
    dplyr::select(all_of(biasCovs)) %>% 
    as.data.frame(na.rm = FALSE) %>% 
    replicate(length(speciesIn) +1  , ., simplify = FALSE) %>% 
    reduce(cbind) %>% 
    bind_cols(geometries) %>% 
    #filter(rowSums(is.na(.)) != (ncol(.)-1))%>%
    st_sf()%>%
    na.omit()%>%
    st_transform(., modelCRS)#%>% # transform the prediction data to the units used in model runs
  # filter(rowSums(is.na(.)) != (ncol(.)-1)) # Now we take all the rows with sum of NAs equal to the number of columns of the dataframe minus the 
  
  # Transform predGrid
  transformedPredRast <- project(predRast, modelCRS)
  
  # update names
  names(predData) <- c(biasCovs,
                       paste(rep(speciesIn, each = length(biasCovs)), 
                             biasCovs, sep = "_"), "geometry")
  
  
  # Generate & convert & Save model/predicts (currently for all species grouped)
  ret <- split(1:nrow(predData), seq(1, ceiling(nrow(predData) / 10000)))
  
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
    dplyr::select("mean", "sd")
  
  if (biasField) {
    cat("Bias field included in model, calculating bias predictions.\n")
    spPredField <- lapply(pred, function(x){
      res <-   x[[1]][[2]]#%>%
      # dplyr::filter(speciesName == 1)
    })%>%
      do.call("rbind", .)%>%
      dplyr::select("mean", "sd")
    spPredGroup <- rbind(spPred, spPredField)
    spPred <- spPredGroup %>%
      group_by(geometry) %>%
      summarise(mean = mean(mean),
                sd = sqrt(mean(sd^2)))
  }
  
  head(spPred)
  
  spPred <- rasterize(spPred, transformedPredRast , names(spPred)[!names(spPred) %in% names(predData)])
  # define species directory & save prediction
  path <- paste(c(strsplit(models[mod][[1]], "/")[[1]][1:4], "Bias"), collapse = "/")  # path
  # make_path(path)
  if(!file.exists(path)){
    dir.create(path)
  }
  saveRDS(spPred, file.path(path, paste0(type, ".rds")))
  
}

