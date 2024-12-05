#### MODEL Predictions ####

###----------------------###
### 0. Bash preparation ####
###----------------------###
args <- commandArgs(trailingOnly = TRUE)

start <- Sys.time()

i <- as.numeric(args[1])
covariatesSquared <- TRUE
.libPaths(c("/cluster/projects/nn11017k/R"))

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

interestedGroup <- "amphibiansReptiles"
load(paste0(interestedGroup,"workflowWorkspace.RData"))
sampSize <- 0.25

print(i)


library(intSDM)
library(rgbif)
library(terra)
library(dplyr)

# Load the workspace witht the workflowList object
#load("workflowWorkspace.RData")

focalGroup <- names(workflowList)[i]
workflow <- workflowList[[focalGroup]]
print(focalGroup)

rm("workflowList")


# load the control parameters
readRDS(paste0(folderName,"/controlPars.RDS")) %>% 
  list2env(envir = .GlobalEnv)

# Find prediction dataset
predictionDataset <- focalTaxa$predictionDataset[focalTaxa$taxa == gsub('[[:digit:]]+', '', focalGroup)]
predictionDatasetShort <- gsub(" ", "", gsub("[[:punct:]]", "", predictionDataset))
predictionDatasetShort <- predictionDatasetShort[1]
# Choose one of the datasets within each segmentation as the prediction data
# I prefer to choose the one with the smallest data points
datasetNames <- workflow$.__enclos_env__$private$datasetName
datasetNames
namesSpeciesData <- names(speciesData)
namesSpeciesDataShort <- gsub(" ", "", gsub("[[:punct:]]", "", datasetNames))


if(!predictionDatasetShort %in% datasetNames){
  predictionDatasetShort <-  namesSpeciesDataShort[!predictionDatasetShort %in% namesSpeciesDataShort][1]
}


dateAccessed <- "2024-12-04" 
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

# Ensure that modelRun is specified
if (!exists("modelRun")) stop("You need to specify the variable modelRun")


# Prediction resolution in stated in the units used in preparing the data
# That is metres
predRes <- 500

# Import model objects datasets
regionGeometry <- readRDS(paste0(folderName, "/regionGeometry.RDS"))
focalCovariates <- read.csv(paste0(folderName, "/focalCovariates.csv"), header= T)
environmentalDataList <- rast(paste0(tempFolderName, "/environmentalDataImported.tiff"))
myMesh <- list(cutoff = 176, max.edge=c(26385, 175903), offset= c(1760, 18))
mesh <- meshTest(myMesh, regionGeometry, crs = crs)
#load("data/meshForProject.RData")
#mesh <- meshToUse
if(covariatesSquared){
  environmentalDataList$summer_precipitation_squared <- (environmentalDataList$summer_precipitation)^2
  environmentalDataList$summer_temperature_squared <- (environmentalDataList$summer_temperature)^2
}

levels(environmentalDataList$land_cover_corine)[[1]][,2][is.na(levels(environmentalDataList$land_cover_corine)[[1]][,2])] <- "Water bodies"
levels(environmentalDataList$land_cover_corine)[[1]][,2][28] <- "Moors and heathland"
landCover <- environmentalDataList$land_cover_corine 
sort(unique(values(environmentalDataList$land_cover_corine)[,1])) 
values(environmentalDataList$land_cover_corine)[,1][is.nan(values(environmentalDataList$land_cover_corine)[,1])] <- 48
levels(environmentalDataList$land_cover_corine) <- levels(landCover)

# Get the crs used in preparing the data for the models
projCRS <- "+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# Get the crs used in fitting the models
modelCRS <- "+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=km +no_defs"

# Import fitted models
models <- lapply(paste0(modelFolderName, "/", interestedGroup, i), function(x){
  try(list.files(x, pattern = paste0("richnessModel.rds"), recursive = TRUE, full.names = TRUE))
})

###-----------------###
### 2. Prep objects ###
###-----------------##

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
modelOutputs <- if(modelRun == "richness") {
  c('Bias')
} else if (modelRun == "redListRichness") {
  'Richness' 
} else {
  c('Predictions', 'Bias', 'Covs', 'Spatial')
}

###-------------------------###
### 3. Generate predictions ###
###-------------------------###

for(i in seq_along(models)){
  # identify focal taxon
  focalTaxon <- strsplit(models[i][[1]], split = "/")[[1]][[4]]
  # import model
  model <- readRDS(models[i][[1]])
  # identify species in model
  speciesIn <- model$species$speciesIn %>% unlist %>% unique
  # indentify if bias field
  biasField <- !is.null(model$summary.random$sharedBias_biasField) | !is.null(model$spatCovs$biasFormula)
  # identify covariates used in model 
  covs <- model$spatCovs$name
  # identify categorical covariate factors 
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
  
  # get complete list of covariate columns from which to predict
  covs <- unique(c(covs, catCovs))
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
  for(type in modelOutputs){
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
      dplyr::select("mean", "sd", "q0.025", "q0.5", "q0.975", "median")
    
    head(spPred)
    
    spPred <- rasterize(spPred, transformedPredRast , names(spPred)[!names(spPred) %in% names(predData)])
    # define species directory & save prediction
    path <- paste(c(strsplit(models[i][[1]], "/")[[1]][1:4], "Bias"), collapse = "/")  # path
    # make_path(path)
    if(!file.exists(path)){
      dir.create(path)
    }
    saveRDS(spPred, file.path(path, paste0(type, ".rds")))
    
  }
}


