

#### MODEL Predictions ####

###----------------------###
### 0. Bash preparation ####
###----------------------###

args <- commandArgs(TRUE)

# THis should only run if the script is being run from the command line
if (length(args) > 2) {
  # Set arguments
  dateAccessed <- args[1]
  predRes <- args[2]
  modelRun <- args[3]
  # Set the working directory
  setwd("~/BioDivMapping")
}

# You can run this from the command line using for example
# Rscript filePath/speciesModelRuns.R 2024-02-08 allSPecies

dateAccessed <- "2024-10-24"

###-----------------###
### 1. Import data ####
###-----------------###

print("Preparing data for model prediction.")

library(PointedSDMs)
library(dplyr)
library(purrr)
library(terra)
library(tidyterra)
library(stringr)
library(intSDM)

predRes <- 1000
# Import local functions
sapply(list.files("functions", pattern = "\\.R$", full.names = TRUE), source)

# Ensure that dateAccessed is specified
if (!exists("dateAccessed")) stop("You need to specify the variable dateAccessed")

# Specify folders for storage of all run data
folderName <- paste0("data/run_", dateAccessed)
tempFolderName <- paste0(folderName, "/temp")

# model output folder
modelFolderName <- paste0(folderName, "/modelOutputs")

# import project control parameters into the environment
readRDS(paste0(folderName,"/controlPars.RDS")) %>% 
  list2env(envir = .GlobalEnv)

# # Ensure that modelRun is specified
# if (!exists("modelRun")) stop("You need to specify the variable modelRun")

# # Redefine modelRun after controlPars import using args if necessary
# if (length(args) != 0) {
#   modelRun <- args[2]
# }

# Import model objects datasets
regionGeometry <- readRDS(paste0(folderName, "/regionGeometry.RDS"))
# extentCoords <- c(174567.2, 6847988, 202197.8, 6878097) |>
#   setNames(c("west", "south", "east", "north"))
# regionGeometry <- defineRegion(level = "box", extentCoords = extentCoords)
focalTaxa <- read.csv(paste0(folderName, "/focalTaxa.csv"), header= T)
focalCovariates <- read.csv(paste0(folderName, "/focalCovariates.csv"), header= T)
environmentalDataList <- rast(paste0(tempFolderName, "/environmentalDataImported.tiff"))
environmentalDataList <- lapply(environmentalDataList, FUN = function(x) {
  crop(x, st_transform(regionGeometry, st_crs(environmentalDataList))) 
  })|>  
  rast()

levels(environmentalDataList$land_cover_corine)[[1]][,2][is.na(levels(environmentalDataList$land_cover_corine)[[1]][,2])] <- "Water bodies"
levels(environmentalDataList$land_cover_corine)[[1]][,2][28] <- "Moors and heathland"
landCover <- environmentalDataList$land_cover_corine 

values(environmentalDataList$land_cover_corine)[,1][is.nan(values(environmentalDataList$land_cover_corine)[,1])] <- 48
levels(environmentalDataList$land_cover_corine) <- levels(landCover)

# Import fitted models
models <- list.files(folderName, pattern = "Model.rds", recursive = TRUE, full.names = TRUE)

###-----------------###
### 2. Prep objects ###
###-----------------###
# res = 2
# original covariate names
origCovs <- names(environmentalDataList)
# which factor
types <- sapply(seq(nlyr(environmentalDataList)), function(x){
  environmentalDataList[[x]][1] %>% unlist %>% class
})
# define template prediction raster 
predRast <- terra::rast(ext(environmentalDataList), res = c(predRes, predRes), crs = crs(st_crs(crs)$wkt))
# Define prediction raster grid at target resolution
if(any(types == "factor")){
  predGridfactor <- environmentalDataList[[types == "factor"]] 
  # Define prediction raster grid for continuous covs (interpolate when predRes <= res, else average) 
  predGrid <- terra::project(environmentalDataList[[types != "factor"]], predRast, 
                             method = if(predRes <= res) "bilinear" else "average") 
  # Define prediction raster grid catagorical covs
  factorRasters <- terra::project(predGridfactor, predRast, method = "mode") 
  levels(factorRasters) <- levels(predGridfactor) # reassign levels 
  # Combine binary rasters into a SpatRaster object
  predGrid <- c(predGrid, factorRasters) 
  #names(predGrid) <- origCovs
} else {
  # Define prediction raster grid (interpolate when predRes <= res, else average) 
  predGrid <- terra::project(environmentalDataList, predRast, method = if(predRes <= res) "bilinear" else "average") 
}

# define geometries to combine with prediction 
geometries <- xyFromCell(predGrid, seq(ncell(predGrid))) %>% 
  as.data.frame() %>% 
  st_as_sf(coords = c("x", "y"), crs = crs) 

# Define model outputs based on modelRun
modelOutputs <- if(modelRun == "richness") {
  c('Richness', 'Bias')
} else if (modelRun == "redListRichness") {
  'Richness' 
} else {
  c(#'Predictions', 
    #'Bias', 
    'Covs')#,
  # 'Spatial')
}


mesh <- myMesh

###-------------------------###
### 3. Generate predictions ###
###-------------------------###

timeStart <- Sys.time()

for(i in seq_along(models)[39:46]){
  # identify focal taxon
  focalTaxon <- strsplit(models[i], "/")[[1]][[4]]
  
  predictionInterceptDataset <- gsub(" ", "", focalTaxa$predictionDataset[focalTaxa$taxa == gsub('[[:digit:]]+', '', focalTaxon)])
  focalSampleSize <- focalTaxa$sampleSize[focalTaxa$taxa == gsub('[[:digit:]]+', '', focalTaxon)]
  # import model
  model <- readRDS(models[i])
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
  
  # prep prediction data 
  predData <- predGrid %>% 
    dplyr::select(all_of(covs)) %>% 
    as.data.frame(na.rm = FALSE) %>% 
    replicate(length(speciesIn) + 1, ., simplify = FALSE) %>% 
    reduce(cbind) %>% 
    bind_cols(geometries) %>% 
    st_sf() %>% 
    na.omit()
  # update names
  names(predData) <- c(covs,
                       paste(rep(speciesIn, each = length(covs)), 
                             covs, sep = "_"), "geometry")
  # identify bias covs
  if(!is.null(model$spatCovs$biasFormula)){
    biasCovs <- covs[covs %in% attributes(terms(model$spatCovs$biasFormula))$term.labels]
    # remove bias covariates from list
    covs <- covs[!covs %in% attributes(terms(model$spatCovs$biasFormula))$term.labels]
  }
  # Generate & convert & Save model/predicts (currently for all species grouped)
  for(type in modelOutputs){
    ret <- split(1:nrow(predData), seq(1, ceiling(nrow(predData) / 5000)))
    if(type == "Predictions") {
      pred <- predict(model, data = predData, predictor = TRUE, mesh = mesh)
    } else if(type == "Bias" && biasField) {
      pred <- predict(model, data = predData, bias = TRUE, mesh = mesh)
    } else if(type== "Covs") {
      pred <- predict(model, data = predData, covariates = covs, mesh = mesh)  # model$spatCovs$name
    } else if(type == "Spatial") {
      pred <- predict(model, data = predData, spatial = TRUE, mesh = mesh)
    } else if(type == "Richness") {
      pred <- lapply(ret, function(x){
        richnessEst <- intSDM:::obtainRichness(model, predictionData = predData[x, ], 
                                               predictionIntercept = predictionInterceptDataset, sampleSize = focalSampleSize)
        return(richnessEst$Probabilities)
      })
    } 

    # species
    
    if (type == "Richness") {
    species <- names(pred[[1]])
    for(sp in species){
      print(sp)
      spPred <- lapply(pred, function(x){
        res <-   x[[sp]]
      })%>%
        do.call("rbind", .)%>%
        select("mean", "sd", "q0.025", "q0.5", "q0.975", "median")
      spPred <- rasterize(spPred, predGrid, names(spPred)[!names(spPred) %in% names(predData)])
      # define species directory & save prediction
      path <- paste(c(strsplit(models[i][[1]], "/")[[1]][1:4], sp), collapse = "/")  # path
      # make_path(path)
      if(!file.exists(path)){
        dir.create(path)
      }
      saveRDS(spPred, file.path(path, paste0(type, ".rds")))
    } else if (type == "Bias") {
      spPred <- lapply(pred, function(x){
        res <-   x[[sp]]
      })%>%
        do.call("rbind", .)%>%
        select("mean")
      spPred <- rasterize(spPred, predGrid, names(spPred)[!names(spPred) %in% names(predData)])
      path <- paste(c(strsplit(models[i][[1]], "/")[[1]][1:4]), collapse = "/")
      saveRDS(spPred, file.path(path, paste0("Bias", ".rds")))
    }
    }
  }
}


#
# print(object.size(predData),units="Kb")
# print(object.size(pred),units="Kb")
# 100: 39, 369
# 50: 111, 1038
# 25: 389, 3637
# 12: 1648, 15386
# 6: 6468, 60368
# 3: 25804, 240840

