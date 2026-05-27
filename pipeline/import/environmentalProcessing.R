#### ENVIRONMENTAL DATA PROCESSINg ####


library(raster)
library(terra)
library(sf)
library(fasterize)
library(dplyr)
library(digest)  # create hash of raster CRS and projection for saving

# Import local functions
sapply(list.files("functions", full.names = TRUE), source)


###----------------------###
### 0. Bash preparation ####
###----------------------###

args <- commandArgs(TRUE)

# THis should only run if the script is being run from the command line
if (length(args) != 0 & !exists("dateAccessed")) {
  # Set arguments
  dateAccessed <- args[1]
  # Set the working directory
  setwd("~/BioDivMapping")
}


###-----------------###
### 1. Preparation ####
###-----------------###

# if it is not already, define dateAccessed
if (!exists("dateAccessed")) {
  stop("Please define a run date for the model first.")
}
# define repo folder names
folderName <- paste0("data/run_", dateAccessed)
tempFolderName <- paste0(folderName, "/temp")

# import project control parameters into the environment
readRDS(paste0(folderName,"/controlPars.RDS")) %>% 
  list2env(envir = .GlobalEnv)

# Import focal covariates
if(file.exists(paste0(folderName, "/focalCovariates.csv"))){
  parameters <- read.csv(paste0(folderName, "/focalCovariates.csv"), header = T)
} else {
  stop("Please source initialiseRepository.R first.")
}

# Import focal taxa
if(file.exists(paste0(folderName, "/focalTaxa.csv"))){
  focalTaxa <- read.csv( paste0(folderName, "/focalTaxa.csv"), header = T)
} else {
  stop("Please source initialiseRepository.R first.")
}

# import regionGeometry list
if(file.exists(paste0(folderName, "/regionGeometry.RDS"))){
  regionGeometry <- readRDS(paste0(folderName, "/regionGeometry.RDS"))
} else {
  stop("Please source defineRegionGeometry.R first.")
}

# import baseRaster
baseRaster <- rast(file.path(folderName, "baseRaster.tiff"))

# import downloaded rasters
json_ls <- jsonlite::fromJSON(file.path(extFolderName, "metadata.json"))
parameterList <- lapply(json_ls$step_2a, function(cov) {
  rast(cov$file)
})


###--------------------------------------###
### 2. reclassify categorical rasters    ####
###--------------------------------------###

# Track which parameters were reclassified (for JSON metadata)
reclass_log <- list()

cat_params_present <- parameters$parameters[
  parameters$categorical & parameters$parameters %in% names(parameterList)
]

for (par in cat_params_present) {
  
  reclass_path <- parameters$reclassFile[parameters$parameters == par]
  
  # Skip if no reclassification file specified
  if (is.na(reclass_path) || reclass_path == "") {
    reclass_log[[par]] <- list(applied = FALSE, reclassFile = NULL)
    next
  }
  
  if (!file.exists(reclass_path)) {
    warning(sprintf("Reclassification file for '%s' not found at '%s'. Skipping.", par, reclass_path))
    reclass_log[[par]] <- list(applied = FALSE, reclassFile = reclass_path, error = "file not found")
    next
  }
  
  reclass_df          <- read.csv(reclass_path, header = TRUE)
  parameterList[[par]] <- reclassRasterCats(parameterList[[par]], reclass_df[, 1], reclass_df[, 2])
  reclass_log[[par]]  <- list(applied = TRUE, reclassFile = reclass_path)
  message(sprintf("Reclassified '%s' using '%s'.", par, reclass_path))
}

###--------------------------------------###
### 3. Expansion of categorical rasters ####
###--------------------------------------###

contList <- list()
catParams <- parameters$parameters[parameters$categorical]
for (par in catParams) {
  focalCatParameter <- parameterList[[par]]
  levelTable <- levels(focalCatParameter)[[1]]
  allCats <- unique(levelTable[, 2])
  if (par == "land_cover_corine") {
    allCats <- allCats[(!allCats %in% c(NA, "Sclerophyllous vegetation"))]
    #allCats <- c("Built up area", "Coniferous forest", "Transitional woodland-shrub", "Moors and heathland")
  }
  catList <- lapply(allCats, FUN = function(cat1) {
    if (par == "kalkinnhold" & cat1 == "no data") {return(NA)}
    catLevels <- levelTable$value[levelTable[, 2] %in% cat1]
    print(paste0("Aggregating: ", cat1))
    catRaster <- ifel(focalCatParameter %in% catLevels, 1, 0)
    contRaster <- terra::project(catRaster, baseRaster, method = "average")
    contRaster
  }) |> setNames(allCats)
  contList[[par]] <- catList
}

fullCatList <- unlist(contList)[!is.na(unlist(contList))]
names(fullCatList) <- gsub(" ", "_", 
                           stringr::str_replace_all(names(fullCatList),
                                                    "[[:punct:]]", "_")) 

parameterListCont <- parameterList[!(names(parameterList) %in% catParams)]
parameterListCont <- c(parameterListCont, fullCatList)
parameterNames <- removeAccents(names(parameterListCont))


###------------------------###
### 4. Data Consolidation ####
###------------------------###

# Crop, match projections and compile raster layers into one object
parametersCropped <- parameterListCont |> 
  lapply(function(x) {
    # Crop each covariate to extent of regionGeometryBuffer
    out <- x
    # Project all rasters to baseRaster and combine
    if(unique(is.factor(x))) {
      # project categorical rasters
      out <- terra::project(out, baseRaster, method = "mode")
      levels(out) <- levels(x)  # reassign levels 
      out
    } else if (nlyr(x) == 1) {
      # project & scale continuous rasters
      ifel(is.na(regionGeometryRast), NA,
           terra::project(out, baseRaster)) |>
        scale()  
    } else {
      projVersion <- ifel(is.na(regionGeometryRast), NA,
                          terra::project(out, baseRaster))
      totalMean <- global( mean(projVersion), "mean", na.rm = TRUE)
      rr <- projVersion - totalMean[,1]
      rms <- global(mean(rr), "rms", na.rm = TRUE)
      rr / rms[,1]
    }
  })

if (!temporal) {
  parametersCropped <- parametersCropped |>  
    rast() |>  # combine raster layers
    setNames(parameterNames)  # assign names
}


###----------------------------###
### 5. Create quadratic terms ####
###----------------------------###

# Check which parameters are needed to make sure we don't take the quadratic of an unwanted term
useParam <- apply(focalTaxa[, colnames(focalTaxa) %in% parameters$parameters], 2, any)
parametersForUse <- names(useParam)[useParam]

quadratics <- parameters[parameters$quadratic & parameters$parameters %in% parametersForUse,]
if (nrow(quadratics) > 0) {
  for(i in seq_along(quadratics$quadratic)) {
    parameter <- quadratics$parameters[i]
    parametersCropped[[paste0(parameter, "_squared")]] <- parametersCropped[[parameter]]^2
  }
}


###--------------------###
### 6. Dataset Upload ####
###--------------------###

# Save both to temp file for model processing and visualisation folder for mapping
if (temporal) {
  parametersCropped <- lapply(parametersCropped, terra::wrap)
  saveRDS(parametersCropped, paste0(tempFolderName,"/environmentalDataImported.RDS"))
} else {
  writeRaster(parametersCropped, paste0(tempFolderName,"/environmentalDataImported.tiff"), overwrite=TRUE)
}


# Create aggregated version for all non-land cover visualisation and reference data
agg <- function(x, fact){
  if(is.factor(x))
    # If the variable is a factor, use the most common result as the average
    terra::aggregate(x, fact, fun = "modal") else 
      terra::aggregate(x, fact)
}

# Aggregate and save raster
if (!temporal) {
  parametersAggregated <- sapp(x = parametersCropped, fun = agg, fact = 2) |>
    crop(baseRaster)
  # writeRaster(parametersAggregated, paste0(folderName,"/environmentalDataImported.tiff"), overwrite=TRUE)
  writeRaster(parametersAggregated, paste0(extFolderName,"/environmentalDataImported.tiff"), overwrite=TRUE)
} else {
  parametersAggregated <- lapply(parametersCropped, FUN = function(x){
    if(unique(is.factor(unwrap(x))))
      # If the variable is a factor, use the most common result as the average
      terra::aggregate(unwrap(x), 2, fun = "modal") |> crop(baseRaster) else 
        terra::aggregate(unwrap(x), 2) |> crop(baseRaster)
  }) 
  parametersAggregated <- lapply(parametersAggregated, terra::wrap)
  # saveRDS(parametersAggregated, paste0(folderName,"/environmentalDataImported.RDS"))
  browser()
  saveRDS(parametersAggregated, paste0(extFolderName,"/environmentalDataImported.RDS"))
}


###--------------------###
### 7. Update JSON    ####
###--------------------###

json_ls <- jsonlite::fromJSON(file.path(extFolderName, "metadata.json"))

# Summarise quadratic terms added
quad_summary <- if (nrow(quadratics) > 0) {
  lapply(quadratics$parameters, function(par) {
    list(source_parameter = par, derived_parameter = paste0(par, "_squared"))
  }) |> setNames(quadratics$parameters)
} else {
  list()
}

# Summarise final parameter set
final_params <- lapply(names(parametersCropped), function(par) {
    # data type
    r_type <- c("integer", "numeric", "factor")[
      which(c(terra::is.int(parametersCropped[[par]]),
              terra::is.num(parametersCropped[[par]]),
              terra::is.factor(parametersCropped[[par]])))[1]]
    list(
      typeof = r_type
    )
  }) |> setNames(names(parametersCropped))

json_ls$step_2b <- list(
  processing_steps = list(
    
    categorical_reclassification = list(
      applied                  = any(sapply(reclass_log, `[[`, "applied")),
      parameters_reclassified  = names(Filter(function(x) x$applied, reclass_log)),
      details                  = reclass_log
    ),
    
    categorical_expansion = list(
      applied            = length(contList) > 0,
      parameters_expanded = names(contList),
      details            = cat_expansion_summary
    ),
    
    continuous_scaling = list(
      method           = "z-score (mean 0, unit variance)",
      applied_to_nlyr1 = "scale()",
      applied_to_nlyrN = "manual standardisation via global mean and RMS"
    ),
    
    quadratic_terms = list(
      applied    = nrow(quadratics) > 0,
      details    = quad_summary
    ),
    
    projection = list(
      target     = "baseRaster",
      resolution = res(baseRaster),
      crs        = crs(baseRaster, proj = TRUE),
      extent     = as.list(ext(baseRaster))
    )
  ),
  
  output = list(
    temporal = temporal,
    format   = "GeoTIFF",
    n_layers = nlyr(parametersCropped),
    parameters = final_params,
    files = list(
      ext = paste0(extFolderName, "/environmentalDataImported.tiff")
    )
  )
)

jsonlite::write_json(
  json_ls,
  file.path(extFolderName, "metadata.json"),
  pretty     = TRUE,
  auto_unbox = TRUE
)
