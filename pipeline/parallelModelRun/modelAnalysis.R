
###----------------------------------------###
### 0. Import relevant libraries and data ####
###----------------------------------------###

library(qs, lib.loc = "/cluster/projects/nn11017k/BioDivMapping/R")
library(stringr)
library(sf)

args <- commandArgs(TRUE)

if (length(args) != 0) {
  # Set arguments
  dateAccessed <- args[1]
  # Set the working directory
}
#dateAccessed <- "2026-04-18"

# Set output folders
folderName <- paste0("data/run_", dateAccessed)
modelFolderName <- paste0(folderName, "/modelOutputs")


# import project control parameters into the environment
readRDS(paste0(folderName,"/controlPars.RDS")) %>% 
  list2env(envir = .GlobalEnv)


# Get taxa surveyed
focalTaxa <- read.csv(paste0(folderName, "/focalTaxa.csv"), header = T)
taxaToCompile <- unique(focalTaxa$taxa)

# Load segment list
segmentList <- readRDS(file.path(folderName, "segmentList.RDS"))

# Get list of valid models and other files
modelNameList <- list.files(modelFolderName, full.names = TRUE, recursive = TRUE, pattern = "richnessModel.qs")
timeTakenFiles <- list.files(modelFolderName, "timeTaken", recursive = T, full.names = T)


###--------------------------###
### 1. Start model analyses ####
###--------------------------###


# Get valid model names
taxaLists <- lapply(taxaToCompile[1], FUN = function(x) {
  modelNameListTaxa <- grep(paste0("/", x), modelNameList, value = TRUE)
  
  # modelsRun
  relevantWorkflows <- grep(x, segmentList, value = T)
  
  # Get predictor species
  predictorSpecies <- focalTaxa$predictorSpecies[grep(x, focalTaxa$taxa)]
  
  # Start lists for inserting mdoel info
  biasEffects <- list()
  fixedEffects <- list()
  hyperParams <- list()
  responseDataList <- list()
  
  # Commence model import loops
  for (mod in seq_along(modelNameListTaxa)) {
    print(modelNameListTaxa[mod])
    importedModel <- qread(modelNameListTaxa[mod])
    
    # Get fixed effects summary
    summaryFixed <- importedModel$summary.fixed[,1:2]
    fixedEffects[[mod]] <- summaryFixed[!grepl("intercept", row.names(summaryFixed)),]
    biasEffects[[mod]] <- importedModel$summary.random$Bias__Effects__Comps[,1:3]
    hyperParams[[mod]] <- importedModel$summary.hyperpar[,1:2]
    
    # Compile model data
    modelData <- importedModel$bru_info$lhoods
    
    # Exclude predictor species if it's not the first model. If it is the first model, create the integration points
    if (mod != 1) {
      modelData <- modelData[names(modelData)[!grepl(predictorSpecies, names(modelData))]]
    } 
    presenceOnlyData <- names(modelData)[grep("mergedDatasetPO", names(modelData))]
    
    # Create integration points data frame if it's the first model
    if (mod == 1) {
      integrationPoints <- modelData[[presenceOnlyData[1]]]$data[is.na(modelData[[presenceOnlyData[1]]]$data$poresp),]
      integrationPoints$dataset <- integrationPoints$speciesNameINDEX_VAR <- integrationPoints$pointType <- "integration"
      integrationPoints$presenceOnly <- NA
      integrationPoints$occurrenceStatus <- NA
    }
    
    # Import and standardise response data
    speciesResponseData <- lapply(names(modelData), FUN = function(x2) {
      ds <- modelData[[x2]]$data
      dsName <- gsub(paste0("_",unique(ds$speciesNameINDEX_VAR)), "", x2)
      dsName <- gsub("_occurrenceStatus", "", gsub("_geometry", "", dsName))
      
      # If PO data, 
      if ("poresp" %in% colnames(ds)) {
        ds <- ds[!is.na(ds$poresp),]
        chooseColumns <- colnames(ds)[!(colnames(ds) %in% c("poresp", "speciesName","._dataset_index_var_.","speciesSpatialGroup",
                                                            "BRU_aggregate","BRU_point_weights",".block","weight"))]
        ds <- ds[,chooseColumns]
        ds$occurrenceStatus <- 1
        ds$presenceOnly <- TRUE
      } else {
        chooseColumns <- colnames(ds)[!(colnames(ds) %in% c("speciesName","._dataset_index_var_.","speciesSpatialGroup",
                                                            "BRU_aggregate","BRU_point_weights",".block"))]
        ds <- ds[,chooseColumns]
        ds$presenceOnly <- FALSE
      }
      ds$pointType <- "occurrence"
      ds$dataset <- dsName
      return(ds)
    })
    
    # Compile response data
    responseData <- do.call(rbind, speciesResponseData)
    
    responseDataList[[mod]] <- responseData
    
  }
  speciesResponseDataTotal <- rbind(integrationPoints[,colnames(responseDataList[[1]])], do.call(rbind, responseDataList))
  
  # Compile covariate effects
  hyperDF <- do.call(rbind, hyperParams)
  biasDF <- do.call(rbind, biasEffects)
  fixedDF <- do.call(rbind, fixedEffects)
  fixedDF$species <- sub("^(([^_]*_){1}[^_]*).*", "\\1", row.names(fixedDF))
  fixedDF$covs <- str_replace(str_replace(row.names(fixedDF), '(.*?)_(.*?)', '\\2'), '(.*?)_(.*?)', '\\2')
  row.names(fixedDF) <- NULL
  effectsList <- list(fixedEffects = fixedDF, biasEffects = biasDF, hyperEffects = hyperDF)
  
  # Get model links 
  modelLinks <- setNames(sapply(importedModel$.args$control.family, function(x) x$link), importedModel$source)
  modelLinksUnique <- modelLinks[names(modelLinks)[!duplicated(names(modelLinks))]]
  
  # Isolate model formulas
  modelFormulasForUse <-  names(importedModel$bru_info$lhoods)[grep(predictorSpecies, names(importedModel$bru_info$lhoods))]
  modelFormulas <-  sapply(importedModel$bru_info$lhoods[modelFormulasForUse],
                           FUN = function(x) {update.formula(x$formula,
                                                             new = formula(paste('. ~',
                                                                                 paste0(x$used$effect,
                                                                                        collapse = ' + '))))})
  modelFormulas <- lapply(modelFormulas, FUN = function(x3) {gsub(paste0(predictorSpecies,"_"), "", x3[3])})
  names(modelFormulas) <- sub("_.*", "", names(modelFormulas))
  
  # define json content
  json_taxa <- list(
    
    #Information about the model
    modelInformation = list(
      model_framework = 'Point process',
      model_type = 'Integrated species distribution model',
      model_method = 'Integrated nested Laplace approximation',
      statistical_methodology = 'Bayesian',
      programming_language = 'R',
      language_version = R.version.string,
      packages = list(packages = c('INLA', 'inlabru','PointedSDMs', 'intSDM'),
                      packageCitations = c(INLA = unlist(citation('INLA')$doi)[1],
                                           inlabru = unlist(citation('inlabru')$doi)[1],
                                           PointedSDMs = unlist(citation('PointedSDMs')$doi)[1],
                                           intSDM = unlist(citation('intSDM')$doi)[1]),
                      packageVersions = c(INLA = as.character(packageVersion('INLA')),
                                          inlabru = as.character(packageVersion('inlabru')),
                                          PointedSDMs = as.character(packageVersion('PointedSDMs')),
                                          intSDM = as.character(packageVersion('intSDM'))))
      
    )
    ,
    #Model outputs
    modelDefinition = list(
      modelLink = modelLinksUnique,
      modelFormulas = modelFormulas
    )
  )
  
  # Produce model diagnostics
  
  timeTakenRelevant <- unlist(lapply(timeTakenFiles[grep(x ,timeTakenFiles)], FUN = function(z) {
    ttFile <- readRDS(z)
    if (units(ttFile) == "days"){
      return(as.double(ttFile) * 24)
    } else {
      return(as.double(ttFile))}
  }
  ))
  
  # # Construct metadata for modeldiagnostics metadata
  # metadataSummaries <- st_drop_geometry(speciesResponseDataTotal) %>%
  #   filter(pointType != "integration")
  
  json_model_diagnostics <- list(
    n_models_run = length(modelNameListTaxa),
    n_models_attempted = length(relevantWorkflows),
    n_species_run = length(unique(effectsList$fixedEffects$species)),
    average_time_run = mean(timeTakenRelevant),
    n_integration_points = nrow(speciesResponseDataTotal[speciesResponseDataTotal$pointType == "integration",]),
    n_species_points = nrow(speciesResponseDataTotal[speciesResponseDataTotal$pointType != "integration",]),
    n_species = length(unique(speciesResponseDataTotal$speciesNameINDEX_VAR)),
    n_datasets = length(unique(speciesResponseDataTotal$dataset)) - 1,
    n_segments = length(modelNameListTaxa),
    file_list = list(file_names = modelNameListTaxa)
  )
  
  
  returnedData <- list(effects = effectsList, modelData = speciesResponseDataTotal, json = json_taxa, diagnostics = json_model_diagnostics)
  return(returnedData)
  
})
names(taxaLists) <- taxaToCompile[1]

# Compile and saved data objects

# Effects lists first
effectsLists <- lapply(taxaLists, FUN = function(x) x$effects) |> setNames(taxaToCompile[1])
saveRDS(effectsLists, paste0(modelFolderName, "/covAnalysis.RDS"))

# Model data object
modelDataFull <- do.call(rbind, lapply(taxaLists, FUN = function(x) x$modelData))
write_sf(modelDataFull, file.path(extFolderName, "speciesDataModelled.gpkg"), append = T)

# Now ssave metadata - read existing json
json_ls <- jsonlite::fromJSON(file.path(extFolderName, "metadata.json"))

# Now model diagnostics
json_ls$step_3a <- lapply(taxaLists, FUN = function(x) x$diagnostics) |> setNames(taxaToCompile[1])
json_ls$step_3a$file <- file.path(extFolderName, "speciesDataModelled.gpkg")

# Model description
json_ls$step_3b <- lapply(taxaLists, FUN = function(x) x$json) |> setNames(taxaToCompile[1])

# And now write json
# write json
jsonlite:::write_json(json_ls,
                      file.path(extFolderName, "metadata.json"), 
                      pretty = TRUE)
