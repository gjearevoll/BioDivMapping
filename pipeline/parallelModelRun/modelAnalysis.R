
###----------------------------------------###
### 0. Import relevant libraries and data ####
###----------------------------------------###

library(qs, lib.loc = "/cluster/projects/nn11017k/BioDivMapping/R")
library(stringr)


args <- commandArgs(TRUE)

if (length(args) != 0) {
  # Set arguments
  dateAccessed <- args[1]
  # Set the working directory
}
#dateAccessed <- "2026-02-09"

# Set output folders
folderName <- paste0("data/run_", dateAccessed)
modelFolderName <- paste0(folderName, "/modelOutputs")

# Get taxa surveyed
focalTaxa <- read.csv(paste0(folderName, "/focalTaxa.csv"), header = T)
taxaToCompile <- unique(focalTaxa$taxa)

# Get list of valid models
modelNameList <- list.files(modelFolderName, full.names = TRUE, recursive = TRUE, pattern = "richnessModel.qs")

# Get valid model names
taxaLists <- lapply(taxaToCompile[1], FUN = function(x) {
  modelNameListTaxa <- grep(paste0("/", x), modelNameList, value = TRUE)
  
  # Get predictor species
  predictorSpecies <- focalTaxa$predictorSpecies[focalTaxa$taxa == x]
  
  # Import model for json use
  richnessModel <-  qread(modelNameListTaxa[1])
  
  # Import model
  biasEffects <- list()
  fixedEffects <- list()
  hyperParams <- list()
  responseDataList <- list()
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
    
    # Exclude predictor species if it's not the first model
    if (mod != 1) {modelData <- modelData[names(modelData)[!grepl(predictorSpecies, names(modelData))]]}
    presenceOnlyData <- names(modelData)[grep("mergedDatasetPO", names(modelData))]
    integrationPoints <- modelData[[presenceOnlyData[1]]]$data[is.na(modelData[[presenceOnlyData[1]]]$data$poresp),]
    integrationPoints$pointType <- "integration"
    speciesResponseData <- lapply(names(modelData), FUN = function(x2) {
      ds <- modelData[[x2]]$data
      if ("poresp" %in% colnames(ds)) {
        ds <- ds[!is.na(ds$poresp),]
        chooseColumns <- colnames(ds)[!(colnames(ds) %in% c("speciesName","._dataset_index_var_.","speciesSpatialGroup",
                                                            "BRU_aggregate","BRU_point_weights",".block","weight"))]
        ds <- ds[,chooseColumns]
      } else {
        chooseColumns <- colnames(ds)[!(colnames(ds) %in% c("occurrenceStatus","speciesName","._dataset_index_var_.","speciesSpatialGroup",
                                                            "BRU_aggregate","BRU_point_weights",".block"))]
        ds <- ds[,chooseColumns]
        ds$poresp <- 1
      }
      ds$pointType <- "occurrence"
      return(ds)
    })
    responseData <- do.call(rbind, speciesResponseData)
    responesData2 <- rbind(responseData, integrationPoints[,colnames(responseData)])
    
    responseDataList[[mod]] <- responseData
    
  }
  speciesResponseDataTotal <- do.call(rbind, responseDataList)
  
  hyperDF <- do.call(rbind, hyperParams)
  biasDF <- do.call(rbind, biasEffects)
  fixedDF <- do.call(rbind, fixedEffects)
  fixedDF$species <- sub("^(([^_]*_){1}[^_]*).*", "\\1", row.names(fixedDF))
  fixedDF$covs <- str_replace(str_replace(row.names(fixedDF), '(.*?)_(.*?)', '\\2'), '(.*?)_(.*?)', '\\2')
  row.names(fixedDF) <- NULL
  effectsList <- list(fixedEffects = fixedDF, biasEffects = biasDF, hyperEffects = hyperDF)
  
  
  # Get model links 
  modelLinks <- setNames(sapply(richnessModel$.args$control.family, function(x) x$link), richnessModel$source)
  modelLinksUnique <- modelLinks[names(modelLinks)[!duplicated(names(modelLinks))]]
  
  # Isolate model formulas
  modelFormulasForUse <-  names(richnessModel$bru_info$lhoods)[grep(predictorSpecies, names(richnessModel$bru_info$lhoods))]
  modelFormulas <-  sapply(richnessModel$bru_info$lhoods[modelFormulasForUse],
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
      modelFramework = 'Point process',
      modelType = 'Integrated species distribution model',
      modelMethod = 'Integrated nested Laplace approximation',
      statisticalMethodology = 'Bayesian',
      RVersion = R.version.string,
      packages = list(packages = c('INLA', 'inlabru','PointedSDMs', 'intSDM'),
                      packageCitations = c(INLA = unlist(citation('INLA')$doi)[1],
                                           inlabru = unlist(citation('inlabru')$doi)[1],
                                           PointedSDMs = unlist(citation('PointedSDMs')$doi)[1],
                                           intSDM = unlist(citation('intSDM')$doi)[1]),
                      packageVersions = c(INLA = packageVersion('INLA'),
                                          inlabru = packageVersion('inlabru'),
                                          PointedSDMs = packageVersion('PointedSDMs'),
                                          intSDM = packageVersion('intSDM')))
      
    )
    ,
    #Model outputs
    modelDefinition = list(
      modelLink = modelLinksUnique,
      modelFormulas = modelFormulas
    )
  )
  
  returnedData <- list(effects = effectsList, modelData = speciesResponseDataTotal, json = json_taxa)
  return(returnedData)
  
})
names(taxaLists) <- taxaToCompile[1]

saveRDS(taxaLists, paste0(modelFolderName, "/covAnalysis.RDS"))
