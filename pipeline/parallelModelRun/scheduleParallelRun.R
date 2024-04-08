nClusters = 5

#cleanFiles<-function(file,newfile){
#  writeLines(iconv(readLines(file,skipNul = TRUE)),newfile)
#}

#cleanFiles("workflowWorkspace.RData", "workflowWorkspace1.RData")

runModel <- function(i, 
                     interestedGroup
){
  
  
  # For some reason i changes to 1 after loading the workspace
  
  print(i)
  #Load packages
  #.libPaths(c("/cluster/projects/nn11017k/R", .libPaths()))
  library(intSDM)
  library(rgbif)
  library(terra)
  library(dplyr)
  
  # Load the workspace witht the workflowList object
  load("workflowWorkspace.RData")

  focalGroup <- names(workflowList)[i]
  workflow <- workflowList[[focalGroup]]
  print(focalGroup)
  

  # load the control parameters
  readRDS(paste0(folderName,"/controlPars.RDS")) %>% 
    list2env(envir = .GlobalEnv)
  
  # Find prediction dataset
  # predictionDataset <- focalTaxa$predictionDataset[focalTaxa$taxa == gsub('[[:digit:]]+', '', focalGroup)]
  # predictionDatasetShort <- gsub(" ", "", gsub("[[:punct:]]", "", predictionDataset))
  
  # Choose one of the datasets within each segmentation as the prediction data
  # I prefer to choose the one with the smallest data points
  datasetNames <- workflow$.__enclos_env__$private$datasetName
  namesSpeciesData <- names(speciesData)
  namesSpeciesDataShort <- gsub(" ", "", gsub("[[:punct:]]", "", namesSpeciesData))
  dataPointsCounts <- lapply(datasetNames, function(x){
     nrow(speciesData[[which(namesSpeciesDataShort %in% x)]])
  })%>%
    do.call("c", .)%>%
   data.frame(dataset = c(datasetNames),
              counts = .)%>%
    dplyr::arrange(counts)
  
  predictionDatasetShort <- dataPointsCounts$dataset[1] 
  
  # Mesh list
  #myMesh <- list(cutoff = 176, max.edge=c(174000, 175903), offset= c(1760, 18))
  

  print(predictionDatasetShort)
  # Add model characteristics (mesh, priors, output)
  workflow$addMesh(cutoff= myMesh$cutoff, max.edge=myMesh$max.edge, offset= myMesh$offset)
  workflow$specifySpatial(prior.range = prior.range,
                          prior.sigma = prior.sigma) #100
  workflow$workflowOutput(modelOutputs)
  if(modelRun == "richness" ){
    workflow$modelOptions(INLA = list(control.inla=list(int.strategy = 'eb', cmin = 0.01), control.compute = list(openmp.strategy="huge"),safe = TRUE), Ipoints = list(method = "direct"), Richness = list(predictionIntercept = predictionDatasetShort))
  }else{
    workflow$modelOptions(INLA = list(control.inla=list(int.strategy = 'eb', cmin = 0.01), 
                                      control.compute = list(openmp.strategy="huge"), safe = TRUE), 
                          Ipoints = list(method = 'direct')) 
  }
  # Add bias fields if necessary
  if (!is.null(biasFieldList[[i]])) {
    indx <- biasFieldList[[i]] %in% workflow$.__enclos_env__$private$datasetName
    workflow$biasFields(biasFieldList[[i]][indx], shareModel = TRUE)
    }
  
  # I have to attach the environmental covariates again
  # For some reason it seems not to find the covariates raster after loading 
  # the previous workspace
  environmentalDataList <- rast(paste0(tempFolderName, "/environmentalDataImported.tiff"))
  env <- colnames(focalTaxa)[colnames(focalTaxa) %in% focalCovariates$parameters]
  focalTaxa <- focalTaxa[,c("taxa", env)]
  focalTaxa <- focalTaxa[focalTaxa$taxa %in% interestedGroup,]
  env <- env[apply(focalTaxa[,-1], 2, any)]
  # 
  for (e in env) {
    cat(sprintf("Adding covariate '%s' to the model.\n", e))
    workflow$addCovariates(Object = environmentalDataList[[e]])
  }
  
  # Run model (this directly saves output to folder specified above)

  intSDM::sdmWorkflow(workflow, 
                      predictionData = predictionData)
  
  # Change model name to ensure no overwrite of richness data
  if (modelRun %in% c("richness", "redListRichness")) {
    file.rename(paste0(folderName, "/modelOutputs/", focalGroup, "/richnessPredictions.rds"), 
                paste0(folderName, "/modelOutputs/", focalGroup, "/", modelRun, "Preds.rds"))
  }
  print(folderName)
  print(focalGroup)
}


# I run the parallelisation with the mclapply function
parallel::mclapply(1:5,
                   runModel,
                   "vascularPlants",
                   mc.cores = nClusters,
                   mc.preschedule = FALSE)
