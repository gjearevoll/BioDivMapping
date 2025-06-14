# Load in all necessary parameters and environmental necessities

args <- commandArgs(trailingOnly = TRUE)

start <- Sys.time()

i <- as.numeric(args[1])
dateToUse <- args[2]
nThreads <- 20
.libPaths(c("/cluster/projects/nn11017k/R"))
library(parallel)
library(foreach)
library(doParallel)
getwd()

# Load in segment number and interested group name
segmentList <- readRDS(paste0("data/run_", dateToUse, "/segmentList.RDS"))

interestedGroup <- gsub('[[:digit:]]+', '', segmentList[i])
load(paste0("data/run_", dateToUse, "/workspaces/", interestedGroup,"workflowWorkspace.RData"))
sapply(list.files("functions", full.names = TRUE), source)

print(segmentList[i])

library(intSDM)
library(rgbif)
library(terra)
library(dplyr)

focalGroup <- segmentList[i]
workflow <- workflowList[[focalGroup]]
print(focalGroup)

rm("workflowList")

# load the control parameters
readRDS(paste0(folderName,"/controlPars.RDS")) %>% 
  list2env(envir = .GlobalEnv)

# Find prediction dataset
predictionDataset <- focalTaxa$predictionDataset[focalTaxa$taxa == gsub('[[:digit:]]+', '', focalGroup)]
predictionDatasetShort <- gsub(" ", "", gsub("[[:punct:]]", "", predictionDataset))

# Choose one of the datasets within each segmentation as the prediction data
# I prefer to choose the one with the smallest data points
datasetNames <- workflow$.__enclos_env__$private$datasetName
datasetNames
namesSpeciesData <- names(speciesData)
namesSpeciesDataShort <- gsub(" ", "", gsub("[[:punct:]]", "", datasetNames))

if(length(predictionDatasetShort) > 1){
  predictionDatasetShort <- predictionDatasetShort[1]
}

if(!predictionDatasetShort %in% datasetNames){
  predictionDatasetShort <-  namesSpeciesDataShort[!predictionDatasetShort %in% namesSpeciesDataShort][1]
}

rm("speciesData") 
gc()

print(predictionDatasetShort)

# # In case the mesh needs a late redefinition
# myMesh$cutoff <- 3*1000
# myMesh$offset <- c(20, 100) * 1000
# myMesh$max.edge <- c(200, 500) * 1000
# fm_int(domain = meshToUse, samplers = regionGeometry, int.args = list(method = 'direct', nsub1 = 15, nsub2 = 15))
meshToUse <- meshTest(myMesh, regionGeometry, crs = crs, print = TRUE)


# Add model characteristics (mesh, priors, output)
workflow$addMesh(Object = meshToUse)

# Sort out model options
workflow$specifySpatial(prior.range = c(prior.range[1], prior.range[2]),
                        prior.sigma = c(prior.sigma[1], prior.sigma[2]))
workflow$workflowOutput(c('Predictions', 'Model'))
workflow$modelOptions(ISDM = list(pointCovariates = NULL,
                                  Offset = NULL, 
                                  pointsIntercept = TRUE, 
                                  pointsSpatial = NULL)
)
workflow$modelOptions(Richness = list(predictionIntercept = predictionDatasetShort, 
                                      speciesSpatial = "replicate"
                                      #samplingSize = 0.25
))

# Now add environmental covariates to the model
environmentalDataList <- rast(paste0(tempFolderName, "/environmentalDataImported.tiff"))
focalCovariates <- read.csv(paste0(folderName, "/focalCovariates.csv"), header= T)

print(focalCovariates$parameters)
env <- colnames(focalTaxa)[colnames(focalTaxa) %in% focalCovariates$parameters[!focalCovariates$categorical]]
focalTaxa <- focalTaxa[,c("taxa", env)]
focalTaxa <- focalTaxa[focalTaxa$taxa %in% interestedGroup,]
env <- env[apply(focalTaxa[,-1], 2, any)]

# Add quadratic variables
quadratics <- paste0(focalCovariates$parameters[focalCovariates$quadratic & focalCovariates$parameters %in% env], "_squared")
env <- c(env, quadratics)

# Add categorical variables
categoricals <- focalCovariates$parameters[focalCovariates$categorical]
categoricals2 <- names(environmentalDataList)[apply(sapply(categoricals, FUN = function(x) {grepl(x, names(environmentalDataList))}), 1, any)]
env <- c(env, categoricals2)
envRenamed <- gsub(" ","_",stringr::str_replace_all(env, "[[:punct:]]", "_"))
names(environmentalDataList) <- gsub(" ","_",stringr::str_replace_all(names(environmentalDataList), "[[:punct:]]", "_"))
#
for (e in envRenamed) {
  cat(sprintf("Adding covariate '%s' to the model.\n", e))
  workflow$addCovariates(Object = environmentalDataList[[e]])
}

rm("environmentalDataList")


# Specify formula for the model
cat("Specifying priors for the model")
workflow$modelFormula(covariateFormula = NULL,
                      biasFormula = NULL 
) 

workflow$biasFields(datasetName = "mergedDatasetPO", prior.range = c(10 * 1000, 0.01),
                    prior.sigma = c(0.8, 0.01))

# Specify priors for the precision of intercept and groups
cat("Specifying priors for the hyperparameters in the model")

predictionData <- createPredictionData(c(5000, 5000), regionGeometry, proj = crs)
# Run model (this directly saves output to folder specified above)
workflow$specifyPriors(effectNames = c("Intercept"), Mean = 0, Precision = 1,
                       priorIntercept = list(initial = -10, fixed = TRUE),
                       priorGroup = list(model = "iid",
                                         hyper = list(prec = list(initial = 0, fixed = TRUE))))

cat("Fitting the models")
cat(dateToUse)
cat(folderName)

intSDM::sdmWorkflow(workflow, 
                    predictionData = predictionData,
                    inlaOptions = list(control.inla=list(int.strategy = 'eb', cmin = 0.01),
                                       num.threads = nThreads,
                                       #num.threads = 5, 
                                       safe = TRUE, 
                                       verbose = TRUE, 
                                       debug = TRUE),
                    ipointsOptions = list(method = 'direct', nsub1 = 10, nsub2 = 10)
)
cat("Finished fitting the models")

# Change model name to ensure no overwrite of richness data
cat("Changing the names of the returned output.")

file.rename(paste0(folderName, "/modelOutputs/", focalGroup, "/richnessPredictions.rds"), 
            paste0(folderName, "/modelOutputs/", focalGroup, "/", modelRun, "Preds.rds"))

cat("Resizing model object")

source("functions/resetEnvironments.R")
richnessModel <- readRDS(paste0(folderName, "/modelOutputs/", focalGroup, "/richnessModel.rds"))
#obj_size(richnessModel)
reducedModel <- reset_environments(richnessModel)
saveRDS(reducedModel, paste0(folderName, "/modelOutputs/", focalGroup, "/richnessModel.rds"))

print(folderName)
print(focalGroup)

end <- Sys.time()
timeTaken <- end - start
print(timeTaken)
saveRDS(nThreads * timeTaken, paste0(folderName, "/modelOutputs/", focalGroup, "/timeTaken.RDS"))
