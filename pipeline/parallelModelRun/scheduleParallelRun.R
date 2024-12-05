# Load in all necessary parameters and environmental necessities

args <- commandArgs(trailingOnly = TRUE)

start <- Sys.time()

i <- as.numeric(args[1])
covariatesSquared <- TRUE
.libPaths(c("/cluster/projects/nn11017k/R"))
library(parallel)
library(foreach)
library(doParallel)
getwd()

interestedGroup <- "vascularPlants"
load(paste0(interestedGroup,"workflowWorkspace.RData"))

print(i)

library(intSDM)
library(rgbif)
library(terra)
library(dplyr)

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


print(predictionDatasetShort)

# create the mesh based on our predefined parameters
meshToUse <- meshTest(myMesh, regionGeometry, crs = crs, print = FALSE)
# Add model characteristics (mesh, priors, output)
#workflow$addMesh(cutoff= myMesh$cutoff/1000, max.edge=myMesh$max.edge/1000, offset= myMesh$offset/1000)
workflow$addMesh(Object = meshToUse)

# Add priors to the model
workflow$specifySpatial(prior.range = c(15, 0.01),
                        prior.sigma = c(0.8, 0.01))


modelOutputs <- c('Predictions', 'Model')

workflow$workflowOutput(modelOutputs)


workflow$modelOptions(ISDM = list(pointCovariates = NULL,
                                  Offset = NULL, 
                                  pointsIntercept = TRUE, 
                                  pointsSpatial = NULL)
)


workflow$modelOptions(Richness = list(predictionIntercept = predictionDatasetShort, 
                                      speciesSpatial = "replicate"
                                      #samplingSize = 0.25
))



# Need to do a final edit of CORINE data
environmentalDataList <- rast(paste0(tempFolderName, "/environmentalDataImported.tiff"))
levels(environmentalDataList$land_cover_corine)[[1]][,2][is.na(levels(environmentalDataList$land_cover_corine)[[1]][,2])] <- "Water bodies"
levels(environmentalDataList$land_cover_corine)[[1]][,2][28] <- "Moors and heathland"
landCover <- environmentalDataList$land_cover_corine 
sort(unique(values(environmentalDataList$land_cover_corine)[,1])) 
values(environmentalDataList$land_cover_corine)[,1][is.nan(values(environmentalDataList$land_cover_corine)[,1])] <- 48
levels(environmentalDataList$land_cover_corine) <- levels(landCover)

# Now make sure the loaded and used environmental covariates match
focalCovariates <- read.csv(paste0(folderName, "/focalCovariates.csv"), header= T)

print(focalCovariates$parameters)
env <- colnames(focalTaxa)[colnames(focalTaxa) %in% focalCovariates$parameters]
focalTaxa <- focalTaxa[,c("taxa", env)]
print( interestedGroup)
focalTaxa <- focalTaxa[focalTaxa$taxa %in% interestedGroup,]
env <- env[apply(focalTaxa[,-1], 2, any)]

# Ensure we have our quadratic terms applied
if(covariatesSquared){
  if("summer_precipitation" %in% env){
    environmentalDataList$summer_precipitation_squared <- (environmentalDataList$summer_precipitation)^2
    env <- c(env, "summer_precipitation_squared")
  }
  if("summer_temperature" %in% env){ 
    environmentalDataList$summer_temperature_squared <- (environmentalDataList$summer_temperature)^2
    env <- c(env,  "summer_temperature_squared")
  }
}

# Add covariates to the model
for (e in env) {
  cat(sprintf("Adding covariate '%s' to the model.\n", e))
  workflow$addCovariates(Object = environmentalDataList[[e]])
}


# Specify formula for the model
cat("Specifying priors for the model")
workflow$modelFormula(covariateFormula = NULL,
                      biasFormula = ~ distance_water +  distance_roads 
) 

# Specify priors for the precision of intercept and groups
cat("Specifying priors for the hyperparameters in the model")

# Run model (this directly saves output to folder specified above)
workflow$specifyPriors(effectNames = c("Intercept"), Mean = 0, Precision = 1,
                       priorIntercept = list(initial = -10, fixed = TRUE),
                       priorGroup = list(model = "iid",
                                         hyper = list(prec = list(initial = 0, fixed = TRUE))))

cat("Fitting the models")
intSDM::sdmWorkflow(workflow, 
                    predictionData = predictionData,
                    inlaOptions = list(control.inla=list(int.strategy = 'eb', cmin = 0.01),
                                       num.threads = 20,
                                       #num.threads = 5, 
                                       safe = TRUE, 
                                       verbose = TRUE, 
                                       debug = TRUE)
)
cat("Finished fitting the models")

# Change model name to ensure no overwrite of richness data
cat("Changing the names of the returned output.")
if (modelRun %in% c("richness", "redListRichness")) {
  file.rename(paste0(folderName, "/modelOutputs/", focalGroup, "/richnessPredictions.rds"), 
              paste0(folderName, "/modelOutputs/", focalGroup, "/", modelRun, "Preds.rds"))
}
print(folderName)
print(focalGroup)

end <- Sys.time()

print(timeTaken <- end - start)
