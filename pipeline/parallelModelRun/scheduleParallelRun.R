args <- commandArgs(trailingOnly = TRUE)

start <- Sys.time()

i <- as.numeric(args[1])
covariatesSquared <- TRUE

#.libPaths(c("/cluster/projects/nn11017k/R"))
library(parallel)
library(foreach)
library(doParallel)
getwd()
interestedGroup <- "vascularPlants"
load(paste0(interestedGroup, "workflowWorkspace.RData"))

# For some reason i changes to 1 after loading the workspace

print(i)
#Load packages
#.libPaths(c("/cluster/projects/nn11017k/R"))
#.libPaths(c("/cluster/projects/nn11017k/R", .libPaths()))
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

# Choose one of the datasets within each segmentation as the prediction data
# I prefer to choose the one with the smallest data points
datasetNames <- workflow$.__enclos_env__$private$datasetName
datasetNames
namesSpeciesData <- names(speciesData)
namesSpeciesDataShort <- gsub(" ", "", gsub("[[:punct:]]", "", datasetNames))


if(!predictionDatasetShort %in% datasetNames){
  predictionDatasetShort <-  "mergedDatasetPA" #namesSpeciesDataShort[1]
}

rm("speciesData") 


print(predictionDatasetShort)

# create a mesh
meshToUse <- meshTest(myMesh, regionGeometry, crs = crs, print = FALSE)
# Add model characteristics (mesh, priors, output)
#workflow$addMesh(cutoff= myMesh$cutoff/1000, max.edge=myMesh$max.edge/1000, offset= myMesh$offset/1000)
workflow$addMesh(Object = meshToUse)

# Add priors to the model
workflow$specifySpatial(prior.range = c(15, 0.01),
                        prior.sigma = c(0.8, 0.01))

#modelOutputs



workflow$workflowOutput("Model")
print("lalala")

workflow$modelOptions(ISDM = list(pointCovariates = NULL,
                                  Offset = NULL, pointsIntercept = TRUE, 
                                  pointsSpatial = NULL)
)
# workflow$modelOptions(ISDM = list(control.inla=list(int.strategy = 'eb', cmin = 0.01),
#                                   # num.threads = 20,
#                                   safe = TRUE, 
#                                   verbose = TRUE, 
#                                   debug = TRUE
# ))

workflow$modelOptions(Richness = list(predictionIntercept = predictionDatasetShort, 
                                      speciesSpatial = "replicate",
                                      samplingSize = 0.25
))


# Add bias fields if necessary
# if (!is.null(biasFieldList[[i]])) {
#  indx <- biasFieldList[[i]] %in% c(datasetNames)
#  workflow$biasFields(biasFieldList[[i]][indx], shareModel = TRUE)
#}

# I have to attach the environmental covariates again
# For some reason it seems not to find the covariates raster after loading 
# the previous workspace

#focalTaxa$distance_water <- FALSE

environmentalDataList <- rast(paste0(tempFolderName, "/environmentalDataImported.tiff"))
levels(environmentalDataList$land_cover_corine)[[1]][,2][is.na(levels(environmentalDataList$land_cover_corine)[[1]][,2])] <- "Water bodies"
levels(environmentalDataList$land_cover_corine)[[1]][,2][28] <- "Moors and heathland"
landCover <- environmentalDataList$land_cover_corine 
sort(unique(values(environmentalDataList$land_cover_corine)[,1])) 
values(environmentalDataList$land_cover_corine)[,1][is.nan(values(environmentalDataList$land_cover_corine)[,1])] <- 48
levels(environmentalDataList$land_cover_corine) <- levels(landCover)




focalCovariates <- read.csv(paste0(folderName, "/focalCovariates.csv"), header= T)
env <- colnames(focalTaxa)[colnames(focalTaxa) %in% focalCovariates$parameters]
focalTaxa <- focalTaxa[,c("taxa", env)]
print( interestedGroup)
focalTaxa <- focalTaxa[focalTaxa$taxa %in% interestedGroup,]
env<- env[apply(focalTaxa[,-1], 2, any)]

quadratics <- focalCovariates[focalCovariates$quadratic & focalCovariates$parameters %in% env,]
if (nrow(quadratics) > 0) {
  for(i in seq_along(quadratics$quadratic)) {
    parameter <- quadratics$parameters[i]
    env <- c(env, paste0(parameter, "_squared"))
  }
}


env <- names(environmentalDataList)

for (e in env) {
  cat(sprintf("Adding covariate '%s' to the model.\n", e))
  workflow$addCovariates(Object = environmentalDataList[[e]])
}

levels(workflow$.__enclos_env__$private$Covariates$land_cover_corine)

# Specify formula for the model
cat("Specifying priors for the model")
workflow$modelFormula(covariateFormula = NULL,
                      biasFormula = ~ distance_water +  distance_roads 
) 

# Specify priors for the precision of intercept and groups
cat("Specifying priors for the hyperparameters in the model")

# workflow$specifyPriors(priorIntercept = list(prior = 'pc.prec', param = c(0.1, 0.1)), 
#                        priorGroup = list(prior = 'pc.prec', param = c(0.1, 0.1)),
#                        effectName = 'Intercept', Mean = 0, Precision = 0.1) 

# Run model (this directly saves output to folder specified above)
workflow$specifyPriors(effectNames = c("Intercept"), Mean = 0, Precision = 1,
                       priorIntercept = list(initial = -10, fixed = TRUE),
                       priorGroup = list(model = "iid",
                                         hyper = list(prec = list(initial = 2, fixed = TRUE))))

cat("Fitting the models")
intSDM::sdmWorkflow(workflow, 
                    inlaOptions = list(control.inla=list(int.strategy = 'eb', cmin = 0.01,  
                                                         control.vb= list(enable = FALSE)),
                                       # num.threads = 20,
                                       safe = TRUE, 
                                       verbose = TRUE, 
                                       debug = TRUE,
                                       inla.mode = "experimental"
                    )
)
cat("Finished fitting the models")

# Change model name to ensure no overwrite of richness data
cat("Changing the names of the returned output.")
print(focalGroup)

end <- Sys.time()

print(timeTaken <- end - start)
