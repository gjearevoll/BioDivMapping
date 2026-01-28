
.libPaths(c("/cluster/projects/nn11017k/R"))
library(PointedSDMs)
library(terra)
library(inlabru)
library(sf)
library(INLA)
library(dplyr)
library(qs)
##1.1 Data needs to be a list of sf datasets,

args <- commandArgs(trailingOnly = TRUE)

start <- Sys.time()

i <- 1 #as.numeric(args[1])
dateAccessed <- "2025-12-11" #args[2]
modelSave <- TRUE #args[3]
biasField <- TRUE #args[4]
POTrend <- FALSE #args[5]
yearTrend <- TRUE #args[6]

# Define model characteristics
modelRun <- c("biasField", "POTrend", "yearTrend")[c(biasField, POTrend, yearTrend)]
saveRDS(modelRun, paste0(modelFolder, "/modelRun.RDS"))

##Load in data here

#dateAccessed <- "2025-12-11"
folderName <- paste0("data/run_", dateAccessed)
readRDS(paste0(folderName,"/controlPars.RDS")) %>% 
  list2env(envir = .GlobalEnv)
sapply(list.files("functions", full.names = TRUE), source)

cat("Loading species data \n")
specData <- qread(paste0(folderName, "/speciesDataProcessed.qs"))

#Select species
segmentList <- readRDS(paste0(folderName, "/segmentList.RDS"))
#speciesSelect <- "Lysimachia_europaea"
speciesSelect <- segmentList[i]
cat("Refining species data \n")
specData2 <- lapply(specData, function(x) x[x$simpleScientificName %in% speciesSelect & x$year > 0, ])
specData3 <- specData2[(lapply(specData2, nrow) != 0)]

# Create species folder
cat("Creating model output folder \n")
modelFolder <- paste0(folderName, "/modelOutputsBiasYearTrend/", speciesSelect)
if (!dir.exists(modelFolder)) {
  dir.create(modelFolder)
}

# Merge presence only data
PODatasets <- unlist(lapply(specData3, FUN = function(x) {
  unique(x$dataType) =="PO"
}))
presenceOnlyData <- do.call(rbind, specData3[PODatasets])
specData3 <- c(list(presenceOnlyData = presenceOnlyData), specData3[!PODatasets])

# Get rid of empty datasets
specData3 <- specData3[unlist(lapply(specData3, FUN = function (x) {!is.null(x)}))]

# Change names
cat("Changing species data names 1\n")
speciesDatanameToChange <- names(specData3)[which(grepl("[^[:alnum:] ]", names(specData3)))]
print(paste("Changing name of this dataset:", speciesDatanameToChange))
speciesDatanameChanged <- gsub("[^[:alnum:] ]", '', speciesDatanameToChange)
names(specData3)[names(specData3) %in% speciesDatanameToChange] <- speciesDatanameChanged

cat("Changing species data names 2\n")
speciesDatanameToChange <- names(specData3)[which(grepl("[^\x01-\x7F]+", names(specData3)))]
print(paste("Changing name of this dataset:", speciesDatanameToChange))
speciesDatanameChanged <- gsub("[^\x01-\x7F]+", '', speciesDatanameToChange)
names(specData3)[names(specData3) %in% speciesDatanameToChange] <- speciesDatanameChanged

cat("Changing species data names 3\n")
names(specData3) <- gsub(" ", '', names(specData3))


##1.2 Covariates need to be terra objects, or a list of terra objects

##Load in  covariates here

cat("Loading environmental data \n")
envData <- lapply(readRDS(paste0(folderName, "/temp/environmentalDataImported.RDS")), terra::unwrap)

##Make sure lyr names == cov names
##Filter all years not in occ_data to ensure everything works fine

cat("Filtering environmental data \n")
envData <- mapply(function(x, y) {
  
  if (terra::nlyr(x) == 1) names(x) <- y
  
  if (terra::nlyr(x) > 4) x[[as.character(yearInterval)]] # or however
  else x
  
}, x = envData, y = names(envData))

print(envData[[1]])
print(envData[[2]])

crs <- '+proj=utm +zone=33 +datum=WGS84 +units=km +no_defs'

cat("Importing region geometry \n")
regionGeometry <- readRDS(paste0(folderName, "/regionGeometry.RDS"))
regionGeometryProj <- st_transform(regionGeometry, crs)

myMesh$cutoff <- myMesh$cutoff/1000
myMesh$max.edge <- myMesh$max.edge/1000
myMesh$offset <- myMesh$offset/1000

cat("Creating mesh \n")
meshToUse <- meshTest(myMesh, regionGeometryProj, crs = crs, print = TRUE)


##1.4 Create integration points
#This should be done before and saved to spead up 
cat("Creating integration points \n")
IPoints <- fmesher::fm_int(domain = meshToUse, samplers = regionGeometryProj, 
                           int.args = list(method = 'direct', nsub1 = 10, nsub2 = 10)) #Change these

# Saved the image here
# cat("Saving workspace \n")
# save.image(paste0(folderName, "/workspaces/upToIP.RData"))

##1.3 Create PointedSDM setups (assuming that we are doing single species models)
##Multi species would not work with current setup of PointedSDMs

cat("Creating mesh projection \n")
meshProjection <- fm_wkt(meshToUse)

specDataProj <- lapply(specData3, FUN = function(x) {
  st_transform(x, crs)
})
envDataProj <- lapply(envData, FUN = function(x) {
  project(x, crs)
})

cat("Setting up PointedSDM model \n")
model_setup <- startISDM(specDataProj, ##list of dataset names
                         Boundary = regionGeometryProj, #Norway boundary
                         IPS = IPoints,
                         pointsSpatial = 'shared', #New
                         Mesh = meshToUse,
                         Projection = fm_wkt(meshToUse),
                         spatialCovariates = envDataProj, #Raster layers as a list
                         responsePA = 'individualCount',
                         temporalName = 'year',
                         Formulas = list(biasFormula = ~ distance_roads)
)



##1.4 Add trends into the model
if (yearTrend) {
  cat("Adding year trend to model \n")
  model_setup$changeComponents('yearTrend(main = year, model = "ar1", constr = FALSE,
                             hyper = list(prec = list(prior = "gaussian", param = c(0, 1)),
                             rho = list(prior ="pc.cor1", param = c(0,0.9))))')
  model_setup$updateFormula(processLevel = TRUE, Formula = ~ . + yearTrend)
}

if (POTrend) {
  cat("Adding POtrend to model \n")
  model_setup$changeComponents('POTrend(main = year, model = "ar1", hyper = list(prec = list(prior = "gaussian", param = c(0, 1))))')
  model_setup$updateFormula(datasetName = 'presenceOnlyData',Formula = ~ . + POTrend)
}

model_setup$updateFormula(datasetName = 'TOVData',Formula = ~ . - distance_roads)

##1.5 Add Bias
#Not sure how we will do this
#Can copy out until we figure it out
if (biasField) {
  cat("Adding bias to model \n")
  model_setup$addBias(allPO = TRUE,
                      shareModel = TRUE,
                      copyModel = FALSE,
                      temporalModel = list(model = "iid"))
}

##1.6 Add priors
#Fixed effects
cat("Adding priors to model \n")
for (dataset in names(specDataProj)) model_setup$priorsFixed(Effect = 'Intercept',
                                                             mean.linear = 0,
                                                             prec.linear = 1,
                                                             datasetName = dataset)

#Random
#Needs to change
cat("Adding spatial and spatiotemporal priors to model \n")
model_setup$specifySpatial(sharedSpatial = TRUE, prior.range = c(100, 0.1), prior.sigma = c(1, 0.1))

if (biasField) {
  model_setup$specifySpatial(Bias = TRUE, prior.range = c(10, 0.1), prior.sigma = c(1, 0.1))
}

model_setup$specifyRandom(temporalModel = list(model = 'ar1', hyper = list(rho = list(prior = 'pc.cor1', param = c(0, 0.9)))))

##1.7 Tightening of priors
INLA:::inla.set.control.fixed.default(prec = 0, mean.linear = 0, prec.linear = 1) # THIS LINE TIGHTENS PRIORS FOR COVARIATE EFFECTS INLCUDING DISTANCE TO ROAD

## 2.0 Estimate models 

# cat("Saving workspace before fitting \n")
# save.image(paste0(folderName, "/workspaces/temporalWorkspaceBeforeFitting.RData"))

cat("Fitting model \n")
model <- fitISDM(model_setup, options = list(bru_max_iter = 1, 
                                             num.threads = 20,
                                             verbose = FALSE,
                                             safe = TRUE,
                                             inla.mode = 'experimental',
                                             control.inla = list(
                                               int.strategy = 'ccd',
                                               cmin = 0, 
                                               control.vb = list(enable = FALSE), #NEW,
                                               tolerance = 0.001,
                                               h = 0.001,
                                               stupid.search = TRUE,
                                               stupid.search.max.iter = 50,
                                               strategy = "adaptive",
                                               diagonal = 0)))
writeLines(model$logfile, paste0(modelFolder, '/file.txt'))

#Save model
if (modelSave) {
  qsave(model, paste0(folderName, "/modelOutputsBiasYearTrend/temporalModel_", speciesSelect, ".qs"))
}

cat("Calculating model run time taken \n")
end <- Sys.time()
timeTaken <- end - start
print(timeTaken)
saveRDS(20 * timeTaken, paste0(modelFolder, "/timeTaken.RDS"))

##Extract everything relevant from the model
#Save these
cat("Extracting model components \n")
RandomComponents <- model$summary.random
FixedComponents <- model$summary.fixed
Hyperparameters <- model$summary.hyper

componentList <- list(random = RandomComponents,
                      fixed = FixedComponents,
                      hyper = Hyperparameters)

saveRDS(componentList, paste0(modelFolder, "/fixedComponents.RDS"))

yearsIn <- yearInterval

# cat("Remove model \n")
# rm(model)

##Make plots here

##Plot of intensity function:

#First need plot of covariates:
#Need a list of prediction datasets (sf format) called predData
#One for each year in the model (with correct covariates in each)

cat("Loading prediction data \n")

predData <- qread(paste0(folderName, "/temp/predData.qs"))
coords = st_coordinates(predData[[1]])

predDataNonBias <- lapply(predData, FUN = function(x) {
  x[,!(colnames(x) %in% c("distance_roads"))] 
})
predDataBias <- lapply(predData, FUN = function(x) {
  x[,(colnames(x) %in% c("distance_roads"))] 
})


cat("Run fixed effects")
FixedUsed <- FixedComponents[row.names(FixedComponents) %in% names(predData[[1]]),]
FixedList <- list()
for (i in 1:length(predData)) {
  
  Vals <- st_drop_geometry(predData[[i]])
  Vals <- Vals[,names(Vals) %in% row.names(FixedUsed)]
  
  Vals <- data.frame(as.matrix(Vals) %*% as.matrix(FixedUsed[names(Vals),]))
  
  fixedVals <- cbind(coords, data.frame(median = Vals$X0.5quant))
  fixedVals$sd <- sum(FixedUsed$sd) #Not sure how to get SD out of here, but it dosen't vary spatially
  fixedVals <- rast(fixedVals) #assuming we want a raster output
  
  crs(fixedVals) <- fm_wkt(meshToUse)
  fixedVals <- mask(fixedVals, vect(regionGeometryProj))
  FixedList[[i]] <- fixedVals
  names(FixedList)[[i]] <- as.character(names(predData[i]))
  
}
saveRDS(lapply(FixedList, terra::wrap), paste0(modelFolder, "/likelihoods.RDS"))



modelMesh <- model$bru_info$model$effects$shared_spatial$main$mapper$mesh

##Create map of spatial field

STField <- split(RandomComponents$shared_spatial,
                 cut(seq_along(RandomComponents$shared_spatial$mean),
                     length(yearsIn), labels = FALSE))

STRasterMedian <- rast()
STRasterSD <- rast()

focalTaxon <- read.csv(paste0(folderName, "/focalTaxa.csv"))
repDataSet <- focalTaxon$predictionDataset

for (i in seq(1, length(yearsIn))) {
  
  TimeField <- fm_evaluate(modelMesh,  data.frame(STField[[i]]), loc = predData[[1]])
  
  temp <- cbind(coords,  data.frame(TimeField[, c('sd', 'X0.5quant')]))
  names(temp)[names(temp) == 'X0.5quant'] <- 'median'
  
  temp <- rast(temp)
  crs(temp) <- fm_wkt(meshToUse)
  temp <-  mask(temp, st_sf(regionGeometryProj))
  
  median <- temp$median + 
    FixedList[[i]]$median + 
    FixedComponents[paste0(repDataSet,'_intercept'), '0.5quant']
  sd <- temp$sd + 
    RandomComponents$yearTrend[['sd']][i] + 
    FixedList[[i]]$sd + 
    FixedComponents[paste0(repDataSet,'_intercept'), 'sd']
  
  if (yearTrend) {
    median <- median + 
      RandomComponents$yearTrend[['0.5quant']][i]
    sd <- temp$sd + 
      RandomComponents$yearTrend[['sd']][i]
  }
  
  names(median) <-  yearsIn[i]
  names(sd) <- yearsIn[i]
  
  STRasterMedian <- c(STRasterMedian, median)
  STRasterSD <- c(STRasterSD, sd)
}


STRasters <- list(STRasterMedian = STRasterMedian,
                  STRasterSD = STRasterSD)
saveRDS(lapply(STRasters, terra::wrap), paste0(modelFolder, "/spatioTemporalFields.RDS"))

cat("Calculating time taken \n")
end <- Sys.time()
timeTaken <- end - start
print(timeTaken)
saveRDS(20 * timeTaken, paste0(modelFolder, "/timeTaken.RDS"))

# Create maps of bias fields

if ("presenceOnlyData" %in% names(specData3)) {
  cat("Creating bias fields \n")
  
  if (biasField) {
    BiasDat <- split(RandomComponents$sharedBias_biasField,
                     cut(seq_along(RandomComponents$sharedBias_biasField$mean),
                         length(yearsIn),
                         labels = FALSE))
  }
  BiasEffects <- RandomComponents$Bias__Effects__Comps
  
  BiasRasterMedian <- terra::rast()
  BiasRasterSD <- terra::rast()
  
  for (i in seq(1, length(yearsIn))) {
    
    
    temp2 <- cbind(coords, data.frame(as.matrix(st_drop_geometry(predDataBias[[i]])) %*% 
                                        as.matrix(BiasEffects[BiasEffects$ID %in% c("distance_roads"),c('sd', '0.5quant')])))
    names(temp2)[names(temp2) == 'X0.5quant'] <- 'median'
    
    if (biasField) {
      BiasField <- fm_evaluate(modelMesh, data.frame(BiasDat[[i]]), loc = predData[[1]])
      temp1 <- cbind(coords, data.frame(BiasField[, c('sd' , 'X0.5quant')]))
      names(temp1)[names(temp1) == 'X0.5quant'] <- 'median'
      temp <- temp1
      temp$median <- temp1$median + temp2$median
      temp$sd <- temp1$sd + temp2$sd
      temp <- rast(temp)
    } else {
      temp <- rast(temp2)
    }
    
    crs(temp) <- fm_wkt(meshToUse)
    
    names(temp) <- rep(as.character(yearsIn[i]), 2)
    temp <- mask(temp, st_sf(regionGeometryProj))
    BiasRasterMedian <- c(BiasRasterMedian, temp[[2]])
    BiasRasterSD <-  c(BiasRasterSD, temp[[1]])
    
  }
  BiasRasters <- list(BiasRasterMedian = BiasRasterMedian,
                      BiasRasterSD = BiasRasterSD)
  saveRDS(lapply(BiasRasters, terra::wrap), paste0(modelFolder, "/biasFields.RDS"))
} else {
  cat("No presence only data, skipping bias field creation\n")
}
##Save biasRaster

