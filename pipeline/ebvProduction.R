

#### ESSENTIAL BIODIVERSITY VARIABLE CREATION  ####

# This script takes the outputs from the full model run (richness and bias estimations) and converst them into
# essential biodiversity variables (species richness, uncertainty and sampling bias) for each parent taxa
# used in the run, and for all subgroups (threatened species, etc.) of those taxa.

# Import all functions and packages, define rune to use
args <- commandArgs(trailingOnly = TRUE)
dateAccessed <- as.character(args[1])
# dateAccessed <- "2026-08-28"

sapply(list.files("functions", full.names = TRUE, recursive = TRUE), source)
library(stringr)
library(dplyr)

# Define directory name
folderName <- paste0("data/run_", dateAccessed)
modelFolderName <- paste0(folderName, "/modelOutputs")

# Create processed output directory
if (!dir.exists(paste0(modelFolderName, "/processedOutputs"))) {
  dir.create(paste0(modelFolderName, "/processedOutputs"))
}

# import project control parameters into the environment
readRDS(paste0(folderName,"/controlPars.RDS")) %>% 
  list2env(envir = .GlobalEnv)

# Import subgroup list
subGroupDF <- read.csv(paste0(folderName, "/subGroups.csv"))

# 1. Preparation
# We need to get lists of files to process, and identify which segments of species did not run either models or
# predictions. We also need to import a large number of other data as well to edit our final products.

# And import the red list species and ansvarsarter
focalTaxa <- read.csv(file.path(folderName, "focalTaxa.csv"))

# Upload our base raster for projection
baseRaster <- rast(file.path(folderName, "baseRaster.tiff")) |>
  project("+proj=utm +zone=33 +datum=WGS84 +units=km +no_defs")

# If scaling used, insert here
croppingGeometryLocation <- "data/external/norge_border/Noreg_polygon.shp"
croppingGeometry <- vect(sf::read_sf(croppingGeometryLocation)) |>
  project(baseRaster)

# Get all richness files to start
allRichnessFiles <- list.files(modelFolderName, recursive = TRUE, full.names = TRUE, pattern = paste0("Richness.rds"))
json_list <- list()

for (taxa in unique(focalTaxa$parentTaxa)) {
  
  # Find all richness folders
  searchTaxa <- focalTaxa$taxa[focalTaxa$parentTaxa == taxa]
  taxaRichnessFiles <- grep(paste0(searchTaxa, collapse = "|"), allRichnessFiles, value = T)
  taxaRichnessDirs <- unique(sub("^((?:[^/]*/){4}).*", "\\1", taxaRichnessFiles))
  
  for (i in seq_along(taxaRichnessDirs)) {
    taxaGroup <- taxaRichnessDirs[i]
    
    speciesListed <- list.dirs(taxaGroup, recursive = FALSE)
    speciesListed <- speciesListed[!grepl("Bias", speciesListed)]
    
    # First we need to download all data and put it in one large data frame
    dataList <- lapply(paste0(speciesListed, "/Richness.rds"), rast) |>
      setNames(gsub(paste0(taxaGroup, "/"), "", speciesListed))
    
    meanRasterSub <- rast(lapply(dataList, `[`, "mean"))
    seRasterSub <- rast(lapply(dataList, `[`, "sd"))
    rm("dataList")
    gc()
    
    writeRaster(meanRasterSub*100, file = paste0(taxaGroup, "/mean.tiff"), overwrite = TRUE, datatype = "INT4U")
    writeRaster(seRasterSub*100, file = paste0(taxaGroup, "/se.tiff"), overwrite = TRUE, datatype = "INT4U")
    
    rm("meanRasterSub", "seRasterSub")
    gc()
    print(paste0(taxaGroup))
    
  }
  
  # First we need to download all data and put it in one large data frame
  cat("\nAggregating means for", taxa)
  meanRaster <- rast(paste0(taxaRichnessDirs, "/mean.tiff"))
  # meanRaster <- meanRaster * finalMask
  cat("\nAggregating sds for", taxa)
  seRaster <-rast(paste0(taxaRichnessDirs, "/se.tiff"))
  
  # Check for a predictor species
  predictorSpecies <- unique(names(meanRaster)[duplicated(names(meanRaster))])
  
  # For each predictor species, get the mean, and replace all other instances with said mean
  for (p in predictorSpecies) {
    allPredSpecies <- meanRaster[p]
    allPredSpeciesSE <- seRaster[p]
    predictorSpeciesRast <- mean(allPredSpecies) |> setNames(p)
    predictorSpeciesRastSE <- mean(allPredSpeciesSE) |> setNames(p)
    restOfSpecies <- meanRaster[[!(names(meanRaster) %in% p)]]
    restOfSpeciesSE <- seRaster[[!(names(seRaster) %in% p)]]
    meanRaster <- c(restOfSpecies, predictorSpeciesRast)
    seRaster <- c(restOfSpeciesSE, predictorSpeciesRastSE)
  }
  
  # Need table of means. THis process is followed for uncertainties as well.
  cat("\nCalculating species stats")
  allSpeciesStats <- c(sum(meanRaster)/100, sum(seRaster/100))
  names(allSpeciesStats) <- c("allSpecies_probability", "allSpecies_uncertainty")
  
  # Get red-listed and ansvars members of species
  groupList <- list()
  groupStat <- list()
  for (group in subGroups) {
    # Get list of species
    subGroupSpecies <- intersect(names(meanRaster), subGroupDF$simpleScientificName[subGroupDF[,group]])
    if (length(subGroupSpecies) == 0) {next}
    subSpeciesStats <- c(sum(meanRaster[[subGroupSpecies]])/100, sum(seRaster[[subGroupSpecies]]/100))
    names(subSpeciesStats) <- paste0(group, "_",c("probability", "uncertainty"))
    groupList[[group]] <- subSpeciesStats
    groupStat[[group]] <- length(subGroupSpecies)
  }
  groupList[["allspecies"]] <- allSpeciesStats
  
  # Now we need to crop and scale each group correctly
  transformedData <- list()
  for (x in names(groupList)) {
    croppedStats <- crop(groupList[[x]], croppingGeometry, mask = T)
    # Scale richness to 0-1
    richness <- croppedStats[[grep("probability", (names(croppedStats)))]]
    uncertainty <- croppedStats[[grep("uncertainty", (names(croppedStats)))]]
    scaledRichness <- (richness - minmax(richness)[1])/(minmax(richness)[2] - minmax(richness)[1])
    scaledUncertainty <- uncertainty/(richness+1)
    
    # Lastly, if max value is significantly higher than 99.5 percentile, truncate figure
    highQuantile <- quantile(values(scaledRichness), c(0.0025, 0.9975), na.rm= T)
    scaledRichness <- ifel(scaledRichness > highQuantile[2], highQuantile[2], scaledRichness)
    scaledRichness <- ifel(scaledRichness < highQuantile[1], highQuantile[1], scaledRichness)
    scaledRichness <- (scaledRichness - minmax(scaledRichness)[1])/(minmax(scaledRichness)[2] - minmax(scaledRichness)[1])
    
    transformedData[[x]] <- c(scaledRichness, scaledUncertainty)
  }
  
  
  # Get compiled list of species
  statsToSave <- rast(transformedData) |> setNames(unlist(lapply(transformedData, names)))
  
  # Get all bias files
  allBiasFiles <- grep(paste0(searchTaxa, collapse = "|"), list.files(modelFolderName, pattern = "Bias.rds", recursive = T, full.names = T), value = T)
  
  biasCompiled <- sum(do.call(c,lapply(allBiasFiles, FUN = function(biasFile) {
    crop(readRDS(biasFile)$mean, croppingGeometry, mask = T)
  })))
  statsToSave[["bias"]] <- biasCompiled
  
  # Save individual species rasters
  cat("\nWriting species probabilities")
  writeRaster(meanRaster, file = paste0(modelFolderName, "/processedOutputs/speciesprobability_", taxa,".tiff"), overwrite = TRUE)
  cat("\nWriting species uncertainties")
  writeRaster(seRaster, file = paste0(modelFolderName, "/processedOutputs/speciesuncertainty_", taxa,".tiff"), overwrite = TRUE)
  
  # Scale final rasters
  writeRaster(statsToSave, file = paste0(modelFolderName, "/processedOutputs/allstats_", taxa,".tiff"), overwrite = TRUE)
  
  # Metadata 
  json_list[[taxa]] <- list(
    taxa = searchTaxa,
    n_taxa = length(searchTaxa),
    n_species = nlyr(meanRaster),
    n_subgroups_species = groupStat,
    file_name = paste0(modelFolderName, "/processedOutputs/allstats_", taxa,".tiff")
  )
}

# read existing json
json_ls <- jsonlite::fromJSON(file.path(extFolderName, "metadata.json"))

# 4a lists the species stuff from above
json_ls$step_4a <- json_list

# define json content
json_ls$step_4b <- list(
  resolution = res,
  crs = crs,
  cropping_geometry = croppingGeometryLocation,
  transformations = data.frame(transformation = c("cropped", "scaled", "truncated"),
                               description = c("Cropped to relevant geometry.", "Scaled to between 0 and 1.",
                                               "Truncated to the .25 and 99.75 percentiles to account for outlying values."))
  
)

# write json
jsonlite:::write_json(json_ls,
                      file.path(extFolderName, "metadata.json"), 
                      pretty = TRUE)

