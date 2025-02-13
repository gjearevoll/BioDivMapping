# Simultaneous processing

library(dplyr)
library(sf)
library(ggplot2)
library(divraster)
library(stringr)
library(tidyr)
library(stars)
library(smoothr)

# Import all functions
sapply(list.files("functions", full.names = TRUE, recursive = TRUE), source)

# Define directory name
directoryName <- "data/run_2025-01-06/modelOutputs"


# 1. Preparation
# We need to get lists of files to process, and identify which segments of species did not run either models or
# predictions. We also need to import a large number of other data as well to edit our final products.

# add simpleScientificName column
redList <- readRDS("data/archive/run_2024-07-26/redList.RDS")
redList <- redList %>%
  mutate(
    simpleScientificName = gsub("-", "", gsub("×","", species)),
    simpleScientificName = coalesce(
      simpleScientificName[match(simpleScientificName, redList$GBIFName)],  # Match redList species
      str_extract(simpleScientificName, "^[A-Za-z]+\\s+[a-z]+")        # Extract binomial name
    ),
    # Replace space with underscore in simpleScientificName
    simpleScientificName = gsub("-", "", gsub("×","", gsub(" ", "_", simpleScientificName)))
  )

# And import the alien species list and ansvarsarter
alienSpecies <- readRDS("data/external/alienSpeciesList.RDS")
ansvarsArter <- readRDS("data/external/ansvarsArterList.RDS")
insectList <- c("aquaticInsects", "beetles", "bugs", "butterfliesMoths", "cockroaches", "earwigs", "flies", 
                "grasshopperLikeInsects", "hymenopterans", "netWingedInsects", "scorpionflies", "snakeflies",
                "spiders")


# Get taxa folder names
dirNames <- list.dirs(directoryName, recursive = FALSE)
dirNamesShort <- sapply(strsplit(dirNames, "/\\s*"), tail, 1)
dirsWithModel <- sapply(strsplit(list.files(directoryName, recursive = TRUE, 
                                            pattern = "*richnessModel.rds"), '\\s*/'), `[`, 1)
dirsNoModel <- dirNames[!gsub(paste0(directoryName, "/"), "", dirNames) %in% dirsWithModel]
dirsNoModelShort <- sapply(strsplit(dirsNoModel, "/\\s*"), tail, 1)
modelsWRichness <- list.files(paste0(directoryName, "/", dirsWithModel), recursive = TRUE, 
                              full.names = TRUE, pattern = "*Richness.rds")
modelsWRichnessSimple <- unique(sapply(strsplit(gsub(paste0(directoryName, "/"), '', modelsWRichness), '\\s*/'), `[`, 1))
modelsNoRichness <- dirsWithModel[!(dirsWithModel %in% modelsWRichnessSimple)]
dirsNoRichness <- dirNamesShort[!(dirNamesShort %in% modelsWRichnessSimple)]
taxaList <- unique(gsub('[[:digit:]]+', '', str_replace(dirNames, paste0('(.*?)', directoryName, '/(.*?)'), '\\1')))
taxaWithData <- unique(gsub('[[:digit:]]+', '', modelsWRichnessSimple))
relevantDirNames <- paste0(directoryName, "/", modelsWRichnessSimple)

# Code for creating extra segment lists
# Check percentage first
speciesToIsolate <- c("fungi")
#speciesToIsolate <- c("bugs", "flies", "grasshopperLikeInsects", "scorpionflies", "spiders")
percentageTotalRun <- 1-length(dirsNoRichness[grepl(paste(speciesToIsolate,collapse = "|"), dirsNoRichness)])/
  length(dirNamesShort[grepl(paste(speciesToIsolate,collapse = "|"), dirNamesShort)])
percentageTotalRun
segmentListRerun <- dirsNoRichness[grepl(paste(speciesToIsolate,collapse = "|"), dirsNoRichness)]
#segmentListRerun <- segmentListRerun[!grepl("Moths", segmentListRerun)] # if flies is involved
saveRDS(segmentListRerun, paste0("segmentList_",paste(speciesToIsolate,collapse = ""), ".RDS"))

# Base raster for projecting fixes
oneRast <- readRDS(list.files(relevantDirNames[grep("beetles", relevantDirNames)][1], full.names = TRUE, recursive = TRUE)[1])
baseRaster <-rast(ext = ext(oneRast)+10, res = res(oneRast))
crs(baseRaster) <- crs(oneRast)

# 2. Condensing of segments
# Here we turn our 10ish RDS files into one more easily useable tiff file, which can then be uploaded easily
# in step 2.

for (t in 1:length(taxaWithData)) {

  # Get everything we need as an empty list
  taxa <- taxaWithData[t]
  if (grepl("vascularPlants", taxa)) {taxa <- "vascularPlants"}

  dirNamesTaxa <- relevantDirNames[grep(paste0("/",taxa), relevantDirNames)]
  oneRast <- readRDS(list.files(dirNamesTaxa[1], full.names = TRUE, recursive = TRUE, pattern = "ss.rds")[1])
  meanRaster <- seRaster <- rast(ext = ext(oneRast), res = res(oneRast))
  crs(meanRaster) <- crs(seRaster) <- crs(oneRast)

  # Start cycling through species groups
  for (i in seq_along(dirNamesTaxa)) {
    taxaGroup <- dirNamesTaxa[i]

    # if (file.exists(paste0(taxaGroup, "/mean.tiff"))) {
    #   next
    # }

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
    writeRaster(seRasterSub, file = paste0(taxaGroup, "/se.tiff"), overwrite = TRUE, datatype = "FLT4S")


    rm("meanRasterSub", "seRasterSub")
    gc()
    print(paste0(taxaGroup))
  }

}

# 3. Production of compiled data
# Here we produce the first 2 data packages. The first contains all probabilities and uncertainties for individual
# species. The second contains all species stats (compiled richness and uncertainty) per species group.
# It does this for all species, and (if applicable) for all threatened species and ansvarsarter.

# here we need to group all insects as one, so we need to reduce the list somewhat.

taxaToMerge <- c("insects")
# Start looping through the groups
speciesData2 <- lapply(taxaToMerge, FUN = function(taxa) {
  
  # Get everything we need as an empty list
  if (grepl("vascularPlants", taxa)) {taxa <- "vascularPlants"}
  
  # If looking at insects, get all names
  if (taxa == "insects") {
    dirNamesTaxa <-  grep(paste(insectList, collapse="|"), relevantDirNames, value = TRUE) 
  } else {
    dirNamesTaxa <- grep(paste0("/",taxa), relevantDirNames, value = TRUE)
  }
  
  # Start cycling through species groups
  # First we need to download all data and put it in one large data frame
  meanRaster <- rast(paste0(dirNamesTaxa, "/mean.tiff"))
  # meanRaster <- meanRaster * finalMask
  seRaster <-rast(paste0(dirNamesTaxa, "/se.tiff"))
  
  # Save individual species rasters
  print("Writing species probabilities")
  writeRaster(meanRaster, file = paste0(directoryName, "/processedOutputs/speciesprobability_", taxa,".tiff"), overwrite = TRUE)
  print("Writing species uncertainties")
  writeRaster(seRaster, file = paste0(directoryName, "/processedOutputs/speciesuncertainty_", taxa,".tiff"), overwrite = TRUE)
  
  # Remove alien species for the overall stats
  meanRasterNative <- meanRaster[[!(names(meanRaster) %in% alienSpecies$simpleScientificName)]]
  seRasterNative <- seRaster[[!(names(seRaster) %in% alienSpecies$simpleScientificName)]]
  
  # Can try doing this stuff now SWITCH THIS STUFF TO ANOTHER LOOP IF NECESSARY
  # Need table of means. THis process is followed for uncertainties as well.
  print("Calculating species stats")
  speciesStats <- c(sum(meanRasterNative)/100, sqrt(sum((seRasterNative)^2)))
  names(speciesStats) <- c("probability", "uncertainty")
  
  writeRaster(speciesStats, file = paste0(directoryName, "/processedOutputs/allspeciesstats_", taxa,".tiff"), overwrite = TRUE)
  
  # Get red-listed and ansvars members of species
  redListers <- names(meanRasterNative)[names(meanRasterNative) %in% redList$simpleScientificName]
  ansvarsArterVec <- names(meanRasterNative)[names(meanRasterNative) %in% ansvarsArter$simpleScientificName]
  
  if (length(redListers) > 1) {
    
    print("Calculating threatened species stats")
    redSpeciesStats <- c(sum(meanRasterNative[[redListers]])/100, sqrt(sum((seRasterNative[[redListers]])^2)))
    names(redSpeciesStats) <- c("probability", "uncertainty")
    writeRaster(redSpeciesStats, file = paste0(directoryName, "/processedOutputs/threatenedspeciesstats_", taxa,".tiff"), overwrite = TRUE)
    
  }
  
  if (length(ansvarsArterVec) > 1) {
    
    print("Calculating species of responsibility stats")
    ansvarsArterStats <- c(sum(meanRasterNative[[ansvarsArterVec]])/100, sqrt(sum((seRasterNative[[ansvarsArterVec]])^2)))
    names(ansvarsArterStats) <- c("probability", "uncertainty")
    writeRaster(ansvarsArterStats, file = paste0(directoryName, "/processedOutputs/ansvarsarterstats_", taxa,".tiff"), overwrite = TRUE)
    
  }
  
  return(taxa)
}
)


# 4. Create masking for species richness and bias data
# Here we apply a mask which turns all data in urban areas and water bodies to zero.
# Must prepare spatial data for masking, need to import water and urban areas to fiure out which rasters to mask
corineUnclassified <- get_corine("data/temp/CORINE/EEA.zip", reclassify = FALSE)
maskedCats <-  c("Airports", "Continuous urban fabric", "Discontinuous urban fabric", "Industrial or commercial units",
                 "Green urban areas", "Sport and leisure facilities", "Sea and ocean", "Water bodies", "Water courses")
urbanWater <- ifel(corineUnclassified %in% maskedCats, 1, 0)

rastToMap <- terra::project(baseRaster, "EPSG:32633")
#rastToMap <- rast(crs = "EPSG:32633", resolution = 0.5, extent = ext(baseRaster))

out <- terra::project(urbanWater, rastToMap, method = "average")
mask100 <- ifel(out == 1, 0, 1)
writeRaster(mask100, "localArchive/mask100.tiff", overwrite = TRUE)
rm("corineUnclassified", "urbanWater", "out")
gc()

mask100 <- rast("localArchive/mask100.tiff")

# Also need to mask out areas outside of Norway
norwayBorder2 <- sf::read_sf("data/external/norge_border/Noreg_polygon.shp")
norwayBorderProjected2 <- terra::project(vect(norwayBorder2), "EPSG:32633")

# 6. Apply mask to species richness estimates ans scale them to produce relative richness

rawRichnessFiles <- list.files(paste0(directoryName,"/processedOutputs"), recursive = TRUE, full.names = TRUE, pattern = "*stats")
richnessFiles <- rawRichnessFiles[!grepl("final", rawRichnessFiles)]

for (taxa in taxaToMerge) {

  richnessFilesToProcess <- richnessFiles[grep(paste0("_",taxa), richnessFiles)]

  scaledRasterWithMasks <- lapply(richnessFilesToProcess, FUN = function(x) {
    statsRaster <- rast(x)
    cat("Reprojecting",x)
    statsRasterReproj <- terra::project(statsRaster, "EPSG:32633", method = "mode")

    # Check mask matches
    # get mask correct
    if (ext(mask100) != ext(statsRasterReproj)) {
      statsRasterCropped <- crop(statsRasterReproj, project(mask100, statsRasterReproj), mask = T)
      statsRasterCropped$probability <- statsRasterCropped$probability * project(mask100, statsRasterReproj)
    } else {
      statsRasterCropped <- crop(statsRasterReproj, mask100, mask = T)
      statsRasterCropped$probability <- statsRasterCropped$probability * mask100
    }


    statsRasterCropped <- crop(statsRasterCropped, norwayBorderProjected2, mask = T)
    names(statsRasterCropped)[1:2] <- c("probability", "uncertainty")
    cat("Creating new metrics for",x)
    statsRasterCropped$skalertRikhet <- (statsRasterCropped$probability-minmax(statsRasterCropped$probability)[1])/
      (minmax(statsRasterCropped$probability)[2]-minmax(statsRasterCropped$probability)[1])
    statsRasterCropped$skalertUsikkerhet <- (statsRasterCropped$uncertainty-minmax(statsRasterCropped$uncertainty)[1])/
      (minmax(statsRasterCropped$uncertainty)[2]-minmax(statsRasterCropped$uncertainty)[1])
    newFileName <- gsub(".tiff", "final.tiff", x)
    writeRaster(statsRasterCropped[[c("skalertRikhet", "skalertUsikkerhet")]], filename = newFileName, overwrite = TRUE)
  })

}

# birdsCheck <- rast("data/run_2025-01-06/modelOutputs/processedOutputs/allspeciesstats_birdsfinal.tiff")
# fungiCheck <- rast("data/run_2025-01-06/modelOutputs/processedOutputs/allspeciesstats_fungifinal.tiff")
#
# plot(crop(birdsCheck$skalertRikhet, vect(st_transform(defineRegion("municipality", "5001"), crs = crs(fungiCheck)))))
# plot(crop(fungiCheck$skalertRikhet, vect(st_transform(defineRegion("municipality", "5001"), crs = crs(fungiCheck)))))


# 7. Lastly, we have to produce species richness for the three types of birds they want - waders, ground hatching
# and woodpeckers.
birdGroups <- c("woodpeckers", "groundNestingBirds", "waders")
birdProbabilities <- rast(paste0(directoryName, "/processedOutputs/speciesprobability_birds.tiff"))
birdUncertainties <- rast(paste0(directoryName, "/processedOutputs/speciesuncertainty_birds.tiff"))
birdsToImport <- names(birdProbabilities)
birdChart <- read.csv("data/external/birdTypeList.csv", sep = ";")
groundNesters <- getGbifBackbone(birdChart[birdChart$group == "groundNestingBirds", "simpleName"])
woodpeckers <- getGbifBackbone(birdsToImport) %>% filter(family =="Picidae")
waders <- getGbifBackbone(birdChart[birdChart$group == "wadingBirds", "simpleName"])
identifiers <- data.frame(simpleName = c(groundNesters$species,
                                         woodpeckers$species,
                                         waders$species),
                          group = c(rep("groundNestingBirds", nrow(groundNesters)),
                                    rep("woodpeckers", nrow(woodpeckers)),
                                    rep("waders", nrow(waders))))
identifiers$simpleScientificName <- gsub(" ", "_", identifiers$simpleName)
identifiers <- identifiers %>%
  filter(simpleScientificName %in% birdsToImport)


for (b in birdGroups) {
  birdNames <- identifiers[identifiers$group == b,]
  birdsMeans <- birdProbabilities[[birdNames$simpleScientificName]]
  birdsSEs <- birdUncertainties[[birdNames$simpleScientificName]]
  # Remove alien species for the overall stats
  birdsMeansNative <- birdsMeans[[!(names(birdsMeans) %in% alienSpecies$simpleScientificName)]]
  birdSEsNative <- birdsSEs[[!(names(birdsSEs) %in% alienSpecies$simpleScientificName)]]

  print("Calculating species stats")
  speciesStats <- c(sum(birdsMeansNative)/100, sqrt(sum((birdSEsNative)^2)))
  names(speciesStats) <- c("probability", "uncertainty")

  # Mask the water and urban areas
  speciesStats$probability <- speciesStats$probability * mask100
  speciesStatsCropped <- crop(speciesStats, norwayBorderProjected2)

  speciesStatsCropped$skalertRikhet <- (speciesStatsCropped$probability-minmax(speciesStatsCropped$probability)[1])/
    (minmax(speciesStatsCropped$probability)[2]-minmax(speciesStatsCropped$probability)[1])
  speciesStatsCropped$skalertUsikkerhet <- (speciesStatsCropped$uncertainty-minmax(speciesStatsCropped$uncertainty)[1])/
    (minmax(speciesStatsCropped$uncertainty)[2]-minmax(speciesStatsCropped$uncertainty)[1])

  birdFileName <- paste0(directoryName, "/processedOutputs/allspeciesstats_", b, "final.tiff")
  writeRaster(speciesStatsCropped[[c("skalertRikhet", "skalertUsikkerhet")]], filename = birdFileName, overwrite = TRUE)

}

# # 5. Production of bias data
# # Here we produce the bias data for every species group. We also mask the bias data, so there's no need to do
# # so in step 5.
# 
# # Get bias estimates
# dirsWithBias <- list.files(directoryName, recursive = TRUE, full.names = TRUE, pattern = "*Bias.rds")
# dirsWithBiasSimple <- sapply(strsplit(gsub(paste0(directoryName, "/"), "", dirsWithBias), '\\s*[/]'), `[`, 1)
# # taxaWithBiasData <- c("mammals", "aquaticInsects", "bats")
# dirsNoBias <- dirsWithModel[!(dirsWithModel %in% dirsWithBiasSimple)]
# taxaWithBiasData <- unique(gsub('[[:digit:]]+' ,"",dirsWithBiasSimple))
# for (taxa in taxaWithBiasData) {
#   dirNamesTaxa <- dirsWithBias[grep(paste0("/", taxa), dirsWithBias)]
#   
#   biasRasterList <- lapply(dirNamesTaxa, FUN = rast)
#   biasRaster <- sum(rast(lapply(biasRasterList, `[`, "mean")))
#   
#   if (terra::ext(biasRaster) != ext(baseRaster)) {
#     print(paste0("Reprojecting ", taxa, " data"))
#     biasRaster <- terra::project(biasRaster, baseRaster, method = "mode")
#   }
#   
#   
#   biasRaster$skalertInnsamlingsintensitet <- (biasRaster$sum -minmax(biasRaster$sum)[1])/
#     (minmax(biasRaster$sum)[2]-minmax(biasRaster$sum)[1])
#   biasRasterCropped <- biasRaster * mask100
#   biasRasterMasked <- crop(biasRasterCropped, norwayBorderProjected, mask = T)
#   
#   
#   writeRaster(biasRasterMasked$skalertInnsamlingsintensitet, file = paste0(directoryName, "/processedOutputs/bias_", taxa,".tiff"), overwrite = TRUE, datatype = "FLT4S")
# }
