# Simultaneous processing

# Import all functions
.libPaths(c("/cluster/projects/nn11017k/R"))
args <- commandArgs(trailingOnly = TRUE)
dateAccessed <- as.character(args[1])
task <- as.character(args[2])
dateAccessed <- "2025-09-04"
task <- "processing"

sapply(list.files("functions", full.names = TRUE, recursive = TRUE), source)
library(stringr)
library(dplyr)
# Define directory name
directoryName <- paste0("data/run_", dateAccessed, "/modelOutputs")


# 1. Preparation
# We need to get lists of files to process, and identify which segments of species did not run either models or
# predictions. We also need to import a large number of other data as well to edit our final products.

# add simpleScientificName column
redList <- readRDS(paste0("data/run_", dateAccessed,"/redList.RDS"))
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


dirsToCheck <- dirNamesShort


modelsWRichness <- list.files(paste0(directoryName, "/", dirsToCheck), recursive = TRUE, 
                              full.names = TRUE, pattern = "*Richness.rds")
modelsWRichnessSimple <- unique(sapply(strsplit(gsub(paste0(directoryName, "/"), '', modelsWRichness), '\\s*/'), `[`, 1))
modelsNoRichness <- dirsToCheck[!(dirsToCheck %in% modelsWRichnessSimple)]
dirsNoRichness <- dirNamesShort[!(dirNamesShort %in% modelsWRichnessSimple)]
taxaList <- unique(gsub('[[:digit:]]+', '', str_replace(dirNames, paste0('(.*?)', directoryName, '/(.*?)'), '\\1')))
taxaWithData <- unique(gsub('[[:digit:]]+', '', modelsWRichnessSimple))
relevantDirNames <- paste0(directoryName, "/", modelsWRichnessSimple)

# 2. Condensing of segments
# Here we turn our 10ish RDS files into one more easily useable tiff file, which can then be uploaded easily
# in step 2.

# Get everything we need as an empty list
taxa <- "vascularPlants"

dirNamesTaxa <- relevantDirNames[grep(paste0("/",taxa), relevantDirNames)]
oneRast <- readRDS(list.files(dirNamesTaxa[1], full.names = TRUE, recursive = TRUE, pattern = "ss.rds")[1])
meanRaster <- seRaster <- rast(ext = ext(oneRast), res = res(oneRast))
crs(meanRaster) <- crs(seRaster) <- crs(oneRast)

if (task == "processing")
{
  # Start cycling through species groups
  for (i in seq_along(dirNamesTaxa)) {
    taxaGroup <- dirNamesTaxa[i]
    
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
    #print(paste0(taxaGroup))
  }
  
} else if (task == "summarising") {
  
  # 3. Production of compiled data
  # Here we produce the first 2 data packages. The first contains all probabilities and uncertainties for individual
  # species. The second contains all species stats (compiled richness and uncertainty) per species group.
  # It does this for all species, and (if applicable) for all threatened species and ansvarsarter.
  
  # here we need to group all insects as one, so we need to reduce the list somewhat.
  
  if (!dir.exists(paste0(directoryName, "/processedOutputs"))) {dir.create(paste0(directoryName, "/processedOutputs"))}
  
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
    
    # Check for a predictor species
    predictorSpecies <- names(meanRaster)[duplicated(names(meanRaster))]
    
    # Isolate predictor species if it exists 
    if (length(predictorSpecies) > 1) {
      allPredSpecies <- meanRaster[unique(predictorSpecies)]
      predictorSpeciesRast <- mean(allPredSpecies)
      
      # Now replace all predictor species with just mean of pred species
      restOfSpecies <- meanRaster[[!(names(meanRaster) %in% predictorSpecies)]]
      meanRaster <- c(restOfSpecies, predictorSpeciesRast)
    }
    
    
    # Save individual species rasters
    print("Writing species probabilities")
    writeRaster(meanRaster, file = paste0(directoryName, "/processedOutputs/speciesprobability_", taxa,".tiff"), overwrite = TRUE)
    print("Writing species uncertainties")
    writeRaster(seRaster, file = paste0(directoryName, "/processedOutputs/speciesuncertainty_", taxa,".tiff"), overwrite = TRUE)
    
    # Remove alien species for the overall stats
    if (exists("alienSpecies")) {
      meanRaster <- meanRaster[[!(names(meanRaster) %in% alienSpecies$simpleScientificName)]]
      seRaster <- seRaster[[!(names(seRaster) %in% alienSpecies$simpleScientificName)]]
    }
    
    # Can try doing this stuff now SWITCH THIS STUFF TO ANOTHER LOOP IF NECESSARY
    # Need table of means. THis process is followed for uncertainties as well.
    print("Calculating species stats")
    speciesStats <- c(sum(meanRaster)/100, sqrt(sum((seRaster/100)^2)))
    names(speciesStats) <- c("probability", "uncertainty")
    
    writeRaster(speciesStats, file = paste0(directoryName, "/processedOutputs/allspeciesstats_", taxa,".tiff"), overwrite = TRUE)
    
    # Get red-listed and ansvars members of species
    
    if (exists ("redList")) {
      redListers <- names(meanRaster)[names(meanRaster) %in% redList$simpleScientificName]
      if (length(redListers) > 1) {
        print("Calculating threatened species stats")
        redSpeciesStats <- c(sum(meanRaster[[redListers]])/100, sqrt(sum((seRaster[[redListers]]/100)^2)))
        names(redSpeciesStats) <- c("probability", "uncertainty")
        writeRaster(redSpeciesStats, file = paste0(directoryName, "/processedOutputs/threatenedspeciesstats_", taxa,".tiff"), overwrite = TRUE)
      }}
    
    if (exists("ansvarsArter")) {
      ansvarsArterVec <- names(meanRaster)[names(meanRaster) %in% ansvarsArter$simpleScientificName]
      if (length(ansvarsArterVec) > 1) {
        print("Calculating species of responsibility stats")
        ansvarsArterStats <- c(sum(meanRaster[[ansvarsArterVec]])/100, sqrt(sum((seRaster[[ansvarsArterVec]]/100)^2)))
        names(ansvarsArterStats) <- c("probability", "uncertainty")
        writeRaster(ansvarsArterStats, file = paste0(directoryName, "/processedOutputs/ansvarsarterstats_", taxa,".tiff"), overwrite = TRUE)
      }}
    
    return(taxa)
  }
  )
  
}


stop ("Enough for now")
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
norwayBorderProjected2 <- terra::project(vect(norwayBorder2), "EPSG:25833")

# 6. Apply mask to species richness estimates ans scale them to produce relative richness

rawRichnessFiles <- list.files(paste0(directoryName,"/processedOutputs"), recursive = TRUE, full.names = TRUE, pattern = "*stats")
richnessFiles <- rawRichnessFiles[!grepl("final", rawRichnessFiles)]

for (taxa in taxaToMerge) {
  
  richnessFilesToProcess <- richnessFiles[grep(paste0("_",taxa), richnessFiles)]
  
  scaledRasterWithMasks <- lapply(richnessFilesToProcess, FUN = function(x) {
    statsRaster <- rast(x)
    cat("\nReprojecting",x)
    statsRasterReproj <- terra::project(statsRaster, terra::project(baseRaster, "EPSG:32633"), method = "mode")
    
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
    cat("\nCreating new metrics for",x)
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

