
#### SPECIES RICHNESS CONVERSION ####

# The following script converts our series of point data into a species richness raster field

library(dplyr)
library(sf)

###-----------------###
### 1. Preparation ####
###-----------------###

# First thing is to bring in imported data
if (!exists("dateAccessed")) {
  dateAccessed <- as.character(Sys.Date())
}

# Get folder names
folderName <- paste0("data/run_", dateAccessed)
tempFolderName <- paste0(folderName, "/temp")

processedDataList <- readRDS(paste0("data/run_", dateAccessed, "/temp/speciesDataProcessed.RDS"))

# Edit data frames to have same number of columns
processedDataForCompilation <- lapply(1:length(processedDataList), FUN = function(x) {
  dataset <- processedDataList[[x]]
  datasetName <- names(processedDataList)[x]
  datasetType <- unique(dataset$dataType)
  if (datasetType == "PO") {
    dataset$individualCount <- 1
  }
  datasetShort <- dataset[dataset$individualCount == 1,c("simpleScientificName", "individualCount", "geometry", "taxa")]
  datasetShort
}
)

# Combine into one data frame and add date accessed
processedDataCompiled <- do.call(rbind, processedDataForCompilation)


###---------------------###
### 2. Raster stacking ####
###---------------------###

# Import environmental data to get an empty raster
blankRaster <- projectRaster(raster(rast(paste0(tempFolderName, "/environmentalDataImported.tiff"))$aspect), 
                             crs= "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")

# Group taxa and create list
focalTaxa <- unique(processedDataCompiled$taxa) 
taxaRasters <- list()

# Stack the rasters for each taxa
for (i in focalTaxa){
  processedTaxaData <- processedDataCompiled[processedDataCompiled$taxa == i,]
  processedPoints <- as(processedTaxaData, "Spatial")
  speciesNames <- unique(processedPoints$simpleScientificName)
  # We now turn every pixel that has more than 1 occurrence of a species in it into a 1, and then 
  # add these rasters together to get total species richness in that square
    speciesPointList <- lapply(speciesNames, FUN = function(x) {
    processedPointsSubset <- processedPoints[processedPoints$simpleScientificName == x,]
    pointsRaster <- rasterize(processedPointsSubset, raster(blankRaster), processedPoints$individualCount, fun = sum)
    values(pointsRaster) <- ifelse(is.na(values(pointsRaster)), 0, 1)
    pointsRaster
  })
  names(speciesPointList) <- speciesNames
  rs <- stack(speciesPointList)
  rs1 <- calc(rs, sum)
  taxaRasters[[i]] <- rs1
}
names(taxaRasters) <- focalTaxa

# For now we're just savign this in visualisation data
writeRaster(taxaRasters, "visualisation/hotspotMaps/data/speciesRichnessData.tiff", overwrite=TRUE)






