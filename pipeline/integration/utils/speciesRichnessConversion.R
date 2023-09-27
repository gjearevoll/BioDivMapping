
#### SPECIES RICHNESS CONVERSION ####
library(terra)

# Edit data frames to have the same number of columns
processedDataForCompilation <- lapply(1:length(processedData), FUN = function(x) {
  dataset <- processedData[[x]]
  datasetName <- names(processedData)[x]
  datasetType <- unique(dataset$dataType)
  if (datasetType == "PO") {
    dataset$individualCount <- 1
  }
  datasetShort <- dataset[dataset$individualCount == 1,c("simpleScientificName", "individualCount", "geometry", "taxa")]
  datasetShort
})

# Combine into one data frame and add date accessed
processedDataCompiled <- do.call(rbind, processedDataForCompilation)


###---------------------###
### 2. Raster stacking ####
###---------------------###

# Import environmental data to get an empty raster
blankRaster <- project(rast(paste0(folderName, "/environmentalDataImported.tiff"))[[1]],
                              "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")

# project regionGeometry to match target raster
regionGeometry_buffer <- project(vect(st_buffer(regionGeometry, 1)), blankRaster)

# Group taxa and create list
focalTaxa <- unique(processedDataCompiled$taxa)
taxaRasters <- list()

# Stack the rasters for each taxa
taxaRasters <- lapply(focalTaxa, FUN = function(x) {
  processedTaxaData <- processedDataCompiled[processedDataCompiled$taxa == x,]
  processedPoints <- vect(processedTaxaData)
  speciesNames <- unique(processedPoints$simpleScientificName)
  
  speciesPointList <- lapply(speciesNames, FUN = function(.x) {
    processedPointsSubset <- terra::subset(processedPoints, processedPoints$simpleScientificName == .x)
    pointsRaster <- terra::rasterize(processedPointsSubset, blankRaster, fun = sum)
    pointsRaster <- !is.na(pointsRaster)
    pointsRaster
  })
  names(speciesPointList) <- speciesNames
  rs <- terra::rast(speciesPointList)
  rs1 <- terra::app(rs, sum)
  
  rasterCropped <- terra::crop(rs1, regionGeometry_buffer, snap = "out", mask = TRUE)
  rasterCropped
})

# combine and assign name
taxaRasters <- rast(taxaRasters)
names(taxaRasters) <- focalTaxa