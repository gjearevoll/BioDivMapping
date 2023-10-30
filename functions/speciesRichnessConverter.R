
#' @title \emph{speciesRichnessConverter}: Cinverts a list of presence datasets from GBIF into a species richness raster

#' @description We have a huge list of different species presences, this takes them and figures out the species richness over a given raster at different pixels.
#'
#' @param regionGeometry An sf object encompassing our region of study, as produced by defineRegion.
#' @param presenceData An sf dataset containing all presences for a given group of species at different points.
#' @param blankRaster A blank raster with the appropriate resolution on which to project the species richness.
#' 
#' @return A stackted raster with a species richness raster for each taxonomic group.
#'


speciesRichnessConverter <- function(regionGeometry, presenceData, blankRaster) {
  
  
  # project regionGeometry to match target raster
  regionGeometry_buffer <- project(vect(st_buffer(regionGeometry, 1)), blankRaster)
  
  # Group taxa and create list
  focalTaxa <- unique(presenceData$taxa)
  taxaRasters <- list()
  
  # Stack the rasters for each taxa
  taxaRasters <- lapply(focalTaxa, FUN = function(x) {
    processedTaxaData <- presenceData[presenceData$taxa == x,]
    processedPoints <- vect(processedTaxaData)
    speciesNames <- unique(processedPoints$acceptedScientificName)
    
    speciesPointList <- lapply(speciesNames, FUN = function(.x) {
      processedPointsSubset <- terra::subset(processedPoints, processedPoints$acceptedScientificName == .x)
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
  
  
  taxaDF <- lapply(taxaRasters, FUN = function(taxaLayer) {st_as_sf(raster::as.data.frame(taxaLayer,xy=TRUE), 
                                                                    coords = c("x", "y"), 
                                                                    crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")})
  names(taxaDF) <- focalTaxa
  
  output <- list(rasters = taxaRasters, richness = taxaDF)
  
  
  return(output)
}
