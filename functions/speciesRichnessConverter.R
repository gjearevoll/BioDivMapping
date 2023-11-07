#' @title \emph{speciesRichnessConverter}: Cinverts a list of presence datasets from GBIF into a species richness raster

#' @description We have a huge list of different species presences, this takes them and figures out the species richness over a given raster at different pixels.
#'
#' @param regionGeometry An sf object encompassing our region of study, as produced by defineRegion.
#' @param presenceData An sf dataset containing all presences for a given group of species at different points.
#' @param blankRaster A blank raster with the appropriate resolution on which to project the species richness.
#' 
#' @return A stacked raster with a species richness raster for each taxonomic group.
#'

speciesRichnessConverter <- function(regionGeometry, presenceData, blankRaster) {
  
  # project regionGeometry to match target raster
  regionGeometry_buffer <- terra::project(vect(st_buffer(regionGeometry, 1)), blankRaster)
  
  # Group taxa and create list
  focalTaxa <- unique(presenceData$taxa)
  
  # define function to count unique species
  n_unique <- function(x) length(unique(x))
  
  # Stack the rasters for each taxa
  taxaRasters <- lapply(focalTaxa, FUN = function(x) {
    rs1 <- presenceData[presenceData$taxa == x,] |>
      terra::rasterize(blankRaster, 
                       field = "acceptedScientificName",
                       fun = n_unique)
    ifel(is.na(rs1), 0, rs1) 
  }) |> 
    setNames(focalTaxa) |> # assign names
    rast() |> # combine into multi-layer raster
    terra::crop(regionGeometry_buffer, snap = "out", mask = TRUE)  # crop

  # convert to sf data.frame
  taxaDF <- lapply(taxaRasters, FUN = function(taxaLayer) {
    as.data.frame(taxaLayer, xy = TRUE, na.rm = TRUE) |> 
      st_as_sf(coords = c("x", "y"), crs = st_crs(taxaLayer))
  }) |> 
    setNames(focalTaxa) # assign names
  
  # combine to list and return output
  return(list(rasters = taxaRasters, richness = taxaDF))
}