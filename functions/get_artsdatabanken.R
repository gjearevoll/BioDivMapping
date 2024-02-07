
#' @title \emph{get_artsdatabanken}: This function downloads kalkinnhold data from an ADB endpoint

#' @description This function downloads the kalkinnhold data from artsdatabanken and then applies named values to create proper categories.
#'
#' @return An aggregated raster containing kalkinnhold data for Norway.
#'
#' @import terra
#' 


get_artsdatabanken <- function() {

  focalURL <- "https://data.artsdatabanken.no/Natur_i_Norge/Natursystem/Milj%C3%B8variabler/Kalkinnhold/grid.32633.tif"
  message(sprintf("Downloading, %s raster from artsdatabanken.", parameter))
  raster <- terra::rast(focalURL)
  rasterFactor <- as.factor(raster)
  
  catLegend <- data.frame(levels = c("0", "14", "154", "98", "210", "42"), 
                          levelNames = c("no data", "svært kalkfattig", "litt kalkrik",
                                         "svæk intermediær", "svært kalkrik",
                                         "temmelig kalkfattig"))
  levelsTable <- merge(levels(rasterFactor)[[1]], catLegend, by.x = "ID", by.y = "levels", all.x = TRUE)
  levels(rasterFactor) <- levelsTable[,c(1,3)]
  
  return(rasterFactor)
  
}
