
#' @title \emph{processANOData}: Standardises an ANO dataset for use in species models.

#' @description This function takes our ANO dataset and standardises it for use alongside the other datasets downloaded from GBIF.
#'
#' @param ANODataset A dataset as downloaded using the importANO function
#' 
#' @return A new processed dataset, standardised for further use.
#'
#' @import sf
#' 


processANOData <- function(ANODataset, crs) {
  
  
  # continue only if at least one species is present in ANO
  if(nrow(ANODataset) > 0) {
    # Start creating a matrix of species occurrence
    
    ANODataset$dataType <- "PA"
  }
  
  ANODataset <- ANODataset[,c("acceptedScientificName", "individualCount", "geometry", "dataType", "taxa", "year", "taxonKeyProject")]
  ANODataset <- ANODataset[!is.na(ANODataset$acceptedScientificName),]
  ANODataset <- st_as_sf(ANODataset, crs = crs)
  
  
  # Remove any duplicated observations from the same year at the same place
  arrangedData <- ANODataset[order(ANODataset$individualCount, decreasing = TRUE),]
  newDataset2 <- arrangedData[!duplicated(arrangedData[,c("geometry", "year", "acceptedScientificName")]),]
  
  return(ANODataset)
}
