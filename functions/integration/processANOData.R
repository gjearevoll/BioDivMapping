


processANOData <- function(ANODataset) {
  
  
  # continue only if at least one species is present in ANO
  if(nrow(ANODataset) > 0) {
    # Start creating a matrix of species occurrence
    
    ANODataset$dataType <- "PA"
  }
  
  ANODataset <- ANODataset[,c("acceptedScientificName", "individualCount", "geometry", "dataType", "taxa", "year")]
  ANODataset <- ANODataset[!is.na(ANODataset$acceptedScientificName),]
  ANODataset <- st_as_sf(ANODataset)
  ANODataset <- st_transform(ANODataset, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
  return(ANODataset)
}
