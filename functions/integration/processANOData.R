


processANOData <- function(ANODataset, regionGeometry, focalSpecies) {
  
  
  # continue only if at least one species is present in ANO
  if(nrow(ANODataset) > 0) {
    # Start creating a matrix of species occurrence
    
    ANODataset$dataType <- "PA"
    
    ANODataset <- st_as_sf(ANODataset)
    st_crs(ANODataset) <- "+proj=longlat +ellps=WGS84"
    
    # Crop to relevant region
    ANODataset <- st_intersection(ANODataset, regionGeometry)
    ANODataset <- st_transform(ANODataset, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ") %>% 
      rename(geometry = SHAPE)
  }
  
  
  ANODataset$taxa <- focalSpecies$taxonomicGroup[match(ANODataset$simpleScientificName, focalSpecies$species)]
  ANODataset <- ANODataset[,c("simpleScientificName", "individualCount", "geometry", "dataType", "taxa")]
  return(ANODataset)
}