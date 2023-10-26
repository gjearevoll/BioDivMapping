

findGBIFName <- function(scientificName) {
  speciesNameTable <- as.data.frame(rgbif::name_backbone(scientificName))
  if ("species" %in% colnames(speciesNameTable)) {
    acceptedName <- speciesNameTable[1, "scientificName"]
  } else {
    acceptedName <- NA
    warning(sprintf("No valid name was found for %s. Suggest manual check.", {scientificName}))
  }
  return(acceptedName)
}

# 
# GBIFImport <- occ_data(scientificName = acceptedName, hasCoordinate = TRUE, limit = 3000, 
#                        geometry = st_bbox(regionGeometry), coordinateUncertaintyInMeters = '0,500')
# as.data.frame(GBIFImport$data)[,c("scientificName", "datasetKey")]
