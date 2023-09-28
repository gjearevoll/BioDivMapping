
### Compile GBIF Import ###

# Function that cuts down the GBIF Import to only necessary columns


compileGBIFImport <- function(z) {
  dataSubset <- z$data
  
  # datasetName does not exist in some species for some reason. In these cases, let it equal NA
  if (!("datasetName" %in% colnames(dataSubset))) {
    dataSubset$datasetName <- NA
  }
  dataSubset[,c("acceptedScientificName", "decimalLongitude", "decimalLatitude", "basisOfRecord", "year", 
                "datasetKey", "coordinateUncertaintyInMeters", "datasetName")]
}
