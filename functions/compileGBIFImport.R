
#' @title \emph{compileGBIFImport}: function to extract relevant data from a GBIF dataset

#' @description This function extracts relevant data from a GBIF download and cuts down to only the necessary columns.
#'
#' @param z A dataset downloaded directly from GBIF.
#' @param columns The columns you would like to keep from the dataset.
#' 
#' @return A dataframe

compileGBIFImport <- function(z, columns = c("acceptedScientificName", "decimalLongitude", "decimalLatitude", "basisOfRecord", "year", 
                                             "datasetKey", "coordinateUncertaintyInMeters", "datasetName")) {
  dataSubset <- z$data
  
  # datasetName does not exist in some species for some reason. In these cases, let it equal NA
  if (!("datasetName" %in% colnames(dataSubset))) {
    dataSubset$datasetName <- NA
  }
  
  # Narrow down to required columns
  return(dataSubset[,columns])
}
