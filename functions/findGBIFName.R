
#' @title \emph{findGBIFName}: This function finds the name that a species is listed under in GBIF

#' @description This function looks up a character string in GBIF and returns the acceptedScientificName that the character string corresponds to.
#'
#' @param scientificName A single character string with a species scientific name.
#' 
#' @return Another character string with the accepted scientific name as listed in GBIF
#' 

findGBIFName <- function(scientificName) {
  
  # Get rid of NAs immediately
  if(is.na(scientificName)) return(NA)
  
  if (str_detect(substr(scientificName,1,1),"[[:lower:]]")) {
    substr(scientificName,1,1) <- toupper(substr(scientificName,1,1))
  }
  speciesNameTable <- as.data.frame(rgbif::name_backbone(scientificName))
  if ("species" %in% colnames(speciesNameTable)) {
    acceptedName <- speciesNameTable[1, "scientificName"]
  } else {
    acceptedName <- NA
    warning(sprintf("No valid name was found for %s. Suggest manual check.", {scientificName}))
  }
  return(acceptedName)
}
