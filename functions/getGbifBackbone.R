#' @title \emph{getGbifBackbone}: get gbif backbone for specifid species names \
#' 
#' @description This function looks up taxa name names in GBIF and obtains the backbone.
#'
#' @param scientificNames A vector of species names which must be found in GBIFs list of accepted scientific name (use findGBIFName to check this)
#' 
#' @return The corresponding dataframe of the taxanomic backbone
#'

getGbifBackbone <- function(scientificNames){
  # first character to lower-case
  ScientificNames <- stringr::str_to_sentence(scientificNames)
  
  # match names with gbif
  speciesNameTable <- as.data.frame(rgbif::name_backbone_checklist(ScientificNames))
  
  # warning message for missing match/species
  missingMatch <- ScientificNames[speciesNameTable$matchType == "NONE"]
  missingSpecies <- ScientificNames[is.na(speciesNameTable$scientificName)]
  if(length(missingMatch) > 0){
    warning(sprintf("No valid match was found for the following species (suggest manual check): %s.", 
                    paste0(missingMatch, collapse = ", ")))
  }
  if(length(missingSpecies) > 0){
    warning(sprintf("No valid species name was found for the following species (suggest manual check): %s.", 
                    paste0(missingSpecies, collapse = ", ")))
  }
  
  # return
  return(speciesNameTable)
}