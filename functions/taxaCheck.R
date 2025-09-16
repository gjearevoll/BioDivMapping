
#' @title \emph{taxaCheck}: Check which of our taxa this species belongs to
#' 
#' @description This function looks at a species accepted scientific name in GBIF and checks whether it belongs to any of our taxonomic groups.
#'
#' @param scientificName A species name which must be foudn in GBIFs list of accepted scientific name (use findGBIFName to check this)
#' @param taxaKeys A list of taxonomic keys for the taxa we're importing.
#' 
#' @return The corresponding taxonomic key. If none - we get an NA.
#'
#'
taxaCheck <- function(scientificName, taxaKeys) {
  
  if (str_detect(substr(scientificName,1,1),"[[:lower:]]")) {
    substr(scientificName,1,1) <- toupper(substr(scientificName,1,1))
  }
  
  speciesNameTable <- as.data.frame(rgbif::name_backbone(scientificName))
  
  if (speciesNameTable$matchType == "NONE") {
    warning(sprintf("No valid name was found for %s. Suggest manual check.", {scientificName}))
    taxonKey <- NA
  } else {
  # Assign a taxon key based on what level of taxonomy the key is valid for
    taxonKey <- taxaKeys[taxaKeys %in% speciesNameTable[,grep("Key" ,colnames(speciesNameTable))]]
  # taxonKey <- ifelse(speciesNameTable$kingdomKey %in% taxaKeys, speciesNameTable$kingdomKey,
  #                                       ifelse(speciesNameTable$phylumKey %in% taxaKeys, speciesNameTable$phylumKey,
  #                                              ifelse(speciesNameTable$classKey %in% taxaKeys, speciesNameTable$classKey,
  #                                                     ifelse(speciesNameTable$orderKey %in% taxaKeys, speciesNameTable$orderKey,
  #                                                            ifelse(speciesNameTable$familyKey %in% taxaKeys, speciesNameTable$familyKey,
  #                                                                   ifelse(speciesNameTable$genusKey %in% taxaKeys, speciesNameTable$genusKey,
  #                                                                          ifelse(speciesNameTable$speciesKey %in% taxaKeys, speciesNameTable$speciesKey, NA)))))))
  
  }
  if(!length(taxonKey)) {taxonKey <- NA}
  return(taxonKey)
}
