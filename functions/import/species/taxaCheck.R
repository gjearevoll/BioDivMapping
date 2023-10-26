
taxaCheck <- function(scientificName, taxaKeys) {
  
  speciesNameTable <- as.data.frame(rgbif::name_backbone(scientificName))
  
  if (speciesNameTable$matchType == "NONE") {
    warning(sprintf("No valid name was found for %s. Suggest manual check.", {scientificName}))
    taxonKey <- NA
  } else {
  # Assign a taxon key based on what level of taxonomy the key is valid for
  taxonKey <- ifelse(speciesNameTable$kingdomKey %in% taxaKeys, speciesNameTable$kingdomKey,
                                        ifelse(speciesNameTable$phylumKey %in% taxaKeys, speciesNameTable$phylumKey,
                                               ifelse(speciesNameTable$classKey %in% taxaKeys, speciesNameTable$classKey,
                                                      ifelse(speciesNameTable$orderKey %in% taxaKeys, speciesNameTable$orderKey,
                                                             ifelse(speciesNameTable$familyKey %in% taxaKeys, speciesNameTable$familyKey,
                                                                    ifelse(speciesNameTable$genusKey %in% taxaKeys, speciesNameTable$genusKey, NA))))))
  
  }
  
  return(taxonKey)
}
