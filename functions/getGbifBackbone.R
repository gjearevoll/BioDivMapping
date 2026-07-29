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
  
  # match names with gbif (in batches to avoid time-out error)
  batches <- split(ScientificNames, ceiling(seq_along(ScientificNames)/200))
  
  # retry up to <tries> times if gbif keeps timing out 
  # each fail, make sleep longer and batch smaller
  speciesNameTable <- data.frame()
  message("Obtaining GBIF backbone")
  
  for (b in seq_along(batches)) {
    for (i in seq_len(5)) {
      message(sprintf("Batch %d/%d, try %d/5", b, length(batches), i))
      result <- tryCatch(
        as.data.frame(rgbif::name_backbone_checklist(batches[[b]], sleep = 2^(i - 1), bucket_size = 300 %/% i)),
        error = function(e) NULL
      )
      if (!is.null(result)) break
    }
    speciesNameTable <- dplyr::bind_rows(speciesNameTable, result)
  }
  
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