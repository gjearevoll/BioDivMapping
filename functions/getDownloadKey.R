
#' @title \emph{getDownloadKey}: Get a download key for a scheduled GBIF download

#' @description This function initiates a download of a given group of taxa through GBIF.
#'
#' @param taxa A vector of GBIF taxon keys.
#' @param regionGeometry An sf object encompassing our region of study, as produced by defineRegion.
#' 
#' @return A download key for a GBIF download.
#'

getDownloadKey <- function(taxa, regionGeometry) {
  
  # Log in to GBIF
  options(gbif_user=rstudioapi::askForPassword("my gbif username"))
  options(gbif_email=rstudioapi::askForPassword("my registred gbif e-mail"))
  options(gbif_pwd=rstudioapi::askForPassword("my gbif password"))
  
  if (!is.numeric(taxa)) {
    keys <- as.integer()
    for(i in 1:length(species)){
      focalSpecies <- gsub("_", " ", species[i])
      keyTable <- name_suggest(q=focalSpecies, rank='species')
      keys[i] <- keyTable$data$key[1]
    }
  } else {
    keys <- taxa
  }
  
  # Get taxon keys for species
  
  download_key <- occ_download(
    pred_in("taxonKey", keys),
    pred("geometry", st_as_text(regionGeometry[[1]])),
    pred_lte("coordinateUncertaintyInMeters", 100),
    type = "and"
  ) 
  return(download_key)
}
