
#' @title \emph{getDownloadKey}: Get a download key for a scheduled GBIF download

#' @description This function initiates a download of a given group of taxa through GBIF.
#'
#' @param taxa A vector of GBIF taxon keys.
#' @param regionGeometry An sf object encompassing our region of study, as produced by defineRegion.
#' 
#' @return A download key for a GBIF download.
#'

getDownloadKey <- function(taxa, regionGeometry) {
  # Check if credentials are stored in .Renviron
  gbif_user <- Sys.getenv("gbif_user", "")
  gbif_email <- Sys.getenv("gbif_email", "")
  gbif_pwd <- Sys.getenv("gbif_pwd", "")
  
  # If not found in .Renviron, check in options, or ask the user
  if (gbif_user == "") {
    gbif_user <- getOption("gbif_user", rstudioapi::askForPassword("my GBIF username"))
  }
  if (gbif_email == "") {
    gbif_email <- getOption("gbif_email", rstudioapi::askForPassword("my registered GBIF e-mail"))
  }
  if (gbif_pwd == "") {
    gbif_pwd <- getOption("gbif_pwd", rstudioapi::askForPassword("my GBIF password"))
  }
  
  # Set the credentials in options (if they were asked from the user)
  options(gbif_user=gbif_user)
  options(gbif_email=gbif_email)
  options(gbif_pwd=gbif_pwd)
  
  if (!is.numeric(taxa)) {
    keys <- as.integer()
    for(i in 1:length(taxa)){
      focalSpecies <- gsub("_", " ", taxa[i])
      keyTable <- name_suggest(q = focalSpecies, rank='species')
      keys[i] <- keyTable$data$key[1]
    }
  } else {
    keys <- taxa
  }
  
  # Get taxon keys for species
  
  download_key <- occ_download(
    pred_in("taxonKey", keys[!is.na(keys)]),
    pred("geometry", st_as_text(regionGeometry[[1]])),
    pred_lte("coordinateUncertaintyInMeters", 250),
    type = "and"
  ) 
  return(download_key)
}
