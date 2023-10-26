
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
    pred_lt("coordinateUncertaintyInMeters", 100),
    type = "and"
  ) %>% 
    occ_download_meta
  return(download_key)
}
