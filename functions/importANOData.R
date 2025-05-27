
#' @title \emph{importANOData}: Import a dataset of species from the ANO database.

#' @description This function downloads data relating to a given list of taxonomicgroups from the Arealrepresentativ naturovervåking (ANO) dataset.
#'
#' @param destinationFolder A character string giving the folder in which your ANO data should be saved,
#' @param regionGeometry An sf object encompassing our region of study, as produced by defineRegion.
#' @param focalTaxon A table containing all taxonomic groups we want to download.
#' @param ANOEndpoint The URL link to the ANO database endpoint.
#' 
#' @return An sf dataframe containing all observations for relevant species in ANO.
#'

# This source file is activated after line 74 of the speciesImport.R script

# Create destination folder for ANO data
importANOData <- function(destinationFolder, regionGeometry, focalTaxon, download = TRUE,
                          ANOEndpoint = "https://nedlasting.miljodirektoratet.no/naturovervaking/naturovervaking_eksport.gdb.zip") {
  
  if (!file.exists(destinationFolder)) {
    dir.create(destinationFolder)
  }
  
  # Download and unzip ANO data from endpoint
  if(download){
    ANOfile <- download.file(url=ANOEndpoint, 
                             destfile=paste0(destinationFolder, "/naturovervaking_eksport.gdb.zip"), mode = "wb")
    unzip(paste0(destinationFolder, "/naturovervaking_eksport.gdb.zip"), 
          exdir = destinationFolder)
    ANOUnzippedFolder <- paste0(destinationFolder, "/naturovervaking_eksport.gdb")
  } else {
    externalANOFolder <- "data/external/naturovervaking_eksport.gpkg.zip"
    
    # Stop code if file doesn't exist locally already
    if (!file.exists(externalANOFolder)) {
      stop("You are requesting a local download of ANO data but do not have the requisite file stored locally. Please save file at ",
           externalANOFolder,"and retry.")
    }
    
    # Otherwise, extract locally
    unzip("data/external/naturovervaking_eksport.gpkg.zip", 
          exdir = destinationFolder)
    ANOUnzippedFolder <- paste0(destinationFolder, "/naturovervaking_eksport.gpkg")
  }
  
  # Import geometry data to later match to species occurrnece
  ANOPoints <- st_read(ANOUnzippedFolder, layer="ANO_SurveyPoint")
  ANOFlateMidpoints <- ANOPoints %>%
    group_by(ano_flate_id) %>% 
    summarize(geometry = st_union(shape)) %>% 
    st_centroid %>%
    st_transform(paste0("EPSG:",crs))
  ANOPoints <- ANOPoints[,c("globalid", "ano_flate_id", "registeringsdato")]
  ANOPoints <- merge(st_drop_geometry(ANOPoints), ANOFlateMidpoints, all.x = T, by = "ano_flate_id")
  
  # Change to our project coordinates
  ANOPoints <- st_transform(st_as_sf(ANOPoints), paste0("EPSG:",crs))
  ANOPoints <- st_intersection(ANOPoints, regionGeometry)
  
  # Import list of species
  ANOSpeciesFull <- st_read(ANOUnzippedFolder, layer="ANO_Art") %>%
    dplyr::filter(parentglobalid %in% ANOPoints$globalid)
  
  #Let's remove NAs from the names
  ANOSpeciesFull <- ANOSpeciesFull[complete.cases(ANOSpeciesFull[, c("art_navn")]), ]
  
  # Narrow down to only species within our taxa
  taxaLegend <- data.frame(speciesName = unique(ANOSpeciesFull$art_navn))
  taxaLegend$taxonKey <- sapply(taxaLegend$speciesName, FUN = function(x) {taxaCheck(x, focalTaxon$key)})
  taxaLegend$taxa <- focalTaxon$taxa[match(taxaLegend$taxonKey, focalTaxon$key)]
  
  # Get GBIF names
  GBIFNameTable <- data.frame(speciesName = unique(ANOSpeciesFull$art_navn))
  GBIFNameTable$GBIFName <- sapply(GBIFNameTable$speciesName, FUN = findGBIFName)
  
  # Add in extra values
  ANOSpeciesFull$year <- format(ANOSpeciesFull$editdate, format="%Y")
  ANOSpeciesFull$taxaKey <- taxaLegend$taxonKey[match(ANOSpeciesFull$art_navn, taxaLegend$speciesName)]
  ANOSpeciesFull$ano_flate_id <- ANOPoints$ano_flate_id[match(ANOSpeciesFull$parentglobalid, ANOPoints$globalid)]
  ANOSpeciesFull <- ANOSpeciesFull[,c("globalid", "parentglobalid", "art_navn", "ano_flate_id", "year", "taxaKey", "creator", "editor", "editdate")]
  
  
  if(any(!is.na(ANOSpeciesFull$taxaKey))){
    ANOSpeciesToUse <- ANOSpeciesFull[!is.na(ANOSpeciesFull$taxaKey),]
    
    keys <- (focalTaxon[focalTaxon$include == TRUE, ])$key
    uniqueKeys <- unique(ANOSpeciesToUse$taxaKey)
    
    # Now we go taxa by taxa
    ANOSpeciesList <- list()
    for (i in 1:length(uniqueKeys)) {
      
      ANOSpecies <- ANOSpeciesToUse %>%
        filter(taxaKey %in% uniqueKeys[i])
      
      ANOSpeciesTable <- as.data.frame(table(ANOSpecies$parentglobalid, ANOSpecies$art_navn), 
                                       stringsAsFactors = FALSE)
      
      # Get the year to match the parent global ID
      ANOYearID <- ANOSpecies[!duplicated(ANOSpecies$parentglobalid),c("parentglobalid", "year")]
      
      # Convert anything more than 2 to a presence
      ANOSpeciesTable$Freq[ANOSpeciesTable$Freq > 0] <- 1
      colnames(ANOSpeciesTable) <- c("globalid", "speciesName", "individualCount")
      ANOSpeciesTable$year <- ANOYearID$year[match(ANOSpeciesTable$globalid, ANOYearID$parentglobalid)]
      ANOSpeciesTable$ano_flate_id <- ANOPoints$ano_flate_id[match(ANOSpeciesTable$globalid, ANOPoints$globalid)]
      
      # Get species count per flate/year combo
      ANOFinal <- ANOSpeciesTable %>%
        group_by(speciesName, year, ano_flate_id) %>%
        summarise(totalCount = sum(individualCount, na.rm = TRUE))
      
      # Turn anything over 1 to 1
      ANOFinal$individualCount <- ifelse(ANOFinal$totalCount >= 1, 1, 0)
      
      # Now get rid of anything except the most recent presence, and if there is no presence the most recent absence
      ANOFinal <- ANOFinal %>%
        arrange(desc(individualCount), desc(year))
      ANOFinal2 <- ANOFinal[!duplicated(ANOFinal[,c("ano_flate_id", "speciesName")]),]
      
      # Add taxa and accepted scientific name
      ANOFinal2$acceptedScientificName <- GBIFNameTable$GBIFName[match(ANOFinal2$speciesName, GBIFNameTable$speciesName)]
      ANOFinal2$taxa <- taxaLegend$taxa[match(ANOFinal2$speciesName, taxaLegend$speciesName)]
      ANOFinal2$processing <- "ANO"
      ANOFinal2$taxonKeyProject <- uniqueKeys[i]
      
      # Save table as list item
      ANOSpeciesList[[i]] <- ANOFinal2[,c("year", "ano_flate_id", "individualCount", "acceptedScientificName", "taxa", "processing", "taxonKeyProject")]
    }
    
    # Put all taxa back together
    ANOSpeciesFullTable <- do.call("rbind", ANOSpeciesList)
    
    # Add geometry data
    ANOData <- merge(ANOSpeciesFullTable, ANOFlateMidpoints, 
                     by="ano_flate_id", all.x=TRUE)
    ANOData <- st_as_sf(ANOData)
    return(ANOData)
  } else {
    return(NULL)
  }
}
