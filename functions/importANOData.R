
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
  ANOPoints <- ANOPoints[,c("GlobalID", "registeringsdato")]
  ANOPoints <- cbind(ANOPoints, st_coordinates(ANOPoints))
  
  # Change to our project coordinates
  ANOPoints <- st_transform(ANOPoints, "+proj=longlat +ellps=WGS84")
  ANOPoints <- st_intersection(ANOPoints, regionGeometry)
  
  # Import list of species
  ANOSpeciesFull <- st_read(ANOUnzippedFolder, layer="ANO_Art") %>%
    dplyr::filter(ParentGlobalID %in% ANOPoints$GlobalID)
  
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
  ANOSpeciesFull$year <- format(ANOSpeciesFull$EditDate, format="%Y")
  ANOSpeciesFull$taxaKey <- taxaLegend$taxonKey[match(ANOSpeciesFull$art_navn, taxaLegend$speciesName)]
  ANOSpeciesFull <- ANOSpeciesFull[,c("GlobalID", "ParentGlobalID", "art_navn", "year", "taxaKey", "Creator", "Editor", "EditDate")]
  
  
  if(any(!is.na(ANOSpeciesFull$taxaKey))){
    ANOSpeciesToUse <- ANOSpeciesFull[!is.na(ANOSpeciesFull$taxaKey),]
    
    keys <- (focalTaxon[focalTaxon$include == TRUE, ])$key
    uniqueKeys <- unique(ANOSpeciesToUse$taxaKey)
    
    # Now we go taxa by taxa
    ANOSpeciesList <- list()
    for (i in 1:length(uniqueKeys)) {
      
      ANOSpecies <- ANOSpeciesToUse %>%
        filter(taxaKey %in% uniqueKeys[i])
      
      ANOSpeciesTable <- as.data.frame(table(ANOSpecies$ParentGlobalID, ANOSpecies$art_navn), 
                                       stringsAsFactors = FALSE)
      
      # Get the year to match the parent global ID
      ANOYearID <- ANOSpecies[!duplicated(ANOSpecies$ParentGlobalID),c("ParentGlobalID", "year")]
      
      # Convert anything more than 2 to a presence
      ANOSpeciesTable$Freq[ANOSpeciesTable$Freq > 0] <- 1
      colnames(ANOSpeciesTable) <- c("GlobalID", "speciesName", "individualCount")
      ANOSpeciesTable$year <- ANOYearID$year[match(ANOSpeciesTable$GlobalID, ANOYearID$ParentGlobalID)]
      
      # Add taxa and accepted scientific name
      ANOSpeciesTable$acceptedScientificName <- GBIFNameTable$GBIFName[match(ANOSpeciesTable$speciesName, GBIFNameTable$speciesName)]
      ANOSpeciesTable$taxa <- taxaLegend$taxa[match(ANOSpeciesTable$speciesName, taxaLegend$speciesName)]
      ANOSpeciesTable$processing <- "ANO"
      ANOSpeciesTable$taxonKeyProject <- uniqueKeys[i]
      
      # Save table as list item
      ANOSpeciesList[[i]] <- ANOSpeciesTable
    }
    
    # Put all taxa back together
    ANOSpeciesFullTable <- do.call("rbind", ANOSpeciesList)
    
    # Add geometry data
    ANOData <- merge(ANOSpeciesFullTable, ANOPoints[,c("GlobalID")], 
                     by="GlobalID", all.x=TRUE) %>%
      rename(geometry = SHAPE)
    return(ANOData)
  } else {
    return(NULL)
  }
}
