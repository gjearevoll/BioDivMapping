
### IMPORT ANO Data ###

# This source file is activated after line 74 of the speciesImport.R script

# Create destination folder for ANO data
importANOData <- function(occurrences, destinationFolder, regionGeometry, focalTaxon,
                          ANOEndpoint = "https://nedlasting.miljodirektoratet.no/naturovervaking/naturovervaking_eksport.gdb.zip") {
  
  if (!file.exists(destinationFolder)) {
    dir.create(destinationFolder)
  }
  
  # Download and unzip ANO data from endpoint
  ANOfile <- download.file(url=ANOEndpoint, 
                           destfile=paste0(destinationFolder, "/naturovervaking_eksport.gdb.zip"), mode = "wb")
  unzip(paste0(destinationFolder, "/naturovervaking_eksport.gdb.zip"), 
        exdir = destinationFolder)
  ANOUnzippedFolder <- paste0(destinationFolder, "/Naturovervaking_eksport.gdb")
  
  # Import geometry data to later match to species occurrnece
  ANOPoints <- st_read(ANOUnzippedFolder, layer="ANO_SurveyPoint")
  ANOPoints <- ANOPoints[,c("GlobalID", "registeringsdato")]
  ANOPoints <- cbind(ANOPoints, st_coordinates(ANOPoints))
  
  # Change to our project coordinates
  ANOPoints <- st_transform(ANOPoints, "+proj=longlat +ellps=WGS84")
  ANOPoints <- st_intersection(ANOPoints, regionGeometry)
  
  # Import list of species
  ANOSpeciesFull <- st_read(ANOUnzippedFolder, layer="ANO_Art") %>%
    filter(ParentGlobalID %in% ANOPoints$GlobalID)
  
  
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
  ANOSpeciesFull <- ANOSpeciesFull[,c("GlobalID", "ParentGlobalID", "art_navn", "year", "taxaKey")]
  
  
  ANOSpecies <- ANOSpeciesFull[!is.na(ANOSpeciesFull$taxaKey),]
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
  
  
  # Add geometry data
  ANOData <- merge(ANOSpeciesTable, ANOPoints[,c("GlobalID")], 
                   by="GlobalID", all.x=TRUE) %>%
    rename(geometry = SHAPE)
  return(ANOData)
  
}
