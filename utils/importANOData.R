
### IMPORT ANO Data ###

# This source file is activated after line 74 of the speciesImport.R script

# Create destination folder for ANO data

tempANOFolder <- "data/temp/ANO"

if (!file.exists(tempANOFolder)) {
  dir.create(tempANOFolder)
}

# Download and unzip ANO data from endpoint
ANOEndpoint <- "https://nedlasting.miljodirektoratet.no/naturovervaking/naturovervaking_eksport.gdb.zip"
ANOfile <- download.file(url=ANOEndpoint, 
                         destfile=paste0(tempANOFolder, "/naturovervaking_eksport.gdb.zip"), mode = "wb")
unzip(paste0(tempANOFolder, "/naturovervaking_eksport.gdb.zip"), 
      exdir = tempANOFolder)
ANOUnzippedFolder <- paste0(tempANOFolder, "/Naturovervaking_eksport.gdb")

# Import geometry data to later match to species occurrnece
ANOPoints <- st_read(ANOUnzippedFolder, layer="ANO_SurveyPoint")
ANOPoints <- ANOPoints[,c("GlobalID", "registeringsdato", "ano_flate_id", "ano_punkt_id", 
                          "hovedoekosystem_punkt",
                          "kartleggingsenhet_1m2", "hovedtype_1m2", "ke_beskrivelse_1m2", 
                          "kartleggingsenhet_250m2", "hovedtype_250m2", "ke_beskrivelse_250m2")]
ANOPoints <- cbind(ANOPoints, st_coordinates(ANOPoints))

# Change to our project coordinates
ANOPoints <- st_transform(ANOPoints, "+proj=longlat +ellps=WGS84")

# Import list of species
ANOSpeciesFull <- st_read(ANOUnzippedFolder, layer="ANO_Art")

# Fix species naming
ANOSpeciesFull$SpeciesName <- paste(toupper(substring(ANOSpeciesFull$art_navn, 1, 1)), 
                                 substring(ANOSpeciesFull$art_navn, 2), 
                                 sep="")
ANOSpeciesFull$simpleScientificName <- gsub(" ", "_", word(ANOSpeciesFull$SpeciesName, 1,2, sep=" "))
ANOSpeciesFull <- ANOSpeciesFull[,c("GlobalID", "ParentGlobalID", "simpleScientificName")]

# Narrow down to only species we are looking for
ANOSpecies <- ANOSpeciesFull[ANOSpeciesFull$simpleScientificName %in% focalSpecies$species,]

# continue only if at least one species is present in ANO
if(nrow(ANOSpecies) > 0) {
  # Start creating a matrix of species occurrence
  ANOSpeciesTable <- as.data.frame(table(ANOSpecies$ParentGlobalID, ANOSpecies$simpleScientificName), 
                                   stringsAsFactors = FALSE)
  # Convert anything more than 2 to a presence
  ANOSpeciesTable$Freq[ANOSpeciesTable$Freq > 0] <- 1
  colnames(ANOSpeciesTable) <- c("GlobalID", "simpleScientificName", "individualCount")
  
  # Add geometry data
  ANOData <- merge(ANOSpeciesTable, ANOPoints[,c("GlobalID")], 
                   by="GlobalID", all.x=TRUE)
  ANOData$dataType <- "PA"
  ANOData$name <- "ANO Data"
  ANOData$geometry <- ANOData$SHAPE
  ANOData$DWCEndpoint <- ANOEndpoint
  ANOData$basisofRecord <- "HUMAN_OBSERVATION"
  ANOData$type <- "SAMPLING_EVENT"
  ANOData$datasetKey <- ANOData$coordinateUncertaintyInMeters <- ANOData$year <- NA
  
  ANOData <- st_as_sf(ANOData)
  st_crs(ANOData) <- "+proj=longlat +ellps=WGS84"
  
  # Crop to relevant region
  ANOData <- st_intersection(ANOData, regionGeometry)
  ANOData <- st_transform(ANOData, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
  
  rm("ANOSpecies", "ANOSpeciesTable", "ANOSpeciesFull", "ANOPoints", "ANOUnzippedFolder", "tempANOFolder")
}