
#' @title \emph{processFieldNotes}: Turns a presence only dataset into a presence absence dataset for surveyed data.

#' @description Some datasets have been reduced by GBIF to presence-only, despite the fact they are in fact presence-absence datastes. This function downloads these datasets directly from the source and adds the absences back in.
#'
#' @param focalEndpoint An endpoint through which the original dataset can be downloaded.
#' @param tempFolderName A directory in which to save the data downloaded directly from the source.
#' @param datasetName The name of the dataset to be downloaded.
#' @param regionGeometry An sf object encompassing our region of study, as produced by defineRegion.
#' @param focalTaxon A dataframe giving the key and names of each taxonomic group we are downloading.
#' 
#' @return A new dataset with absences added back in.
#'
#' @import sf
#' 
#' 
#' 
processVannmiljo <- function(focalEndpoint, tempFolderName, datasetName, regionGeometry, 
                              focalTaxon, crs, coordUncertainty, yearToStart) {
  
  
  # Get the relevant endpoint
  
  # Download and unzip file in temp folder
  #options(timeout=100)
  download.file(focalEndpoint, paste0(tempFolderName,"/", datasetName ,".zip"), mode = "wb")
  unzip(paste0(tempFolderName,"/", datasetName ,".zip"), exdir = paste0(tempFolderName,"/",  datasetName))
  
  # Load in occurrence data
  occurrence <- read.delim(paste0(tempFolderName,"/", datasetName ,"/occurrence.txt"))
  occurrence <- occurrence[!grepl("Marin", occurrence$samplingProtocol),]
  occurrence <- occurrence[!grepl("Ukjent", occurrence$samplingProtocol),]
  
  # make sure we have a year column
  if (!("year" %in% colnames(occurrence))) {
    occurrence$year <- substr(occurrence$eventDate,1,4)
  }
  occurrence <- occurrence %>%
    filter(year >= yearToStart & !is.na(year))
  
  # Remove data with bad coord uncertainty
  if( !is.na(coordUncertainty)) {
    occurrence <- occurrence %>%
      filter(coordinateUncertaintyInMeters <= coordUncertainty)
  }

  
  # Create surveyed species
  surveyedSpecies <- unique(occurrence$scientificName[occurrence$taxonRank == ""])
  surveyedSpecies2 <- str_subset(surveyedSpecies, "^\\w+\\s\\w+$")
  
  # Buold a species table
  speciesLegend <- data.frame(surveyedSpecies = surveyedSpecies2, 
                              acceptedScientificName = sapply(surveyedSpecies2, FUN = findGBIFName),
                              taxonKey = sapply(surveyedSpecies2, FUN = function(x) {taxaCheck(x, focalTaxon$key)})) %>%
    filter(!is.na(taxonKey))
  
  # Get the most recent eventID (survey) at each parentEventID (survey)
  eventIDs <- occurrence[!duplicated(occurrence[,c("eventID", "parentEventID", "year")]),c("eventID", "parentEventID", "year")]
  arrangedEvents <- eventIDs[order(eventIDs$year, decreasing = TRUE),]
  newDataset2 <- arrangedEvents[!duplicated(arrangedEvents[,c("parentEventID")]),]
  
  # Find only eventIDs within our regionGeometry
  eventLocations <- occurrence %>%
    filter(!is.na(decimalLatitude) & !is.na(decimalLongitude) & eventID %in% newDataset2$eventID) %>%
    dplyr::select(decimalLatitude, decimalLongitude, eventID, samplingProtocol) %>%
    distinct()
  eventLocationsSF <- st_as_sf(eventLocations,                         
                               coords = c("decimalLongitude", "decimalLatitude"),
                               crs = "+proj=longlat +ellps=WGS84")
  eventLocationsSF <- st_transform(eventLocationsSF, crs = crs)
  eventLocationsSF <- st_intersection(eventLocationsSF, regionGeometry)
  
  # At this point we may find that there are no relevant points from this dataset available - 
  # in this case we want to finish the function early
  if (nrow(eventLocationsSF) == 0) {
    return(NULL)
  }

  # Get a dates table to match years to events
  eventDates <- occurrence %>%
    dplyr::select(year, eventID) %>%
    distinct()
  
  # Start constructing table- do this per sampling protocol
  samplingProtocols <- unique(eventLocationsSF$samplingProtocol)
  eventList <- lapply(samplingProtocols, FUN = function(sp) {
    speciesSampled <- unique(occurrence$scientificName[occurrence$samplingProtocol == sp])
    speciesSampled2 <- speciesSampled[speciesSampled %in% speciesLegend$surveyedSpecies]
    eventIDsUsed <- unique(eventLocationsSF$eventID[eventLocationsSF$samplingProtocol == sp])
    if (length(speciesSampled2) == 0) {
      return(NA)
    } else {
        return(expand.grid(scientificName = speciesSampled2, eventID = eventIDsUsed))
      }
  })
  
  # Remove empty events
  eventList2 <- eventList[unlist(lapply(eventList, FUN = function(x) {!is.null(nrow(x))}))]
  
  eventTable <- do.call(rbind, eventList2)
  eventTable <- merge(eventTable, eventDates, all.x = TRUE, by = "eventID")
  
  # Create an individual count 
  occurrence$individualCount <- 1
  
  # Add in occurrence data, an NA in coordinateUncertainy column means the species was NOT found in the survey
  eventTableWithOccurrences <- merge(eventTable, occurrence[,c("eventID", "scientificName", "individualCount")], all.x = TRUE,
                                     by.x = c("scientificName", "eventID"), by.y = c("scientificName", "eventID"))
  eventTableWithOccurrences$individualCount[is.na(eventTableWithOccurrences$individualCount)] <- 0
  eventTableWithOccurrences$geometry <- eventLocationsSF$geometry[match(eventTableWithOccurrences$eventID, eventLocationsSF$eventID)]
  eventTableWithOccurrences$acceptedScientificName <- speciesLegend$acceptedScientificName[match(eventTableWithOccurrences$scientificName, speciesLegend$surveyedSpecies)]
  
  # Add final columns
  eventTableWithOccurrences$dataType <- "PA"
  eventTableWithOccurrences$taxonKey <- speciesLegend$taxonKey[match(eventTableWithOccurrences$acceptedScientificName, speciesLegend$acceptedScientificName)]
  eventTableWithOccurrences$taxa <- focalTaxon$taxa[match(eventTableWithOccurrences$taxonKey, focalTaxon$key)]
  eventTableWithOccurrences$taxonKeyProject <- focalTaxon$key[match(eventTableWithOccurrences$taxonKey, focalTaxon$key)]
  
  # New dataset is ready!
  newDataset <- st_as_sf(eventTableWithOccurrences,          
                         crs = crs)
  newDataset <- newDataset %>%
    dplyr::select(acceptedScientificName, individualCount, geometry, dataType, taxa, year, taxonKeyProject) %>%
    filter(!is.na(acceptedScientificName))
  
  # Remove any duplicated observations from the same year at the same place
  arrangedData <- newDataset[order(newDataset$individualCount, decreasing = TRUE),]
  newDataset2 <- arrangedData[!duplicated(arrangedData[,c("geometry", "acceptedScientificName")]),]
  
  saveRDS(newDataset, paste0(tempFolderName,"/", datasetName ,"/processedDataset.RDS"))
  return(newDataset)
}
