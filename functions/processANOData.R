
#' @title \emph{processANOData}: Standardises an ANO dataset for use in species models.

#' @description This function takes our ANO dataset and standardises it for use alongside the other datasets downloaded from GBIF.
#'
#' @param ANODataset A dataset as downloaded using the importANO function
#' 
#' @return A new processed dataset, standardised for further use.
#'
#' @import sf
#' 


processANOData <- function(focalEndpoint, tempFolderName, datasetName, regionGeometry, focalTaxon, crs, yearToStart) {
  

  # Download and unzip file in temp folder
  #ptions(timeout=100)
  download.file(focalEndpoint, paste0(tempFolderName,"/", datasetName ,".zip"), mode = "wb")
  unzip(paste0(tempFolderName,"/", datasetName ,".zip"), exdir = paste0(tempFolderName,"/",  datasetName))
  
  # Load in event and occurrence data
  events <- read.delim(paste0(tempFolderName,"/", datasetName ,"/event.txt")) %>%
    filter(coordinateUncertaintyInMeters < coordUncertainty)
  occurrence <- read.delim(paste0(tempFolderName,"/", datasetName ,"/occurrence.txt"))
  
  # Get all main events from data and get event table for just these events
  mainEvents <- unique(substr(events$parentEventID,1,13))
  eventTable <- events[events$eventID %in% mainEvents, c("eventID", "decimalLatitude", "decimalLongitude")]
  eventTable$year <- substr(eventTable$eventID, 5,8)
  
  # Now remove all events fro before start eyar
  eventTable <- eventTable[eventTable$year >= yearToStart,]
  eventLocationsSF <- st_as_sf(eventLocations,                         
                               coords = c("decimalLongitude", "decimalLatitude"),
                               crs = "+proj=longlat +ellps=WGS84")
  eventLocationsSF <- st_transform(eventLocationsSF, crs = crs)
  eventLocationsSF <- st_intersection(eventLocationsSF, regionGeometry)
  
  # Get occurrences down only to ANO squares and species found
  occurrenceShort <- occurrence[substr(occurrence$eventID,1,13) %in% unique(eventLocationsSF$eventID), c("scientificName", "eventID")]
  occurrenceShort$eventID <- substr(occurrenceShort$eventID,1,13)
  occurrenceShort$individualCount <- 1
  occurrenceShort <- occurrenceShort[!duplicated(occurrenceShort),]
  
  # Build a species table
  surveyedSpecies <- unique(occurrenceShort$scientificName)
  speciesLegend <- data.frame(surveyedSpecies = surveyedSpecies, 
                              acceptedScientificName = sapply(surveyedSpecies, FUN = findGBIFName),
                              taxonKey = sapply(surveyedSpecies, FUN = function(x) {taxaCheck(x, focalTaxon$key)})) %>%
    filter(!is.na(taxonKey)) %>%
    filter(!is.na(acceptedScientificName))
  if (nrow(speciesLegend) == 0) {return(NULL)}
  
  # Create table with all data combinations that we can match to
  eventTable <- expand.grid(scientificName = speciesLegend$surveyedSpecies, eventID = unique(eventLocationsSF$eventID))
  eventTable <- merge(eventTable, occurrenceShort, all.x = TRUE, by = c("eventID", "scientificName"))
  
  # Add details in to final table
  eventTable$geometry <- eventLocationsSF$geometry[match(eventTable$eventID, eventLocationsSF$eventID)]
  eventTable$year <- substr(eventTable$eventID, 5,8)
  eventTable$acceptedScientificName <- speciesLegend$acceptedScientificName[match(eventTable$scientificName, 
                                                                                  speciesLegend$surveyedSpecies)]
  eventTable$dataType <- "PA"
  eventTable$taxonKey <- speciesLegend$taxonKey[match(eventTable$acceptedScientificName, speciesLegend$acceptedScientificName)]
  eventTable$taxa <- focalTaxon$taxa[match(eventTable$taxonKey, focalTaxon$key)]
  eventTable$taxonKeyProject <- focalTaxon$key[match(eventTable$taxonKey, focalTaxon$key)]
  eventTable$individualCount[is.na(eventTable$individualCount)] <- 0
  
  
  # New dataset is ready!
  newDataset <- st_as_sf(eventTable,          
                         crs = crs)
  newDataset <- newDataset %>%
    dplyr::select(acceptedScientificName, individualCount, geometry, dataType, taxa, year, taxonKeyProject, eventID) %>%
    filter(!is.na(acceptedScientificName))
  
  saveRDS(newDataset, paste0(tempFolderName,"/", datasetName ,"/processedDataset.RDS"))
  return(newDataset)
}
