
#' @title \emph{processFieldNotesEvent}: Turns a presence only dataset into a presence absence dataset for surveyed data.

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
processFieldNotesEvent <- function(focalEndpoint, tempFolderName, datasetName, regionGeometry, focalTaxon, crs) {
  
  # Get the relevant endpoint
  
  # Download and unzip file in temp folder
  #ptions(timeout=100)
  download.file(focalEndpoint, paste0(tempFolderName,"/", datasetName ,".zip"), mode = "wb")
  unzip(paste0(tempFolderName,"/", datasetName ,".zip"), exdir = paste0(tempFolderName,"/",  datasetName))
  
  # Function to change file names
  foo = function(x){
    paste(toupper(substring(x, 1, 1)),
          tolower(substring(x, 2, nchar(x))),
          sep = "")
  }
  
  # Load in event and occurrence data
  events <- read.delim(paste0(tempFolderName,"/", datasetName ,"/event.txt"))
  occurrence <- read.delim(paste0(tempFolderName,"/", datasetName ,"/occurrence.txt"))
  
  # Get all species surveyed
  surveyedSpecies <-  unique(occurrence$scientificName)
  surveyedSpecies <- surveyedSpecies[!is.na(surveyedSpecies)]
  
  # Find only eventIDs within our regionGeometry
  eventLocations <- events %>%
    filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) %>%
    dplyr::select(decimalLatitude, decimalLongitude, eventID, coordinateUncertaintyInMeters) %>%
    distinct()
  eventLocationsSF <- st_as_sf(eventLocations,                         
                               coords = c("decimalLongitude", "decimalLatitude"),
                               crs = "+proj=longlat +ellps=WGS84")
  eventLocationsSF <- st_transform(eventLocationsSF, crs = crs)
  eventLocationsSF <- st_intersection(eventLocationsSF, regionGeometry) %>%
    filter(coordinateUncertaintyInMeters <= 250)
  
  # At this point we may find that there are no relevant points from this dataset available - 
  # in this case we want to finish the function early
  if (nrow(eventLocationsSF) == 0) {
    return(NULL)
  }
  
  # Make sure we have a 'year' column
  if (!("year" %in% colnames(events)) | all(is.na(unique(events$year)))) {
    if ("eventDate" %in% colnames(occurrence)) {
    eventTable <- distinct(occurrence[,c("eventID", "eventDate")])
    eventTable$year <- format(as.Date(eventTable$eventDate), "%Y") 
    events$year <- eventTable$year[match(events$eventID, eventTable$eventID)]
    } else if ("eventDate" %in% colnames(events)) {
      eventTable <- distinct(events[,c("eventID", "eventDate")])
      events$year <- format(as.Date(eventTable$eventDate), "%Y") 
    } else {
    events$year <- NA
  } }
   
  # Get a dates table to match years to events
  eventDates <- events %>%
    filter(eventID %in% eventLocationsSF$eventID) %>%
    dplyr::select(year, eventID) %>%
    distinct()
  
  # # Find species found each year FOR LATER
  # speciesDates <- occurrence %>%
  #   dplyr::select(eventID, scientificName) %>%
  #   distinct()
  # speciesDates$year <- eventDates$year[match(speciesDates$eventID, eventDates$eventID)]
  # speciesDates <- speciesDates[,c("scientificName", "year")] %>%
  #   distinct()
  
  # Build a species table
  speciesLegend <- data.frame(surveyedSpecies = surveyedSpecies, 
                              acceptedScientificName = sapply(surveyedSpecies, FUN = findGBIFName),
                              taxonKey = sapply(surveyedSpecies, FUN = function(x) {taxaCheck(x, focalTaxon$key)})) %>%
    filter(!is.na(taxonKey))
  if (nrow(speciesLegend) == 0) {return(NULL)}
  
  # Create table with all data combinations that we can match to
  eventTable <- expand.grid(species = speciesLegend$surveyedSpecies, eventID = unique(eventLocationsSF$eventID))
  eventTable <- merge(eventTable, eventDates, all.x = TRUE, by = "eventID")
  
  # # Create table with all data combinations that we can match to FOR LATER
  ## Note that later (line 109) you'll need to add to merge by year too
  # eventTable <- do.call(rbind, lapply(unique(speciesDates$year), FUN = function(y) {
  #   yearSpecies <- speciesDates$scientificName[speciesDates$year == y]
  #   yearEvents <- eventDates$eventID[eventDates$year == y]
  #   expand.grid(species = yearSpecies, eventID = yearEvents, year = y)
  # }))
  
  # Create an individual count 
  occurrence$individualCount <- 1
  
  # Add in occurrence data, an NA in individualCount column means the species was NOT found in the survey
  eventTableWithOccurrences <- merge(eventTable, occurrence[,c("eventID", "scientificName", "individualCount")], all.x = TRUE,
                                     by.x = c("species", "eventID"), by.y = c("scientificName", "eventID"))
  eventTableWithOccurrences$individualCount[is.na(eventTableWithOccurrences$individualCount)] <- 0
  eventTableWithOccurrences$geometry <- eventLocationsSF$geometry[match(eventTableWithOccurrences$eventID, eventLocationsSF$eventID)]
  eventTableWithOccurrences$acceptedScientificName <- speciesLegend$acceptedScientificName[match(eventTableWithOccurrences$species, speciesLegend$surveyedSpecies)]
  
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
  saveRDS(newDataset, paste0(tempFolderName,"/", datasetName ,"/processedDataset.RDS"))
  return(newDataset)
}
