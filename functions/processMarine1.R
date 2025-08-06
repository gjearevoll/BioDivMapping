
#' @title \emph{processANOData}: Standardises an ANO dataset for use in species models.

#' @description This function takes our ANO dataset and standardises it for use alongside the other datasets downloaded from GBIF.
#'
#' @param ANODataset A dataset as downloaded using the importANO function
#' 
#' @return A new processed dataset, standardised for further use.
#'
#' @import sf
#' 
# 
# library(sf)
# library(dplyr)
# library(stringr)
# # Need some variables to make this work
# crs <- 32633
# focalEndpoint <- "https://gbif.imr.no/ipt/archive.do?r=imr_mareano_beamtrawl"
# datasetName <- "imr_mareano_beamtrawl"
# regionGeometry <- read_sf("~/Marine-biodiversity-Norwegian-deep-sea/data/aoi_ocean/aoi_ocean.shp") %>%
#   st_transform(32633)
# tempFolderName <- "~/Marine-biodiversity-Norwegian-deep-sea/data/temp"
# source("functions/findGBIFName.R")
# source("functions/taxaCheck.R")

processMarine1 <- function(focalEndpoint, tempFolderName, datasetName, regionGeometry, focalTaxon, crs, coordUncertainty, yearToStart) {
  
  # Download the data from the relevant endpoint and unzip it in a temporary folder, then bring it into your environment
  download.file(focalEndpoint, paste0(tempFolderName,"/", datasetName ,".zip"), mode = "wb")
  unzip(paste0(tempFolderName,"/", datasetName ,".zip"), exdir = paste0(tempFolderName,"/",  datasetName))
  occurrence <- read.delim(paste0(tempFolderName,"/", datasetName ,"/occurrence.txt")) %>%
    dplyr::mutate(individualCount = 1)
  event <- read.delim(paste0(tempFolderName,"/", datasetName ,"/event.txt"))
  
  # Get only events that have locations, years and relevant coordinate uncertainty
  event$decimalLatitude <- as.numeric(event$decimalLatitude)
  event$decimalLongitude <- as.numeric(event$decimalLongitude)
  event <- event[!is.na(event$decimalLatitude) & !is.na(event$decimalLongitude),]
  
  if (!is.na(coordUncertainty)) {
    event <- event %>% filter(coordinateUncertaintyInMeters  <= coordUncertainty)
  }
 
  eventSF <- st_as_sf(event,                         
                           coords = c("decimalLongitude", "decimalLatitude"),
                           crs = "+proj=longlat +ellps=WGS84")
  sf_use_s2(FALSE)
  eventSF <- st_intersection(st_transform(eventSF, crs), regionGeometry)
  sf_use_s2(TRUE)
  
  if (nrow(eventSF) == 0) {return(NULL)}
  
  # Get a year  (if it doesn't exist)
  if (!("year" %in% colnames(eventSF))) {
    dateColumn <- gsub("\\.", "/", eventSF$eventDate)
    eventSF$year <- substr(as.Date(dateColumn, tryFormats = c("%d.%m.%Y", "%d/%m/%Y", "%Y/%m/%d", "%Y-%m-%d")),1,4)
  }
  
  # Get a list of all the years
  uniqueYears <- unique(eventSF$year)
  uniqueYears <- uniqueYears[uniqueYears > yearToStart & !is.na(uniqueYears)]
  
  # Join occurrences to events
  if ("year" %in% colnames(occurrence)) {occurrence <- dplyr::select(occurrence, -year)}
  
  occurrenceSF <- merge(occurrence, eventSF[,c("eventID", "year", "geometry")], all.x = TRUE, by = "eventID")
  cat("\nMerging events to occurrences")
  occurrencesSF <- occurrenceSF %>%
    filter(!is.na(eventID) & year %in% uniqueYears)
  
  allSpecies <- unique(occurrencesSF$scientificName)
  
  # Get list of all species and ID now
  cat("\nGetting species names")
  # speciesLegend <- data.frame(surveyedSpecies = unique(occurrence$scientificName), 
  #                             acceptedScientificName = sapply(unique(occurrence$scientificName), FUN = findWormsName))
  # 
  speciesLegend <- data.frame(surveyedSpecies = allSpecies, 
                              acceptedScientificName = sapply(allSpecies, FUN = findGBIFName),
                              taxonKey = sapply(allSpecies, FUN = function(x) {taxaCheck(x, focalTaxon$key)})) %>%
    filter(!is.na(acceptedScientificName))
  
  # Now execute the exact same function for every year
  fullYearList <- lapply(uniqueYears, FUN = function(x) {
    
    
    cat("\nProcessing year",x)
    # Get the data for just that year
    occurrenceOneYear <- as.data.frame(occurrencesSF) %>%
      filter(year == x)
    
    # Create a list of all species sampled and all sampling events
    allSpecies <- unique(occurrenceOneYear$scientificName)
    allSamplingEvents <- unique(occurrenceOneYear$eventID)
    allSamplingEvents <- allSamplingEvents[allSamplingEvents != "" & !is.na(allSamplingEvents)]
    
    # Get the correct name for each species from WORMS
    speciesLegendSubset <- speciesLegend[speciesLegend$surveyedSpecies %in% allSpecies,]
    
    # Get rid of all species that were either not species or didn't have a proper name
    allSpecies2 <- speciesLegendSubset$surveyedSpecies[!is.na(speciesLegendSubset$surveyedSpecies)]
    
    # Now create a table of all possible species/samplingEvent combinations
    eventTable <- expand.grid(scientificName = allSpecies2, eventID = allSamplingEvents)
    
    # Merge the initial list to the expanded list to see which species were found - those with NAs in the individual count column weren't foudn
    eventTable2 <- merge(eventTable, occurrenceOneYear[,c("eventID", "scientificName", "individualCount", "id")], 
                         all.x = TRUE, by = c("eventID", "scientificName"))
    eventTable2$individualCount[is.na(eventTable2$individualCount)] <- 0
    
    eventTable3 <- eventTable2
    
    # Add final columns
    eventTable3$dataType <- "PA"
    eventTable3$year <- x
    eventTable3$taxonKey <- speciesLegend$taxonKey[match(eventTable3$scientificName, speciesLegend$surveyedSpecies)]
    eventTable3$taxa <- focalTaxon$taxa[match(eventTable3$taxonKey, focalTaxon$key)]
    
    # Add locations to the data
    locationTable <- occurrenceOneYear[!duplicated(occurrenceOneYear[,c("eventID", "geometry")]),
                                       c("eventID", "geometry")]
    eventTableWithLocations <- merge(eventTable3, locationTable, all.x = TRUE,
                                     by.x = "eventID", by.y = "eventID")
    
    # Turn into an SF dataset
    eventLocationsSF <- st_as_sf(eventTableWithLocations, crs = crs)
    return(eventLocationsSF)
    
  }) 
  
  # Add all datasets together
  allData <- do.call(rbind, fullYearList)
  
  # Remove any duplicated observations from the same year at the same place
  arrangedData <- allData[order(allData$individualCount, decreasing = TRUE),]
  newDataset <- arrangedData[!duplicated(arrangedData[,c("geometry", "year", "scientificName")]),]
  saveRDS(newDataset, paste0(tempFolderName,"/", datasetName ,"/processedDataset.RDS"))
  
  return(newDataset)
}

# processMareanoTrawl(focalEndpoint, tempFolderName, datasetName, regionGeometry, focalTaxon, crs)
