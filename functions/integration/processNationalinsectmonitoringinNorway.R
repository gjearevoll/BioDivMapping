
###----------------------------------------###
### PROCESSING FUNCTION INSECT MONITORING ####
###----------------------------------------###

# This script downloads the datasets of the National insect monitoring study directly from the endpoint supplied
# to GBIF. The study is stored differently to others and requires unique processing.

processNationalInsectMonitoring <- function(focalData, endpoint, tempFolderName) {
  
  library(sf)
  library(stringr)
  
  # Download and unzip file in temp folder
  options(timeout=100)
  zippedDownload <- paste0(tempFolderName,"/NationaInsectMonitoring.zip")
  download.file(endpoint, zippedDownload, mode = "wb")
  unzip(paste0(tempFolderName,"/NationaInsectMonitoring.zip"), exdir = paste0(tempFolderName,"/NationaInsectMonitoring"))
  
  # Read in occurrence and event data
  events <- read.delim(paste0(tempFolderName, "/NationaInsectMonitoring/event.txt"))
  occurrence <- read.delim(paste0(tempFolderName,"/NationaInsectMonitoring/occurrence.txt"))
  
  # There are four levels to this thing. Level 1 events are linked to level 2 by their parent ID and so forth.
  # Level 1 - Identification of an insect in a trap
  # Level 2 - Laying of the trap itself
  # Level 3 - Multiple traps within a sampling period
  # Level 4 - Multiple sampling periods within a sampling season
  
  # First, get a list of all species samples and create a legend for them
  surveyedSpecies <- unique(occurrence$scientificName)
  legendTable <- data.frame(scientificName = surveyedSpecies, genus = word(surveyedSpecies,1))
  
  # Narrow it down to the same genus we're looking for
  legendTable <- legendTable[legendTable$genus %in% word(unique(focalData$acceptedScientificName), 1),]
  legendTable$acceptedScientificName <- sapply(legendTable$scientificName, FUN = findGBIFName)
  
  # And now narrow it down to species
  legendTable <- legendTable[legendTable$acceptedScientificName %in% focalData$acceptedScientificName,]
  
  # Here I've taken level 1 events and linked them to level 2. Each location has a separate date for the trap sampling.
  occurrencesWithEvent <- occurrence[occurrence$eventID %in% events$eventID,]
  
  # Now get all key data columns from the events data
  occurrencesWithEvent$parentEventID <- events$parentEventID[match(occurrencesWithEvent$eventID, events$eventID)]
  occurrencesWithEvent$locationID <- events$locationID[match(occurrencesWithEvent$parentEventID, events$eventID)]
  occurrencesWithEvent$eventDate <- events$eventDate[match(occurrencesWithEvent$eventID, events$eventID)]
  occurrencesWithEvent$decimalLongitude <- events$decimalLongitude[match(occurrencesWithEvent$parentEventID, events$eventID)]
  occurrencesWithEvent$decimalLatitude <- events$decimalLatitude[match(occurrencesWithEvent$parentEventID, events$eventID)]
  
  # Take only most recent event from a location
  occurrencesWithEvent$exactDate <- as.Date(substr(occurrencesWithEvent$eventDate,1,10))
  mostRecentSample <- occurrencesWithEvent %>%
    dplyr::select(parentEventID, locationID, exactDate) %>%
    group_by(locationID) %>%
    slice_max(exactDate, n = 1,na_rm = TRUE) %>% 
    distinct() %>%
    ungroup()
  mostRecentSample$year <- substr(mostRecentSample$exactDate, 1, 4)
  
  # Now add a line for every species
  ourDataset <- merge(mostRecentSample, legendTable["scientificName"], all = TRUE)
  ourDataset$acceptedScientificName <- legendTable$acceptedScientificName[match(ourDataset$scientificName, legendTable$scientificName)]
  ourDataset <- ourDataset[!is.na(ourDataset$acceptedScientificName),]
  
  # Now get abundances for those specie that have them
  ourDatasetAbundance <- merge(ourDataset, occurrencesWithEvent[,c("scientificName", "organismQuantity", "parentEventID",
                                                                   "locationID")], all.x = TRUE, 
                               by = c("parentEventID", "locationID", "scientificName"))
  
  # Now create a locations table to get location data
  locations <- distinct(occurrencesWithEvent[,c("locationID", "decimalLatitude", "decimalLongitude")])
  ourDatasetLocated <- merge(ourDatasetAbundance, locations, all.x = TRUE, by = "locationID")
  
  # Convert all NAs to 0
  ourDatasetLocated$organismQuantity[is.na(ourDatasetLocated$organismQuantity)] <- 0
  
  # Convert this into an sf object
  newDataset <- st_as_sf(ourDatasetLocated, coords = c("decimalLongitude", "decimalLatitude"),
                         crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  # Cut down set
  newDataset <- newDataset[c("acceptedScientificName", "organismQuantity", "year")] %>%
    rename(individualCount = organismQuantity)
  
  # Crop to relevant region
  st_crs(newDataset) <- "+proj=longlat +ellps=WGS84"
  newDataset <- st_intersection(newDataset, regionGeometry)
  newDataset <- st_transform(newDataset, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
  newDataset$dataType <- "Counts"
  
  taxaLegend <- distinct(st_drop_geometry(focalData[,c("taxa", "acceptedScientificName")]))
  
  newDataset$taxa <- taxaLegend$taxa[match(newDataset$acceptedScientificName, taxaLegend$acceptedScientificName)]
  return(newDataset)
  
  
}
