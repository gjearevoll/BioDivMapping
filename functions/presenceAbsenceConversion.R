
#' @title \emph{presenceAbsenceConversion}: Turns a presence only dataset into a presence absence dataset for surveyed data.

#' @description Some datasets have been reduced by GBIF to presence-only, despite the fact they are in fact presence-absence datastes. This function downloads these datasets directly from the source and adds the absences back in.
#'
#' @param focalEndpoint An endpoint through which the original dataset can be downloaded.
#' @param tempFolderName A directory in which to save the data downloaded directly from the source.
#' @param datasetName The name of the dataset to be downloaded.
#' @param focalData The dataset downloaded from GBIF.
#' @param regionGeometry An sf object encompassing our region of study, as produced by defineRegion.
#' @param focalTaxon A dataframe giving the key and names of each taxonomic group we are downloading.
#' 
#' @return A new dataset with absences added back in.
#'
#' @import sf
#' 
#' 
presenceAbsenceConversion <- function(focalEndpoint, tempFolderName, datasetName, focalData, regionGeometry, focalTaxon) {
  
  # Get the relevant endpoint
  
  # Download and unzip file in temp folder
  options(timeout=100)
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
  occurrence$species <- gsub(" ", "_", foo(occurrence$scientificName))
  speciesLegend <- data.frame(surveyedSpecies = unique(occurrence$species), 
                              acceptedScientificName = sapply(unique(occurrence$species), FUN = findGBIFName),
                              taxonKey = sapply(unique(occurrence$species), FUN = function(x) {taxaCheck(x, focalTaxon$key)})) %>%
    filter(!is.na(taxonKey))
  
  # Create table with all data combinations that we can match to
  allSpecies <- merge(speciesLegend, data.frame(eventID = unique(occurrence$eventID)), all = TRUE) %>%
    filter(!is.na(acceptedScientificName))
  allSpecies$longitude <- events$decimalLongitude[match(allSpecies$eventID, events$eventID)]
  allSpecies$latitude <- events$decimalLatitude[match(allSpecies$eventID, events$eventID)]
  allSpecies$dataType <- "PA"
  allSpecies$taxa <- focalTaxon$taxa[match(allSpecies$taxonKey, focalTaxon$key)]
  
  # Create an individual count if the species/event combination are found in our original data. If they aren't
  # the species was absent.
  mergedSpecies <- merge(allSpecies, occurrence[,c("eventID", "species", "occurrenceID")], all.x = TRUE,
                         by.x = c("surveyedSpecies", "eventID"), by.y = c("species", "eventID"))
  mergedSpecies$individualCount <- ifelse(is.na(mergedSpecies$occurrenceID), 0, 1)
  mergedSpecies <- mergedSpecies[!is.na(mergedSpecies$longitude),]
  mergedSpecies$year <- events$year[match(mergedSpecies$eventID, events$id)]

  # New dataset is ready!
  newDataset <- st_as_sf(mergedSpecies,                         
                               coords = c("longitude", "latitude"),
                               crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  # Now just cut out all observations outside our region
  st_crs(newDataset) <- "+proj=longlat +ellps=WGS84"
  
  # Crop to relevant region
  newDataset <- st_intersection(newDataset, regionGeometry)
  newDataset <- st_transform(newDataset, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
  newDataset <- newDataset[,c("acceptedScientificName", "individualCount", "geometry", "dataType", "taxa", "year")]
  return(newDataset)
}
