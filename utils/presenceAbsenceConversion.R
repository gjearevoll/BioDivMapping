
###--------------------------------------------###
### PROCESSING FUNCTION PRESENCE/ABSENCE DATA ####
###--------------------------------------------###

focalEndpoint <- metadata$DWCEndpoint[metadata$name == datasetName]

download.file(focalEndpoint, paste0(tempFolderName,"/", datasetName ,".zip"))
unzip(paste0(tempFolderName,"/", datasetName ,".zip"), exdir = paste0(tempFolderName,"/",  datasetName))

foo = function(x){
  paste(toupper(substring(x, 1, 1)),
        tolower(substring(x, 2, nchar(x))),
        sep = "")
}

events <- read.delim(paste0(tempFolderName,"/", datasetName ,"/event.txt"))
occurrence <- read.delim(paste0(tempFolderName,"/", datasetName ,"/occurrence.txt"))
occurrence$species <- gsub(" ", "_", foo(occurrence$scientificName))

allSpecies <- expand.grid(simpleScientificName = focalSpecies$species,
                         eventID = unique(occurrence$eventID))
allSpecies$longitude <- events$decimalLongitude[match(allSpecies$eventID, events$eventID)]
allSpecies$latitude <- events$decimalLatitude[match(allSpecies$eventID, events$eventID)]
allSpecies$dataType <- dataType

allSpecies$individualCount <- ifelse(allSpecies$simpleScientificName %in% occurrence$species &
                                allSpecies$eventID %in% occurrence$eventID, 1, 0)

newDataset <- st_as_sf(allSpecies,                         
                     coords = c("longitude", "latitude"),
                     crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
