
###--------------------------------------------###
### PROCESSING FUNCTION PRESENCE/ABSENCE DATA ####
###--------------------------------------------###

# This script downloads the datasets of presence/absence data directly from the endpoint supplied
# to GBIF. THis way we can figure out which species were NOT found.

library(sf)

# Get the relevant endpoint
focalEndpoint <- metadata$DWCEndpoint[metadata$name == datasetName]

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
surveyedSpecies <- unique(occurrence$species)
ourSurveyedSpecies <- focalSpecies$species[focalSpecies$species %in% surveyedSpecies]

# Create table with all data combinations that we can match to
allSpecies <- expand.grid(simpleScientificName = ourSurveyedSpecies,
                          eventID = unique(occurrence$eventID))
allSpecies$longitude <- events$decimalLongitude[match(allSpecies$eventID, events$eventID)]
allSpecies$latitude <- events$decimalLatitude[match(allSpecies$eventID, events$eventID)]
allSpecies$dataType <- dataType

# Create an individual count if the species/event combination are found in our original data. If they aren't
# the species was absent.
allSpecies$individualCount <- ifelse(allSpecies$simpleScientificName %in% occurrence$species &
                                       allSpecies$eventID %in% occurrence$eventID, 1, 0)
allSpecies <- allSpecies[complete.cases(allSpecies),]

# New dataset is ready!
newDatasetNorway <- st_as_sf(allSpecies,                         
                             coords = c("longitude", "latitude"),
                             crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Now just cut out all observations outside our region
newDataset <- newDatasetNorway
st_crs(newDataset) <- "+proj=longlat +ellps=WGS84"

# Crop to relevant region
newDataset <- st_intersection(newDataset, regionGeometry)
newDataset <- st_transform(newDataset, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
newDataset$taxa <- focalSpecies$taxonomicGroup[match(newDataset$simpleScientificName, focalSpecies$species)]

