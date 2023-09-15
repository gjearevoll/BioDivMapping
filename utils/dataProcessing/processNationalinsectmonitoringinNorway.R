
###----------------------------------------###
### PROCESSING FUNCTION INSECT MONITORING ####
###----------------------------------------###

# This script downloads the datasets of the National insect monitoring study directly from the endpoint supplied
# to GBIF. The study is stored differently to others and requires unique processing.

library(sf)

# Get the relevant endpoint
focalEndpoint <- metadata$DWCEndpoint[metadata$name == datasetName]

# Download and unzip file in temp folder
options(timeout=100)
download.file(focalEndpoint, paste0(tempFolderName,"/", datasetName ,".zip"), mode = "wb")
unzip(paste0(tempFolderName,"/", datasetName ,".zip"), exdir = paste0(tempFolderName,"/",  datasetName))

# Read in occurrence and event data
events <- read.delim(paste0(tempFolderName, "/National insect monitoring in Norway/event.txt"))
occurrence <- read.delim(paste0(tempFolderName,"/National insect monitoring in Norway/occurrence.txt"))

# There are four levels to this thing. Level 1 events are linked to level 2 by their parent ID and so forth.
# Level 1 - Identification of an insect in a trap
# Level 2 - Laying of the trap itself
# Level 3 - Multiple traps within a sampling period
# Level 4 - Multiple sampling periods within a sampling season

# First, get a list of all species samples
surveyedSpecies <- gsub(" ", "_",unique(occurrence$scientificName))
ourSurveyedSpecies <- focalSpecies$species[focalSpecies$species %in% surveyedSpecies]

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
occurrencesMostRecent <- occurrencesWithEvent %>%
  group_by(scientificName, locationID) %>%
  slice_max(exactDate, n = 1) %>% 
  ungroup()
occurrencesMostRecent <- occurrencesMostRecent[!duplicated(occurrencesMostRecent[,c("scientificName", "locationID")]),]

# The resulting data frame gives you results for the most recent individual insect abundances at each site
ourDataset <- occurrencesMostRecent[,c("scientificName", "organismQuantity", "decimalLatitude", "decimalLongitude", "locationID")]
ourDataset <- ourDataset %>% 
  mutate(simpleScientificName = gsub(" ", "_", scientificName)) %>%
  filter(simpleScientificName %in% ourSurveyedSpecies)

locations <- ourDataset[!duplicated(ourDataset[,c("locationID", "decimalLatitude", "decimalLongitude")]),
                        c("locationID", "decimalLatitude", "decimalLongitude")]

# We now need to fill in the absences - any species which were looked for but not found. THis si the same technique
# as used in the presenceAbsenceCOnversion.R script.
allSpecies <- expand.grid(simpleScientificName = ourSurveyedSpecies,
                          locationID = unique(ourDataset$locationID))
mergedDataset <- merge(allSpecies, ourDataset[,c("locationID", "simpleScientificName", "organismQuantity")], 
                       all.x = TRUE, by = c("locationID", "simpleScientificName"))
mergedDataset <- merge(mergedDataset, locations,
                       all.x = TRUE, by = "locationID")

# Convert all NAs to 0
mergedDataset$organismQuantity[is.na(mergedDataset$organismQuantity)] <- 0

# Convert this into an sf object
newDataset <- st_as_sf(mergedDataset, coords = c("decimalLongitude", "decimalLatitude"),
                       crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


newDataset <- newDataset[c("simpleScientificName", "organismQuantity")] %>%
  rename(individualCount = organismQuantity)

newDataset$dataType <- "Counts"

newDataset$taxa <- focalSpecies$taxonomicGroup[match(newDataset$simpleScientificName, focalSpecies$species)]

