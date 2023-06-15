

#### SPECIES DATA IMPORT ####

# The following script imports our various forms of species data and processes them, based on the type of data
# (presence-absence/occurrence-only/abundance) and other specifications related to the source.

library(intSDM)
library(rgbif)
library(sf)

###-----------------###
### 1. Preparation ####
###-----------------###

# Run script to define geographical region and resolution we are working with 
level <- "municipality"  # level can be country, county, municipality, or points (examples of points given below)
region <- "5001"
#points <- c(4.641979, 57.97976, 31.05787, 71.18488)
#names(points) <- c("north", "south", "east", "west")
source("utils/defineRegion.R")

# Define initial species list.
# Here we would normally import a list of species names with associated taxonomic groups. 
focalSpecies <- read.csv("data/external/focalSpecies.csv", header = T)

###--------------------###
### 2. Dataset Import ####
###--------------------###

# Import dataset 1
dataset1 <- PA_redlist[PA_redlist$species %in% focalSpecies$species,]
dataset1$taxonomicGroup <- focalSpecies$taxonomicGroup[match(dataset1$species, focalSpecies$species)]
dataset1 <- st_filter(dataset1, regionGeometry)
attr(dataset1, "dataType") <- "PA"

# Import dataset 2
dataset2 <- occ_data(scientificName = focalSpecies$species, hasCoordinate = TRUE, limit = 500, 
                     geometry = st_bbox(regionGeometry))

# Convert dataset to sp values
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
df <- lapply(dataset2, FUN = function(x) {
  returningItem <- as.data.frame(x$data[,c("species", "decimalLongitude", "decimalLatitude")])
  st_as_sf(x = returningItem,                         
           coords = c("decimalLongitude", "decimalLatitude"),
           crs = projcrs)
})
dataset2 <- do.call(rbind, df)
dataset2$taxonomicGroup <- focalSpecies$taxonomicGroup[match(dataset2$species, gsub("_", " ", focalSpecies$species))]
attr(dataset2, "dataType") <- "PO"

###--------------------###
### 3. Dataset Upload ####
###--------------------###

# For now we're just doing this to the data/temp folder, later this will go to Wallace. A version also needs to be saved in
# the visualisation folder though, as this will go into the occurrence mapping.
speciesDataList <- list(dataset1, dataset2)
attr(speciesDataList, "level") <- level
attr(speciesDataList, "region") <- region
dataList <- list(species = speciesDataList, geometry = regionGeometry)
saveRDS(dataList, "data/temp/speciesDataImported.RDS")
saveRDS(dataList, "visualisation/hotspotMap/speciesDataList.RDS")

