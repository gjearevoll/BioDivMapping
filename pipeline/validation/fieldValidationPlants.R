
#### MODEL VALIDATION ####

# The following script processes our different forms of data based on rules for different datasets, so that
# they are ready for use in our integrated SDMs.

# Import relevant libraries
library(sf)
library(terra)
library(dplyr)

args <- commandArgs(TRUE)

if (length(args) != 0) {
  # Set arguments
  runDate <- args[1]
}

#runDate <- "2026-03-05"

# Get total species richness for an area using the NTNU Vascular Plant Field Notes

if (!file.exists("pipeline/validation/data/GBIFDownload/event.txt")) {
  destFile <- "pipeline/validation/data/GBIFDownload.zip"
  download.file("https://gbif.vm.ntnu.no/archive.do?r=vascularplantfieldnotes", destFile, mode = "wb", timeout = 400)
  unzip(destFile, exdir = "pipeline/validation/data/GBIFDownload")
}

events <- read.delim("pipeline/validation/data/GBIFDownload/event.txt") %>% filter(year > 1999)
occurrences <- read.delim("pipeline/validation/data/GBIFDownload/occurrence.txt")

# Get all species surveyed
speciesSurveyed <- unique(occurrences$scientificName)

cat("Downloaded occurrence and event data\n")

# Get richness per event
eventsJoined <- merge(events[,c("eventID", "locationID")], occurrences, all.x = TRUE, by = "eventID") %>%
  filter(individualCount == 1)
eventsJoined <- eventsJoined[!duplicated(eventsJoined[,c("eventID","scientificName")]),]
eventsRichness <- tally(group_by(eventsJoined, eventID))

cat("Calculated richness per event\n")

### Import shapefile for vascular plants ###
# We're still not sure whether we'll make this publicly available #
plantShapeFileSF <- read_sf("data/external/validation/VascularLocalities.shp") |>
  st_transform("EPSG:25833")

# Check that all events in GBIF have a corresponding location
eventsInGBIF <- unique(eventsJoined$locationID)
eventsInSHP <- st_drop_geometry(plantShapeFileSF$locationID)

# summary(eventsInGBIF %in% eventsInSHP)

# There's 1 missing, so we need to remove this
events <- events[events$locationID %in% eventsInSHP,]
events$richness <- eventsRichness$n[match(events$eventID, eventsRichness$eventID)]
eventsInGBIF <- unique(events$locationID)

cat("Got all relevant events as shapefiles\n")

# Now remove any locations that are not in our event data
summary(eventsInSHP %in% eventsInGBIF)
plantShapeFile <- vect(plantShapeFileSF[plantShapeFileSF$locationID %in% eventsInGBIF,])

# Find the area of each polygon
plantShapeFile$area <- expanse(plantShapeFile)/1000000
plantShapeFile$richness <- events$richness[match(plantShapeFile$locationID, events$locationID)]

# Take sample for practice
plantShapeFileSample <- plantShapeFile

# Import distance to road metric
source("functions/checkAndImportRast.R")
source("functions/isSubset.R")
regionGeometryBuffer <- st_union(plantShapeFileSF) |>
  st_transform(25833) |> 
  st_bbox() |> 
  st_as_sfc() |>
  st_segmentize(dfMaxLength = 10000) |> 
  vect() 
distance_roads <- checkAndImportRast("distance_roads", regionGeometryBuffer, "data/temp/geonorge/", 
                                     FALSE, yearInterval)
plantShapeFileSample$distance_roads <- extract(distance_roads, plantShapeFileSample, fun = "mean")[,2]

# Get species richness for each area
if (runDate == "2015") {
  probabilities <- rast("data/external/validation/PredFor-Alle-Karplanter_Norge_25833.tif")
  predictedPresences <- extract(probabilities, plantShapeFileSample, fun = "mean", na.rm = TRUE)
  plantShapeFileSample$predictedRichness <- predictedPresences[,-1]
  cat("Writing vector\n")
  writeVector(plantShapeFileSample, 
              paste0("pipeline/validation/data/predictedRichness", runDate,".gpkg"), 
              overwrite = TRUE)
} else {
  probabilities <- rast(paste0("run_", runDate,"/modelOutputs/processedOutputs/speciesprobability_vascularPlants.tiff"))
  
  # Narrow down to only species surveyed
  probabilities <- probabilities[[names(probabilities) %in% gsub(" ", "_", speciesSurveyed)]]
  
  # Get probabilities megred and as fixed values
  # probabilities <- probabilities[[1:5]]
  extractedProbs <- predictedPresences <- extract(probabilities, plantShapeFileSample, fun = "max", na.rm = TRUE)
  #predictedPresences[,-1] <- ifelse(predictedPresences[,-1] > 90, 1, 0)
  cat("Calculated presence/absence in location\n")
  plantShapeFileSample$predictedRichness <- rowSums(predictedPresences[,-1])/100
  cat("Writing vector\n")
  writeVector(plantShapeFileSample, 
              paste0("pipeline/validation/data/predictedRichness", runDate,".gpkg"), 
              overwrite = TRUE)
}
cat("Download species probabilities\n")

### It makes no sense to automate the running the script from here onwards, hence the STOP message below
stop("Automated section of script completed")

# Richness analysis
library(ggplot2)
library(terra)

runDate <- "2026-03-05"

plantRichness <- vect(paste0("pipeline/validation/data/predictedRichness", runDate,".gpkg"))
plantRichness <- plantRichness[plantRichness$richness >= 0,]

# Remove all regions too small for analysis and plot
plantRichness <- plantRichness[plantRichness$area > 1 & plantRichness$area < 10,]
plantRichnessDF <- as.data.frame(plantRichness)
plantRichnessDF$scaledRichness <- scale(plantRichnessDF$predictedRichness)
plantRichnessDF$sizeCat <- ifelse(plantRichnessDF$area < 2, "0-2", "2-10")

# 500 by 350
ggplot(plantRichnessDF[plantRichnessDF$area > 1 & plantRichnessDF$area < 21,], aes(x=richness, y=scaledRichness, color = sizeCat)) +
  geom_point(alpha = 0.5)+ scale_colour_brewer(palette = "Dark2") +
  labs(x = "Observed richness", y = "Estimated (scaled) richness", title =  "a) Current results",
       colour = "Area size\n(square\nkilometres)") + 
  geom_smooth(method=lm , color="black", fill="gray", se=TRUE, lwd = 0.4) +
  theme_classic()


# Do the same for the 2015 results
oldPlantRichness <- vect(paste0("pipeline/validation/data/predictedRichness2015.gpkg"))
oldPlantRichness <- oldPlantRichness[oldPlantRichness$richness >= 0 & !is.na(oldPlantRichness$predictedRichness),]

# Remove all regions too small for analysis and plot
oldPlantRichness <- oldPlantRichness[oldPlantRichness$area > 1 & oldPlantRichness$area < 10,]
oldPlantRichnessDF <- as.data.frame(oldPlantRichness)
oldPlantRichnessDF$scaledRichness <- scale(oldPlantRichnessDF$predictedRichness)
oldPlantRichnessDF$sizeCat <- ifelse(oldPlantRichnessDF$area < 2, "0-2", ifelse(oldPlantRichnessDF$area > 10, "10+", "2-10"))

ggplot(oldPlantRichnessDF[oldPlantRichnessDF$area > 1 & oldPlantRichnessDF$area < 21,], aes(x=richness, y=scaledRichness, color = sizeCat)) +
  geom_point(alpha = 0.5)+ scale_colour_brewer(palette = "Dark2") +
  labs(x = "Observed richness", y = "", title =  "b) Non-ISDM results",
       colour = "Area size\n(square\nkilometres)") + 
  geom_smooth(method=lm , color="black", 
              fill="gray", se=TRUE, lwd = 0.4) +
  theme_classic() +
  ylim(-1,2.5)

summary(lm(data = plantRichnessDF, richness ~ scaledRichness + sizeCat))
summary(lm(data = oldPlantRichnessDF, richness ~ scaledRichness + sizeCat))
