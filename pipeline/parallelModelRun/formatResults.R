
# Process the richness results from each section

library(intSDM)
library(rgbif)
library(terra)
library(dplyr)

interestedGroup <- "vascularPlants"

# Import local functions
sapply(list.files("functions", full.names = TRUE), source)

# Ensure that modelRun and dateAccessed are specified
if (!exists("modelRun")) stop("You need to specify the variable modelRun")
if (!exists("dateAccessed")) stop("You need to specify the variable dateAccessed")

# Specify folders for storage of all run data
folderName <- paste0("data/run_", dateAccessed)
tempFolderName <- paste0(folderName, "/temp")

# model output folder
modelFolderName <- paste0(folderName, "/modelOutputs1")

filesWithinModelFolderName <- list.files(path=modelFolderName, pattern = interestedGroup, all.files=TRUE, 
           full.names=TRUE)

outputsRichness <- list()
outputProbs <- list()
for(results in seq_along(filesWithinModelFolderName)){

outputs <- readRDS(paste0(filesWithinModelFolderName[results], "/richnessPreds.rds"))
if(results == 1){
  predictionData <- outputs$Richness
}
outputsRichness[[results]] <- outputs$Richness
outputProbs[[results]] <- outputs$Probabilities
}

outputProbs <- purrr::flatten(outputProbs)%>%
  as.list()
predictionData$mean <- Reduce(`+`, lapply(outputProbs, function(x) x$mean))
predictionData$q0.025 <- Reduce(`+`, lapply(outputProbs, function(x) x$q0.025))

#plot ggplot

ggplot(predictionData)+
  geom_sf(aes(color = mean))
