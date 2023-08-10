

#### MODEL RUNS ####

# The following script imports our various forms of environmental data and processes them, based on the type of data
# and other specifications related to the source.

###-----------------###
### 1. Preparation ####
###-----------------###

print("Preparing data for model run.")

library(intSDM)
library(rgbif)

externalImport <- FALSE

# Initialise folders for storage of all run data
if (!exists("dateAccessed")) {
  dateAccessed <- as.character(Sys.Date())
}
folderName <- paste0("data/run_", dateAccessed)
tempFolderName <- paste0(folderName, "/temp")
modelFolderName <- paste0(folderName, "/modelOutputs")
if (!file.exists(modelFolderName)) {
  dir.create(modelFolderName)
}

# Import species list
focalSpecies <- read.csv("data/external/focalSpecies.csv", header = T)
focalSpecies <- focalSpecies[focalSpecies$selected,]
focalGroups <- unique(focalSpecies$taxonomicGroup)

# Import datasets
regionGeometry <- readRDS(paste0(folderName, "/regionGeometry.RDS"))
environmentalDataList <- readRDS(paste0(tempFolderName, "/environmentalDataImported.RDS"))

if (externalImport == TRUE) {
  
  # When importing externally we need to take steps to transform this back into a series of nested lists
  targetDatabase <- "species_occurrences"
  source("utils/initiateWallaceConnection.R")
  allSpeciesData <- dbReadTable(con, "processed_species_data")
  
  # Now convert data into nested lists
  allSpeciesGroups <- unique(allSpeciesData$taxa)
  speciesData <- lapply(allSpeciesGroups, FUN = function(x) {
    speciesSubset <- allSpeciesData[allSpeciesData$taxa == x,]
    allDatasets <- unique(speciesSubset$datasetName)
    speciesByDataset <- lapply(allDatasets, FUN = function(y) {
      dataSubset <- speciesSubset[speciesSubset$datasetName == y,]
      geometrisedDataSubset <- st_as_sf(dataSubset, coords = c("decimalLongitude", "decimalLatitude"))
      st_crs(geometrisedDataSubset) <- st_crs(regionGeometry)
      geometrisedDataSubset
    })
    names(speciesByDataset) <- allDatasets
    speciesByDataset
  })
  names(speciesData) <- allSpeciesGroups
  
} else {
  speciesData <- readRDS(paste0(tempFolderName, "/speciesDataProcessed.RDS"))
}



source("utils/modelPreparation.R")

###----------------###
### 2. Run models ####
###----------------###

print("Starting model run.")

# Begin running different species groups
for (i in 1:length(focalGroups)) {
  
  # Define species group to create
  focalGroup <- focalGroups[i]
  workflow <- workflowList[[focalGroup]]

  # Add model characteristics (mesh, priors, output)
  workflow$addMesh(cutoff= 20000, max.edge=c(50000, 90000), offset= 100000)
  workflow$specifySpatial(prior.range = c(300000, 0.05),
                          prior.sigma = c(500, 0.2)) #100
  workflow$workflowOutput('Maps')
  workflow$modelOptions(INLA = list(control.inla=list(int.strategy = 'eb', cmin = 0),
                                    safe = TRUE))
  
  # Run model (this directly saves output to folder specified above)
  sdmWorkflow(workflow)
}


###---------------------###
### 3. Download photos ####
###---------------------###

for (taxa in focalGroups) {
  speciesImaged <- focalSpecies$species[focalSpecies$taxonomicGroup == taxa]
  taxaFolder <- paste0("visualisation/hotspotMaps/data/photos/",taxa)
  if (!file.exists(taxaFolder)) {
    dir.create(taxaFolder)
  }
  for (species in speciesImaged) {
    # Create species folder
    speciesFolder <- paste0(taxaFolder,"/",species)
    if (!file.exists(speciesFolder)) {
      dir.create(speciesFolder)
    }
    tryCatch({inatAttempt <- rinat::get_inat_obs(taxon_name = species, maxresults = 10)
    imageURL <- inatAttempt$image_url[!is.na(inatAttempt$image_url) & inatAttempt$image_url != ""][1]
    download.file(imageURL, destfile = paste0(speciesFolder,"/speciesImage.jpg" ), mode = 'wb')}
    , error = function(species){print(paste0("Error for species ",species))})
  }}
