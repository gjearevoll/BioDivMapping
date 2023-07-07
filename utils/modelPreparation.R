

#### MODEL DATA PREPARATION ####

# The following script imports our various forms of environmental data and processes them, based on the type of data
# and other specifications related to the source.

workflowList <- list()

# Begin running different species groups
for (i in 1:length(focalGroups)) {
  
  # Define species group to create
  focalGroup <- focalGroups[i]
  focalGroupSpecies <- focalSpecies$species[focalSpecies$taxonomicGroup %in% focalGroup]
  
  focalSpeciesData <- speciesData
  
  # Initialise workflow, creating folder for model result storage
  workflow <- startWorkflow(
    Projection = '+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs',
    Species = focalGroupSpecies,
    saveOptions = list(projectDirectory = modelFolderName, projectName =  focalGroup), Save = TRUE
  )
  workflow$addArea(Object = st_sf(regionGeometry), resolution = '60')
  
  # Add datasets - note that for the moment this excludes the NTNU field notes and ANO,
  # the model will currently not run with these involved
  for (l in c(1:5)) {
    dataset <- focalSpeciesData[[l]]
    
    if (nrow(dataset) < 5) next
    
    dataType <- unique(dataset$dataType)
    datasetName <- gsub(" ", "", gsub("[[:punct:]]", "", names(focalSpeciesData)[l]))
  
    workflow$addStructured(dataStructured = dataset,
                           datasetType = dataType,
                           datasetName = datasetName,
                           responseName = 'individualCount',
                           speciesName = 'simpleScientificName')
  }
  
  # Add environmental characteristics
  for (e in 1:length(environmentalDataList)) {
       workflow$addCovariates(Object = environmentalDataList[[e]])
  }
    
  workflowList[[focalGroup]] <- workflow
  
}

rm("i", "l", "dataset", "dataType", "datasetName", "workflow", 
   "environmentalDataList", "focalGroup", "focalGroupSpecies")

