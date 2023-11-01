
#' @title \emph{modelPreparation}: Get a download key for a scheduled GBIF download

#' @description This function initiates a download of a given group of taxa through GBIF.
#'
#' @param focalTaxa A vector of the taxonomic groups we are modelling.
#' @param speciesData A list of processed datasets to be used in the intSDM models.
#' @param redListModelled A vector of red listed species found in the datasets with enough occurrences for a decent model.
#' @param regionGeometry An sf object encompassing our region of study, as produced by defineRegion.
#' @param modelFolderName The directory where model outputs should be saved.
#' @param environmentalDataList A list of raster giving relevant environmental variables.
#' 
#' @return An R6 environment object with enough information to run an intSDM model.

modelPreparation <- function(focalTaxa, speciesData, redListModelled, regionGeometry, modelFolderName, environmentalDataList) {
  
  workflowList <- list()
  
  # Begin running different species groups
  for (i in 1:length(focalTaxa)) {
    
    # Define species group to create
    focalGroup <- focalTaxa[i]
    
    # We need to remove all unnecessary species datasets from the species data
    focalSpeciesDataRefined <- lapply(speciesData, FUN = function(x) {
      focalDataset <- x[x$taxa %in% focalGroup & x$acceptedScientificName %in% redListModelled & !is.na(x$acceptedScientificName),]
      if (nrow(focalDataset) == 0) {
        focalDataset <- NA
      }
      focalDataset
    })
    focalSpeciesDataRefined <- focalSpeciesDataRefined[!is.na(focalSpeciesDataRefined)]
    
    if (length(focalSpeciesDataRefined) == 0) {
      print(paste0("No data at all for ", focalGroup))
      next
    }  
    
    
    # Get species list
    speciesList <- lapply(focalSpeciesDataRefined, FUN = function(x) {
      unique(x$acceptedScientificName)
    })
    speciesList <- unique(do.call(c, speciesList))
    
    # Initialise workflow, creating folder for model result storage
    workflow <- startWorkflow(
      Projection = '+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs',
      Species = speciesList,
      saveOptions = list(projectDirectory = modelFolderName, projectName =  focalGroup), Save = TRUE
    )
    workflow$addArea(Object = st_sf(regionGeometry))
    
    # Add datasets - note that for the moment this excludes the NTNU field notes and ANO,
    # the model will currently not run with these involved
    for (l in c(1:length(focalSpeciesDataRefined))) {
      dataset <- focalSpeciesDataRefined[[l]]
      
      
      dataType <- unique(dataset$dataType)
      datasetName <- gsub(" ", "", gsub("[[:punct:]]", "", names(focalSpeciesDataRefined)[l]))
      
      workflow$addStructured(dataStructured = dataset,
                             datasetType = dataType,
                             datasetName = datasetName,
                             responseName = 'individualCount',
                             speciesName = 'acceptedScientificName')
    }
    
    # Add environmental characteristics
    for (e in 1:nlyr(environmentalDataList)) {
      cat(sprintf("Adding covariate '%s' to the model.\n", names(environmentalDataList)[e]))
      workflow$addCovariates(Object = environmentalDataList[[e]])
    }
    
    workflowList[[focalGroup]] <- workflow
    
  }
  return(workflowList)
}