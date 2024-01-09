
#' @title \emph{modelPreparation}: Get a download key for a scheduled GBIF download

#' @description This function initiates a download of a given group of taxa through GBIF.
#'
#' @param focalTaxa A vector of the taxonomic groups we are modelling.
#' @param speciesData A list of processed datasets to be used in the intSDM models.
#' @param redListModelled A vector of red listed species found in the datasets with enough occurrences for a decent model. If NULL, all species will be modelled.
#' @param regionGeometry An sf object encompassing our region of study, as produced by defineRegion.
#' @param modelFolderName The directory where model outputs should be saved.
#' @param environmentalDataList A list of raster giving relevant environmental variables.
#' @param crs The coordinate reference system used in the workflow.
#' 
#' @return An R6 environment object with enough information to run an intSDM model.
#' @importFrom terra nlyr
#' @importFrom sf st_sf
#' @importFrom intSDM startWorkflow
#' 
modelPreparation <- function(focalTaxa, speciesData, redListModelled = NULL, regionGeometry, modelFolderName, environmentalDataList = NULL, crs = NULL) {
  
  if(is.null(crs)){
    if(!is.null(environmentalDataList)){
      crs <- crs(environmentalDataList)
    } else {
      # get crs from regionGeometry
      crs <- st_crs(regionGeometry)
      # convert lat/long projection to UTM
      if(st_crs(crs)$units_gdal == "degree"){
        crs <- utmCRS(regionGeometry)
      }
    }
  }
  
  workflowList <- list()
  # Begin running different species groups
  for (focalTaxon in unique(focalTaxa$taxa)) {
    
    # We need to use only species that are 
    # a) in the right taxa
    # b) are in our list of red list species to model (this can also be removed if we just want to model all species)
    # c) have a valid accepted scientific name
    focalSpeciesDataRefined <- lapply(speciesData, FUN = function(x) {
      focalDataset <- x[x$taxa %in% focalTaxon & 
                          if(is.null(redListModelled)) {
                            T
                          } else {
                            x$acceptedScientificName %in% redListModelled
                          } &
                          !is.na(x$acceptedScientificName),]
      if (nrow(focalDataset) == 0) {
        focalDataset <- NA
      }
      focalDataset
    })
    focalSpeciesDataRefined <- focalSpeciesDataRefined[!is.na(focalSpeciesDataRefined)]
    
    # Eliminate taxa if no data    
    if (length(focalSpeciesDataRefined) == 0) {
      print(paste0("No data at all for ", focalTaxon))
      next
    }  
    
    # Combine species by functionalGroup if requested (else leave as separate)
    # identify species with data
    uniqueTaxaSpecies <- unique(unlist(lapply(focalSpeciesDataRefined, function(ds){
      as.character(ds$taxonKeyProject)
    })))
    
    # identify functional groups in species with data for focal taxonomic group
    focalSpeciesWithData <- focalTaxa[focalTaxa$key %in% uniqueTaxaSpecies &  # species with data
                                        focalTaxa$taxa %in% focalTaxa,]  # and of focal taxa (in case same species in different taxa)
    # if any species are to be modelled as functional groups
    if(any(!is.na(focalSpeciesWithData$functionalGroup) & focalSpeciesWithData$functionalGroup != "")){
      # update data
      focalSpeciesDataRefined <- joinFunctionalGroups(speciesData = focalSpeciesDataRefined,
                                                         focalTaxon = focalSpeciesWithData) 
      focalTaxonSpecies <- unique(unlist(lapply(focalSpeciesDataRefined, function(ds){
        as.character(ds$acceptedScientificName)
      })))
    }
    
    # Eliminate taxa if no data
    if (length(focalSpeciesDataRefined) == 0) {
      print(paste0("No data at all for ", focalTaxon))
      next
    }  
    
    
    # Get species list
    speciesList <- lapply(focalSpeciesDataRefined, FUN = function(x) {
      unique(x$simpleScientificName)
    })
    speciesList <- unique(do.call(c, speciesList))
    
    # Initialise workflow, creating folder for model result storage
    workflow <- startWorkflow(
      Projection = st_crs(crs)$proj4string,
      Species = speciesList,
      saveOptions = list(projectDirectory = modelFolderName, projectName =  focalTaxon), Save = TRUE
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
                             speciesName = 'simpleScientificName')
    }
    
    # Add environmental characteristics
    env <- if(is.null(environmentalDataList)) 0 else seq(nlyr(environmentalDataList))
    for (e in env) {
      cat(sprintf("Adding covariate '%s' to the model.\n", names(environmentalDataList)[e]))
      workflow$addCovariates(Object = environmentalDataList[[e]])
    }
    
    workflowList[[focalTaxon]] <- workflow
    
  }
  return(workflowList)
}
