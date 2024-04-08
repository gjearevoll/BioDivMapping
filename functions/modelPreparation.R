
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
modelPreparation <- function(focalTaxa, focalCovariates, speciesData, redListModelled = NULL, regionGeometry, 
                             modelFolderName, environmentalDataList = NULL, crs = NULL, segmentation = FALSE,
                             nSegment = NULL) {
  
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
  
  # Create a list of different species combinations to lower simultaneous computing requirements for larger datasets
  if (segmentation) {
    speciesLists <- list()
    for (focalTaxon in unique(focalTaxa$taxa)) {
      
      # Find the most common species to include in each run
      predictionDataset <- focalTaxa$predictionDataset[focalTaxa$taxa == focalTaxon]
      if (length(predictionDataset) > 1) {predictionDataset <- unique(predictionDataset)}
      speciesCounts <- sort(table(unlist(lapply(speciesData, FUN = function(x) {
        x$simpleScientificName
        }))), TRUE)
      fullSpeciesList <- names(speciesCounts)
      speciesCounts <- speciesCounts[fullSpeciesList %in% unique(speciesData[[predictionDataset]]$simpleScientificName)]
      commonSpecies <- names(speciesCounts)[1]
      restOfSpecies <- fullSpeciesList#[!(fullSpeciesList %in% commonSpecies)]
      
      # Segment rest of species into lists of 8 species
     # print(paste("Splitting ", length(restOfSpecies), "species into", length(restOfSpecies)/nSegment, "groups"))
      segmentedList <- split(restOfSpecies, ceiling(seq_along(restOfSpecies)/nSegment))
      #segments <- rep(1:ceiling(length(restOfSpecies)/nSegment), nSegment)[1:length(restOfSpecies)]
      #segmentedList <- split(restOfSpecies, segments)
      speciesNames <- lapply(segmentedList, FUN = function(x) {
        c(x)#, commonSpecies)
      })
      names(speciesNames) <- paste0(focalTaxon, seq(length(speciesNames)))
      speciesLists[[focalTaxon]] <- speciesNames
    }
    totalList <- do.call(c, speciesLists)
    
    # Create taxa names to use
    names(totalList) <- unlist(lapply(strsplit(names(totalList), '.', fixed = TRUE), '[', 2))
    taxaNames <- names(totalList)
  } else {
    taxaNames <- unique(focalTaxa$taxa)
  }
  
  workflowList <- list()
  focaltaxa <- focalTaxa
  # Begin running different species groups
  for (focalTaxon in taxaNames) {
    
    focalGroup <- if (segmentation) gsub('[[:digit:]]+', '', focalTaxon) else focalTaxon
    
    # We need to use only species that are 
    # a) in the right taxa
    focalSpeciesDataRefined <- lapply(speciesData, FUN = function(x) {
      focalDataset <- if (segmentation) x[x$simpleScientificName %in% totalList[[focalTaxon]],] else x[x$taxa %in% focalTaxon,]
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
    uniqueTaxaSpecies <- unique(bind_rows(focalSpeciesDataRefined)$taxonKeyProject)
    # identify functional groups in species with data for focal taxonomic group
    focalSpeciesWithData <- focalTaxa[focalTaxa$key %in% uniqueTaxaSpecies &  # species with data
                                        focalTaxa$taxa %in% focalGroup,]  # and of focal taxa (in case same species in different taxa)
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

    geometryWithWeight <- st_sf(regionGeometry)
    #geometryWithWeight$weight <- 0.001
    workflow$addArea(Object = st_sf(geometryWithWeight))
    
    # Add datasets - note that for the moment this excludes the NTNU field notes and ANO,
    # the model will currently not run with these involved
    for (l in seq_along(focalSpeciesDataRefined)) {
      dataset <- focalSpeciesDataRefined[[l]]
      
      
      dataType <- unique(dataset$dataType)
      datasetName <- gsub(" ", "", gsub("[[:punct:]]", "", names(focalSpeciesDataRefined)[l]))
      
      workflow$addStructured(dataStructured = dataset,
                             datasetType = dataType,
                             datasetName = datasetName,
                             responseName = 'individualCount',
                             speciesName = 'simpleScientificName')
    }
    
    # Add environmental characteristics. If there is a corresponding column for the focalTaxon in the environmental covariate matrix use that,
    # if not, use all calculated env covariates
    # Reduce focalTaxa to focalCovariates
    env <- colnames(focalTaxa)[colnames(focalTaxa) %in% focalCovariates$parameters]
    focalTaxa <- focalTaxa[,c("taxa", env)]
    focalTaxa <- focalTaxa[focalTaxa$taxa %in% focalGroup,]
    env <- env[apply(focalTaxa[,-1], 2, any)]

    for (e in env) {
      cat(sprintf("Adding covariate '%s' to the model.\n", e))
      workflow$addCovariates(Object = environmentalDataList[[e]])
    }

     focalTaxa <- focaltaxa
    
    workflowList[[focalTaxon]] <- workflow
    
  }
  return(workflowList)
}
