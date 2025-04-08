
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
#' @param nSegment The number of species in each group to run.
#' @param speciesOccurenceThreshold The threshold used to select the number of species occurence required to fit the model
#' @param datasetOccurreneThreshold Datasets with occurrences not up to the threshold are combined together.
#' 
#' @return An R6 environment object with enough information to run an intSDM model.
#' @importFrom terra nlyr
#' @importFrom sf st_sf
#' @importFrom intSDM startWorkflow
#' 
modelPreparation <- function(focalTaxa, focalCovariates, speciesDataAll, redListModelled = NULL, regionGeometry, 
                             modelFolderName, environmentalDataList = NULL, crs = NULL, segmentation = FALSE,
                             nSegment = NULL, speciesOccurenceThreshold = 50, datasetOccurreneThreshold = 10000,
                             mergeDatasets = TRUE,
                             mergeAllDatasets = FALSE,
                             richness = TRUE) {
  
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
    speciesDataList <- list()
    
    # Keep track of the occurrence records in each segment
    nOccurences <- list()
    for (focalTaxon in unique(focalTaxa$taxa)) {
      speciesData <- speciesDataAll
      
      # select the speciesData belonging to a particular taxonomic group
      speciesDataNames <- names(speciesData)
      speciesData <- lapply(as.list(seq_along(speciesData)), function(x){
        # In each dataset, select the species that belong to the focalTaxon
        dat <- speciesData[[x]] %>%
          dplyr::filter(taxa %in% focalTaxon)
        
        if(nrow(dat) > 0){
          ret <- dat
        } else {
          ret <- NULL
        }
        return(ret)
      })
      names(speciesData) <- speciesDataNames
      
      # Remove any empty datasets
      speciesDataForTaxon <- lapply(speciesData, function(x){
        ret <- !is.null(x)
      }
      )%>%
        do.call("c", .)
      
      speciesData <- speciesData[speciesDataForTaxon]
      
      # Find the most common species to include in each run
      # Try to make sure prediction dataset is not more than one
      # And extract the prediction dataset
      predictionDataset <- focalTaxa$predictionDataset[focalTaxa$taxa == focalTaxon]
      if (length(predictionDataset) > 1) {
        predictionDataset <- unique(predictionDataset)
        predictionDataset <- predictionDataset[!is.na(predictionDataset)]
        
      }
      
      # Extract the number of species occurrence within each dataset
      speciesDataNames <- names(speciesData)  
      speciesOccTable <- lapply(as.list(seq_along(speciesData)), FUN = function(x){
        # In each dataset, select the species that belong to the focalTaxon
        dat <- speciesData[[x]] %>%
          dplyr::filter(taxa %in% focalTaxon)
        
        # Extract the species Info
        if(nrow(dat) > 0){
          data.frame(datasetName = speciesDataNames[[x]],
                     speciesInfo = c(dat$simpleScientificName))
        }
      })%>%
        do.call("rbind", .)%>%
        reshape2::acast(., speciesInfo ~ datasetName)
      
      
      # We filter species that do not meet the minumum number of occurence threshold
      speciesInfo <- speciesOccTable%>%
        rowSums() %>%
        sort()
      speciesToRemove <- names(speciesInfo[speciesInfo < speciesOccurenceThreshold])
      cat(paste("Removing", length(speciesToRemove), "species because they have less than", speciesOccurenceThreshold, "occurrences"))
      
      speciesToKeep <- names(speciesInfo[speciesInfo >= speciesOccurenceThreshold])
      cat(paste("Keeping", length(speciesToKeep), "species because they have greater than or equal to", speciesOccurenceThreshold, "occurrences"))
      
      # Remove NAs from the species to Keep
      speciesToKeep <- speciesToKeep[!is.na(speciesToKeep)]
      
      # Let's filter the datasets with species occurrence and 
      # merge datasets that have less that nDatasetOccurrences
      
      occurrencesInEachDataset <- speciesOccTable %>%
        as.data.frame(.)%>%
        mutate(speciesInfo = rownames(.))%>% #Add the species info to filter out the species we don't need
        dplyr::filter(speciesInfo %in% speciesToKeep)%>%
        dplyr::select(-c(speciesInfo))%>%
        t()%>%
        rowSums()%>%
        sort()%>%
        cumsum(.)
      
      # We select datasets that do not meet a certain criterion
      dataToMerge <- TRUE
      if (length(speciesData) == 1) {dataToMerge <- FALSE}
      
      if (mergeDatasets == TRUE & dataToMerge == TRUE) {
        
        if(!mergeAllDatasets){
          
          datasetsToMerge <- names(occurrencesInEachDataset[occurrencesInEachDataset < datasetOccurreneThreshold])
          datasetsToMerge <- datasetsToMerge[!datasetsToMerge %in% predictionDataset]
          print(paste0("Datasets merged into one:", datasetsToMerge ))
          
          #Check if all the datasets are of the same dataType
          mergedDataWithType <- lapply(speciesData[datasetsToMerge], function(x){
            unique(x$dataType)
          })%>%
            do.call("rbind", .)%>%
            as.data.frame(.)%>%
            mutate(datasetName = rownames(.))%>%
            rename(dataType = V1)
        } else {
          datasetsToMerge <- names(occurrencesInEachDataset)
          datasetsToMerge <- datasetsToMerge[!datasetsToMerge %in% predictionDataset]
          print(paste0("Datasets merged into one:", datasetsToMerge ))
          
          #Check if all the datasets are of the same dataType
          mergedDataWithType <- lapply(speciesData[datasetsToMerge], function(x){
            unique(x$dataType)
          })%>%
            do.call("rbind", .)%>%
            as.data.frame(.)%>%
            mutate(datasetName = rownames(.))%>%
            rename(dataType = V1) 
          
        }
        
        uniqueDataType <- unique(mergedDataWithType$dataType)
        
        mergedDatasets <- list()
        #if(length(uniqueDataType) > 1 | !mergeAllDatasets){
        #for(i in seq_along(uniqueDataType)){
        i <- 1
        uniqueDataType <- "PO"
        dataToMerge <- mergedDataWithType[mergedDataWithType$dataType == uniqueDataType, ]
        mergedDatasets[[i]] <- speciesData[dataToMerge$datasetName]%>%
          do.call("rbind", .)%>%
          as.data.frame(., row.names = NULL)
        
        rownames(mergedDatasets[[i]]) <- NULL
        
        mergedDatasets[[i]] <- mergedDatasets[[i]] %>%
          st_as_sf()
        
        names(mergedDatasets)[i] <- paste0("mergedDataset", uniqueDataType)

        #Put the merged dataset together with the rest of the speciesData
        if(!is.null(unlist(speciesData[predictionDataset]))){  
          speciesData <- c(speciesData[predictionDataset], 
                           mergedDatasets,
                           speciesData[!names(speciesData) %in% c(dataToMerge$datasetName, predictionDataset)])
        } else {
          speciesData <- c(mergedDatasets,
                           speciesData[!names(occurrencesInEachDataset) %in% c(dataToMerge$datasetName, predictionDataset)]) 
        }
        
      }
      # This is the dataset we are going to use for this taxonomic group
      #speciesData <- speciesData[!names(speciesData) %in% datasetsToMerge]
      
      # Get the list of all the species      
      speciesCounts <- sort(table(unlist(lapply(speciesData, FUN = function(x) {
        x$simpleScientificName
      }))), TRUE)
      
      # We select the species in speciesToKeep
      fullSpeciesList <- names(speciesCounts)[names(speciesCounts) %in% speciesToKeep]
      
      #Out of these, we check those in the prediction dataset
      speciesCountsInPredDataset <- unique(speciesData[[predictionDataset]]$simpleScientificName)[unique(speciesData[[predictionDataset]]$simpleScientificName) %in% fullSpeciesList]
      
      # These are the list of species in the prediction dataset
      # So that we can have the prediction dataset and the rest of the species that 
      # are not in the prediction dataset
      speciesInPredictionDataset <- speciesCountsInPredDataset
      restOfSpecies <- fullSpeciesList[!(fullSpeciesList %in% speciesInPredictionDataset)]
      
      # Put the two datasets together
      fullSpeciesList <- c(speciesInPredictionDataset, restOfSpecies)
      
      # Segment rest of species into lists of 8 species
      cat(paste("Splitting ", length(fullSpeciesList), "species into", ceiling(length(fullSpeciesList)/nSegment), "groups")) 
      if (length(fullSpeciesList) > 1) {
      groupings <- factor(rep(seq(1, floor(length(fullSpeciesList)/nSegment)), nSegment))
      segmentedList <- split(fullSpeciesList, groupings)
      } else {
        segmentedList <- list(fullSpeciesList)
      }
      # segmentedList <- split(fullSpeciesList, ceiling(seq_along(fullSpeciesList)/nSegment))
      #segments <- rep(1:ceiling(length(restOfSpecies)/nSegment), nSegment)[1:length(restOfSpecies)]
      #segmentedList <- split(restOfSpecies, segments)
      speciesNames <- lapply(segmentedList, FUN = function(x) {
        c(x)#, commonSpecies)
      })
      
      nOccurences <- lapply(as.list(seq_along(segmentedList)), function(x){
        # y <- 
        speciesCounts[segmentedList[[x]]]
      })
      
      names(speciesNames) <- paste0(focalTaxon, seq(length(speciesNames)))
      names(nOccurences) <- names(speciesNames)
      speciesLists[[focalTaxon]] <- speciesNames
      speciesDataList[[focalTaxon]] <- speciesData
      saveRDS(nOccurences, paste0(tempFolderName, "/",focalTaxon,"numberOfOccurrences.RDS"))
    }
    totalList <- do.call(c, speciesLists)
    
    # Create taxa names to use
    names(totalList) <- unlist(lapply(strsplit(names(totalList), '.', fixed = TRUE), '[', 2))
    taxaNames <- names(totalList)
  } else {
    taxaNames <- unique(focalTaxa$taxa)
  }
  
  # names(speciesDataList)
  
  workflowList <- list()
  focaltaxa <- focalTaxa
  # Begin running different species groups
  for (focalTaxon in taxaNames) {
    
    focalGroup <- if (segmentation) gsub('[[:digit:]]+', '', focalTaxon) else focalTaxon
    
    # We need to use only species that are 
    # a) in the right taxa
    focalSpeciesDataRefined <- lapply(speciesDataList[[focalGroup]] , FUN = function(x) {
      focalDataset <- if (segmentation) x[x$simpleScientificName %in% totalList[[focalTaxon]],] else x[x$taxa %in% focalTaxon,]
      if (nrow(focalDataset) == 0) {
        focalDataset <- NA
      }else{
        focalDataset
      }
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
    
    print(paste("Number of occurence records for", focalTaxon, "is", sum(nOccurences[[focalTaxon]])))
    # Initialise workflow, creating folder for model result storage
    workflow <- startWorkflow(
      Projection = st_crs(crs)$proj4string,
      Species = speciesList,
      Richness = richness,
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
    quadratics <- paste0(focalCovariates$parameters[focalCovariates$quadratic & focalCovariates$parameters %in% env], "_squared")
    env <- c(env, quadratics)
    
    for (e in env) {
      cat(sprintf("Adding covariate '%s' to the model.\n", e))
      workflow$addCovariates(Object = environmentalDataList[[e]])
    }
    
    focalTaxa <- focaltaxa
    
    workflowList[[focalTaxon]] <- workflow
    
  }
  return(workflowList)
}
