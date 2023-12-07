#' @title \emph{defineBiasFields}: defines the datasets to calculate bias fields for when running our species models

#' @description This function goes through the datasets available for our separate taxa dn decides which ones will be used to create sampling bias fields for our model. The choice goes 1) did we dictate a list 
#' of datasets in the function?. If so, just use these for every taxa. 2) Did we pre-define datastes to be used as sampling biases in our metadata file?
#' If so, check which datasets are used by each taxa and keep the ones that are. 3) If there  is no pre-definition, we simply use every dataset for which there is data for a taxa.
#' 
#'
#' @param focalTaxaRun A character vector with the names of all taxa involved in the species model run.
#' @param dataTypes A list of dataset names with a boolean 'bias' column dictating whether or not they are to be used in calculating sampling bias.
#' @param speciesData A processed species dataset.
#' @param redList Our red-listed species list.
#' @param datasets A list of datasets to be used for each taxa.
#' 
#' 
#' @return An list of character vectors, each containing the datasets that should be used for respective taxa.
#'



defineBiasFields <- function(focalTaxaRun, dataTypes, speciesData, redList, datasets = NA) {
  # Check whether we have a pre-defined set of datasets
  if (!is.na(datasets)) {
    biasFields <- rep(list(datasets), length(focalTaxaRun))
    return(biasFields)
  }
  
  biasFieldList <- list()
  # Go through species by species
  for (t in seq_along(focalTaxaRun)) {
    # See which individual species will be modelled
    redListedSpecies <- redList$GBIFName[redList$taxa %in% focalTaxaRun[t] & redList$valid == TRUE]
    
    # Find which datasets have data for this taxa
    useableDatasets <- sapply(speciesData, FUN = function(x) {nrow(x[x$acceptedScientificName %in% redListedSpecies,])})
    useableDatasets <- names(useableDatasets[useableDatasets != 0])
    
    # Get suggested datasets from our metadata
    biasedDatasets <- dataTypes[dataTypes$biased,]
    
    # Check if there are datasets that were defined in our metadata file
    if (nrow(biasedDatasets) != 0) {
      suggestedDatasets <- biasedDatasets$name[biasedDatasets$name %in% useableDatasets]
    } else {
      cat(paste0("No suggested datasets found for ", focalTaxaRun[t],
                 ". Generating suggestions for bias fields based on datsets with data for taxa.\n"))
      suggestedDatasets <- useableDatasets
    }
    
    # Turn into shortened form
    suggestedDatasets2 <- sapply(suggestedDatasets, FUN = function(x) {
     gsub("[[:punct:]]", "",gsub(" ", "", x))
    })
    
    biasFieldList[[t]] <- suggestedDatasets2
    # Now find out which
  }
  
  return(biasFieldList)
}
