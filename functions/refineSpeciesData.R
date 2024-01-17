
#' @title \emph{refineSpeciesData}: Filters down our species data based on which type of model you're about to run.

#' @description The different models require different data. redListSpecies models require only red listed species, and only those who have enough data. redListeRichness models require only red listed species, and only presence data, and speciesRichness models require only presence data. This function applies those filters.
#'
#' @param modelRun The type of model you are running.
#' @param speciesData The processed species data list.
#' 
#' @return A new list of datasets.
#'
#' 
#' 

refineSpeciesData <- function(modelRun, speciesData) {
  

  
  # Cut down to only red-listed species
  if (modelRun %in% c("redListRichness", "redListSpecies")) {
    
    # Select either all red list species or only those with enough occurrences
    redListSelection <- if(modelRun %in% c("redListSpecies")) redList$valid else rep(TRUE, nrow(redList))
    speciesForSelection <- redList$GBIFName[redListSelection]
    
    speciesData <- lapply(speciesData, FUN = function(x) {
      x %>%
        filter(acceptedScientificName %in% speciesForSelection)
    })
  }

  # Cut down to only presence data
  if (modelRun %in% c("richness", "redListRichness")) {
    speciesData <- lapply(speciesData, FUN = function(x) {
      if ("individualCount" %in% colnames(x)) {
        x %>%
          filter(individualCount > 0) %>%
          mutate(individualCount = 1)
      } else {
        x %>%
          mutate(individualCount = 1)
      }
    }
    )
  }
  
  speciesData <- speciesData[lapply(speciesData, FUN = nrow) > 0]
  
  return(speciesData)
  
  
}

