
#' @title \emph{filterByRedList}: This function extracts a list of red-listed species with sufficient adata from a dataframe

#' @description This function takes a data frame and cuts it down to give a list of species found on the Norwegian red list with a sufficient number of observations.
#'
#' @param redListSpecies A character vector of species names.
#' @param processedData A large data frame, which must contain a column named 'acceptedScientificName' and contain names in the same format as the redListSpecies vector.
#' @param threshold The minimum number of observations throughout the dataset required for a species to be 'valid'.
#' 
#' @return A list with two character vectors, one containing all red listed species with sufficient data, one containing red-list species without sufficient data.
#'

filterByRedList <- function(redListSpecies, processedData, threshold) {
  
  processedDataBySpecies <- processedData %>%
    group_by(acceptedScientificName) %>%
    filter(acceptedScientificName %in% redListSpecies) %>%
    tally()
  
  validSpecies <- processedDataBySpecies$acceptedScientificName[processedDataBySpecies$n > threshold]
  invalidSpecies <- processedDataBySpecies$acceptedScientificName[processedDataBySpecies$n <= threshold]
  speciesLists <- list(validSpecies = validSpecies, invalidSpecies = invalidSpecies)
}
