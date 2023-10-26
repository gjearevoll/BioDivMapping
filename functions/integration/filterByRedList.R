filterByRedList <- function(redListSpecies, processedData, threshold) {
  
  processedDataBySpecies <- processedData %>%
    group_by(acceptedScientificName) %>%
    filter(acceptedScientificName %in% redListSpecies) %>%
    tally()
  
  validSpecies <- processedDataBySpecies$acceptedScientificName[processedDataBySpecies$n > threshold]
  invalidSpecies <- processedDataBySpecies$acceptedScientificName[processedDataBySpecies$n <= threshold]
  speciesLists <- list(validSpecies = validSpecies, invalidSpecies = invalidSpecies)
}
