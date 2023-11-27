#' Retrieve Scientific Names from GBIF Using Usage Keys
#'
#' This function queries the GBIF (Global Biodiversity Information Facility) database
#' to retrieve scientific names associated with given usage keys. It utilizes the `name_usage`
#' function from the `rgbif` package. The function is designed to handle multiple usage keys 
#' and returns a corresponding scientific name for each.
#'
#' @param usageKey A vector of integers or numeric values representing GBIF usage keys. 
#'                 Each usage key is a unique identifier for a taxon in the GBIF database.
#'                 The function can handle a single key or multiple keys.
#'
#' @return A character vector containing the scientific names corresponding to each 
#'         provided usage key. If a scientific name cannot be retrieved for a given key,
#'         `NA` is returned in its place. The length of the returned vector matches 
#'         the length of the input `usageKey` vector.
#'
#' @export
#'
#' @examples
#' getScientificName(1234567) # Single usage key
#' getScientificName(c(1234567, 2345678)) # Multiple usage keys
#'
getScientificName <- function (usageKey) 
 {
     names <- sapply(usageKey, function(key) {
         result <- name_usage(key = key)
         if (!is.null(result$data$scientificName) && nrow(result$data) > 
             0) {
             return(result$data$scientificName)
         }
         else {
             return(NA)
         }
     })
     return(names)
 }

