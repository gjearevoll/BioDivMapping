#' @title \emph{importRedList}: Import species from the Artsdatabanken red list

#' @description This function pulls a species dataframe from the Artsdatabanken red list (2021) going by the category of threat they are facing.
#'
#' @param categories A vector of character codes dictating which categories of species (by threatened status) should be downloaded.
#' @param area Assessment area: "N" for mainland Norway, "S" for Svalbard.
#' @param includeAdjusted Whether to also include categories adjusted one step down for immigration from neighbouring populations (e.g. "VUº").
#'
#' @return A dataframe with all red-listed species in the given categories.
#'

importRedListNew <- function(categories, area = "N", includeAdjusted = TRUE) {
  if (includeAdjusted) categories <- c(categories, paste0(categories, "º"))
  
  query <- list(
    `$filter` = paste0("assessmentArea eq '", area, "' and category in (",
                       paste0("'", categories, "'", collapse = ","), ")"),
    `$select` = "scientificName,category,taxonRank,speciesGroup"
  )
  apiQuery <- httr::GET("https://lister.artsdatabanken.no/odata/v1/SpeciesAssessment2021", query = query)
  
  # The API returns 100 rows at a time, with a link to the next page
  pages <- list()
  repeat {
    httr::stop_for_status(apiQuery)
    data <- jsonlite::fromJSON(httr::content(apiQuery, as = "text", encoding = "UTF-8"))
    pages[[length(pages) + 1]] <- data$value
    if (is.null(data$`@odata.nextLink`)) break
    apiQuery <- httr::GET(data$`@odata.nextLink`)
  }
  
  fullList <- do.call(rbind, pages)
  fullList <- data.frame(species = fullList$scientificName, status = fullList$category,
                         taxonRank = fullList$taxonRank, speciesGroup = fullList$speciesGroup)
  fullList$species <- gsub("\\s*\\(.*?\\)", "", fullList$species)
  fullList$species <- gsub("__", "_", fullList$species)
  return(fullList)
}
