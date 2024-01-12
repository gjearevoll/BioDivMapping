
#' @title \emph{importRedList}: Import species from the Artsdatabanken red list

#' @description This function pulls a species dataframe from the AArrtsdatabanken red list going by the category of threat they are facing.
#'
#' @param categories A vector of character codes dictating which categories of species (by threatened status) should be downloaded.
#' 
#' @return A dataframe with all red-listed species in the given categories.
#' 

importRedList <- function(categories) {
  redListCategories <- lapply(categories, FUN = function(x) {
    apiQuery <- httr::GET(paste0("https://artsdatabanken.no/api/Resource/?Take=999999&Type=taxon&Tags=Kategori/", x))
    charChange <- rawToChar(apiQuery$content)
    Encoding(charChange) <- "UTF-8" #The encoding for fromJSON should be UTF-8
    data <- jsonlite::fromJSON(charChange)
    cbind(data$AcceptedNameUsage$ScientificName, data$Kategori)}
  )
  
  fullList <- as.data.frame(do.call(rbind, redListCategories))
  colnames(fullList) <- c("species", "status")
  return(fullList)
}
