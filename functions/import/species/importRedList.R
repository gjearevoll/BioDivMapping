library(httr)
library(jsonlite)

importRedList <- function(categories) {
  redListCategories <- lapply(categories, FUN = function(x) {
    apiQuery <- GET(paste0("https://artsdatabanken.no/api/Resource/?Take=999999&Type=taxon&Tags=Kategori/", x))
    data <- fromJSON(rawToChar(apiQuery$content))
    cbind(data$AcceptedNameUsage$ScientificName, data$Kategori)}
  )
  
  fullList <- as.data.frame(do.call(rbind, redListCategories))
  colnames(fullList) <- c("species", "status")
  return(fullList)
}
