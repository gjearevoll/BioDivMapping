###------------------------------###
### 2. Attach relevant metadata ####
###------------------------------###

# This script can return up to three objects

# 1. A shortened data frame giving key info regarding each unique dataset - this is mandatory
# 1. A full list of all metadata associated with each unique dataset in the GBIF import - this is optional
# 3. A table giving an overview of the amoutn of data located in each different dataset - also optional

metadataPrep <- function(GBIFDataFrame, fullMeta = FALSE, metaSummary = FALSE) {
  # Start on data frame 1
  metadataAddition <- data.frame(datasetKey = unique(GBIFDataFrame$datasetKey))
  
  # Whether or not we are going to provide the full metadata at the end of this, it still helps to have all the data
  # on hand, so we download all metadata sets correspondent to the initial data frame
  fullMetadata <- lapply(metadataAddition$datasetKey, FUN = function(x) {
    metadataAttempt <- rgbif::datasets(data = "all",uuid = x)
    metadataAttempt$data
  }
  )
  names(fullMetadata) <- metadataAddition$datasetKey
  
  # Get the type of data
  metadataAddition$type <- unlist(lapply(metadataAddition$datasetKey, FUN = function(x) {
    fullMetadata[[x]]$type
  }
  ))
  
  # Name of dataset
  metadataAddition$name <- unlist(lapply(metadataAddition$datasetKey, FUN = function(x) {
    fullMetadata[[x]]$title
  }
  ))
  
  # And get an endpoitn we can download it from if need be
  metadataAddition$DWCEndpoint <- unlist(lapply(metadataAddition$datasetKey, FUN = function(x) {
    endpoints <- fullMetadata[[x]]$endpoints
    endpoint <- endpoints$url[endpoints$type == "DWC_ARCHIVE"]
    ifelse(length(endpoint) == 0, NA, endpoint)
  }
  ))
  
  # Start the list that will be returned
  metadataList <- list(metadata = metadataAddition)
  
  # Append data object 2, if it is wanted
  if (fullMeta == TRUE) {
    metadataList <- append(metadataList, list(fullMetadata = fullMetadata))
  }
  
  rm("fullMetadata")
  
  # create a summary of the amount of data each set is providing, if it is requested
  if (metaSummary == TRUE) {
    GBIFDataByDataset <- GBIFImportCompiled %>%
      group_by(datasetKey, datasetName) %>% tally() %>% arrange(-n) %>%
      as.data.frame()
    GBIFWithMeta <- merge(GBIFDataByDataset, metadataAddition, all.x = TRUE, by = "datasetKey")
    metadataSummary <- GBIFWithMeta %>%
      group_by(name) %>%
      summarise(obsv = sum(n)) %>%
      arrange(-obsv) %>%
      as.data.frame()
    metadataSummary$percent <- round(100*metadataSummary$obsv/sum(metadataSummary$obsv),2)
    metadataList <- append(metadataList, list(metadataSummary = metadataSummary))  
    rm("GBIFDataByDataset", "GBIFWithMeta")
  }
  
  return(metadataList)
}
