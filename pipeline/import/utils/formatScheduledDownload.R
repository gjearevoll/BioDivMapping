

#### SPECIES RICHNESS CONVERSION ####

# This script takes your download key and downloads and produces a data frame containing all your 
# relevant species occurrences.

# Download and unzip the file from GBIF
downloadGet <- occ_download_get(key=downloadKey$key, path=paste0(tempFolderName), overwrite=TRUE)
occurrences <- occ_download_import(downloadGet)

# Reduce to relevant columns immediately to save space
occurrences <- occurrences[,c("acceptedScientificName", "decimalLongitude", "decimalLatitude", "basisOfRecord",
                              "year", "month", "datasetKey", "datasetName", "taxonRank", "kingdomKey", "phylumKey", "classKey", "orderKey",
                              "familyKey", "genusKey", "speciesKey", "issue")]

# Filter down to relevant datasets
issuesToFlag <- c("ZERO_COORDINATE|COORDINATE_OUT_OF_RANGE|COORDINATE_INVALID|COORDINATE_PRECISION_INVALID|COORDINATE_UNCERTAINTY_METRES_INVALID")
occurrences <- occurrences %>%
  filter(datasetKey %in% dataTypes$datasetKey[!is.na(dataTypes$processing)]) %>%
  filter(grepl(issuesToFlag,issue))

# Assign a taxon key based on what level of taxonomy the key is valid for
occurrences$taxonKeyProject <- ifelse(occurrences$kingdomKey %in% focalTaxon$key, occurrences$kingdomKey,
                                      ifelse(occurrences$phylumKey %in% focalTaxon$key, occurrences$phylumKey,
                                             ifelse(occurrences$classKey %in% focalTaxon$key, occurrences$classKey,
                                                    ifelse(occurrences$orderKey %in% focalTaxon$key, occurrences$orderKey,
                                                           ifelse(occurrences$familyKey %in% focalTaxon$key, occurrences$familyKey,
                                                                  ifelse(occurrences$genusKey %in% focalTaxon$key, occurrences$genusKey,
                                                                         ifelse(occurrences$speciesKey %in% focalTaxon$key, occurrences$speciesKey, NA)))))))

# Add taxa and reduce to relevant columns
occurrences$taxa <- focalTaxon$taxa[match(occurrences$taxonKeyProject, focalTaxon$key)]
