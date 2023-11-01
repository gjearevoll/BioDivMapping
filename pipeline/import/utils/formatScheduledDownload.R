

#### SPECIES RICHNESS CONVERSION ####

# This script takes your download key and downloads and produces a data frame containing all your 
# relevant species occurrences.

# Download and unzip the file from GBIF
download.file(url=downloadKey$downloadLink,
              destfile=paste0(tempFolderName, "/GBIFImport.zip"),
              quiet=TRUE, mode="wb")
unzip(paste0(tempFolderName, "/GBIFImport.zip"), exdir = paste0(tempFolderName, "/GBIFImport"))

# Import the occurrences
occurrences <- read.delim(paste0(tempFolderName, "/GBIFImport/occurrence.txt"))

# Filter down to relevant datasets
occurrences <- occurrences %>%
  filter(datasetKey %in% dataTypes$datasetKey[!is.na(dataTypes$dataType)])

# Assign a taxon key based on what level of taxonomy the key is valid for
occurrences$taxonKeyProject <- ifelse(occurrences$kingdomKey %in% focalTaxon$key, occurrences$kingdomKey,
                                      ifelse(occurrences$phylumKey %in% focalTaxon$key, occurrences$phylumKey,
                                             ifelse(occurrences$classKey %in% focalTaxon$key, occurrences$classKey,
                                                    ifelse(occurrences$orderKey %in% focalTaxon$key, occurrences$orderKey,
                                                           ifelse(occurrences$familyKey %in% focalTaxon$key, occurrences$familyKey,
                                                                  ifelse(occurrences$familyKey %in% focalTaxon$key, occurrences$genusKey,
                                                                         ifelse(occurrences$familyKey %in% focalTaxon$key, occurrences$speciesKey, NA)))))))

# Add taxa and reduce to relevant columns
occurrences$taxa <- focalTaxon$taxa[match(occurrences$taxonKeyProject, focalTaxon$key)]
occurrences$simpleScientificName <- gsub(" ", "_", word(occurrences$acceptedScientificName, 1,2, sep=" "))
