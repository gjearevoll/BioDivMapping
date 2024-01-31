
#### INSTANT DOWNLAOD IMPORT ####

# This script is built for smaller, test downloads from GBIF. It does not produce a DOI for the download
# and thus should not be used for anything that will be published or needs a citation.

gbifImportsPerTaxa <- lapply(focalTaxa, FUN = function(x) {
  focalTaxaImport <- focalTaxon$key[focalTaxon$taxa == x]
  GBIFImport <- occ_data(taxonKey = focalTaxaImport, hasCoordinate = TRUE, limit = 3000, 
                         geometry = st_bbox(regionGeometry), coordinateUncertaintyInMeters = '0,100')
  # compile import 
  if(all(names(GBIFImport) == c("meta", "data"))){  # if only one species selected
    GBIFImportCompiled <- compileGBIFImport(GBIFImport)
  } else if(any(names(GBIFImport) %in% focalSpeciesImport)){  # if multiple species 
    GBIFImportCompiled <- do.call(rbind, lapply(GBIFImport, compileGBIFImport))
  }
  GBIFImportCompiled$simpleScientificName <- gsub(" ", "_", word(GBIFImportCompiled$acceptedScientificName, 1,2, sep=" "))
  GBIFImportCompiled$taxa <- x
  GBIFImportCompiled
})