# combine species modelled as functional groups (FGs)
# by changing their scientific name to their FG name

joinFunctionalGroups <- function(speciesData, focalTaxon) {
  # remove duplicate species in same FG 
  focalTaxon <- unique(focalTaxon[,c("key", "taxa", "scientificName", "functionalGroup")])
  # identify any species in multiple groups
  instances <- table(focalTaxon$key)
  multiFG <- names(instances[instances > 1])
  # check if list of datasets
  if(inherits(speciesData, "sf") && !inherits(speciesData, "list")){
    unlist <- TRUE
    speciesData <- list(data = speciesData)
  } else {
    unlist <- FALSE
  }
  # for each dataset
  speciesData <- lapply(speciesData, function(ds) {
    # make copy of original species names
    ds$scientificName <- ds$acceptedScientificName
    # identify records of species part of one functional group
    # (typically generalists, or if only distinct specialists)
    singleFG <- ds$taxonKeyProject %in% names(instances[instances == 1]) &
      focalTaxon$functionalGroup[  # functional group not blank 
        match(ds$taxonKeyProject, focalTaxon$key)] != ""  
    # rename species part of one functional group 
    ds$acceptedScientificName[singleFG] <-
      focalTaxon$functionalGroup[match(ds$taxonKeyProject[singleFG], 
                                         focalTaxon$key)]
    # return ds if no species in taxa have multiple FGs
    if (length(multiFG) == 0) {
      return(ds)
    }
    # otherwise, identify any species in dataset used in multiple FGs
    # (typically specialists nested in model for generalists, if present, 
    # or species modelled in group and individually)
    multiFG.ds <- multiFG[multiFG %in% unique(ds$taxonKeyProject)]
    # return ds if no species in dataset are used in multiple FGs
    if (length(multiFG.ds) == 0) {
      return(ds)
    }
    # otherwise, loop through each multi FG species to duplicate records and change species name
    ds.multiFG <- do.call(rbind, lapply(multiFG.ds, function(key) {
      # identify rows to be replicated
      species_rows <- ds[ds$taxonKeyProject == key, ] 
      # define FG replacement names, using species for blank groups 
      fg <- focalTaxon[focalTaxon$key == key, ]
      fg <- ifelse(fg$functionalGroup == "", "", fg$functionalGroup)
      
      # Replicate rows for species with multiple FGs
      replicated_rows <- do.call(rbind, replicate(length(fg), species_rows, simplify = FALSE))
      # replacement names ("" for species where modelled individually and with functionalGroup)
      newName <- rep(fg, each = nrow(species_rows)) 
      replicated_rows$acceptedScientificName[newName != ""] <- newName[newName != ""]  # assign name
      return(replicated_rows) 
    }))
    # combine with remaining data (i.e., species modelled separately or in one FG)
    return(rbind(ds.multiFG, ds[!(ds$taxonKeyProject %in% multiFG.ds), ]))
  })
  # unlist if data was sf
  if(unlist){
    speciesData <- speciesData[[1]]
  }
  
  return(speciesData)
}

