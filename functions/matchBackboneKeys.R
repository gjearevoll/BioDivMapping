#' @title \emph{matchBackboneKeys}: Check which of our taxa this species belongs to
#' 
#' @description This function looks up species' accepted scientific names in GBIF and checks whether they belong to any of the taxonomic groups.
#'
#' @param speciesBackbones A data.frame of species backbones as returned by 'getGbifBackbone'
#' @param taxaKeys A vector of taxonomic keys for the taxa we're importing.
#' 
#' @return The corresponding taxonomic key. If none - we get an NA.
#'
#'

matchBackboneKeys <- function(speciesBackbones, taxaKeys){
  # filter taxa key  
  out <- ifelse(speciesBackbones$kingdomKey %in% taxaKeys, speciesBackbones$kingdomKey,
                ifelse(speciesBackbones$phylumKey %in% taxaKeys, speciesBackbones$phylumKey,
                       ifelse(speciesBackbones$classKey %in% taxaKeys, speciesBackbones$classKey,
                              ifelse(speciesBackbones$orderKey %in% taxaKeys, speciesBackbones$orderKey,
                                     ifelse(speciesBackbones$familyKey %in% taxaKeys, speciesBackbones$familyKey,
                                            ifelse(speciesBackbones$genusKey %in% taxaKeys, speciesBackbones$genusKey,
                                                   ifelse(speciesBackbones$speciesKey %in% taxaKeys, speciesBackbones$speciesKey, NA)))))))
  # return
  return(out)
}