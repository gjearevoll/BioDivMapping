#' Retrieve GBIF Usage Keys Based on Taxa Names
#'
#' This function queries the GBIF (Global Biodiversity Information Facility) database using `rgbif::name_backbone` to retrieve usage keys for given taxa names. It supports querying for multiple taxa names simultaneously and allows for specifying a confidence threshold for the GBIF match.
#'
#' @param names A vector of `character` strings representing taxa names. Each name is passed to separate calls of `rgbif::name_backbone`. The vector can contain one or multiple taxa names.
#' @param confidenceThreshold A `numeric` value between 0 and 100. This parameter specifies the minimum acceptable match confidence level from GBIF. If the match confidence is below this threshold, `NA` is returned. The default value is 95.
#' @param ... Additional named arguments to be passed to `rgbif::name_backbone`. These arguments can be provided either as single elements or as vectors with the same length as `names`. If any arguments have a shorter length than `names`, they will be recycled to match the length.
#'
#' @return A vector containing GBIF usage keys corresponding to each taxa name provided in `names`. If a match does not meet the confidence threshold or if no match is found, `NA` is returned for that specific taxa name.
#'
#' @export
#'
#' @examples
#' getUsageKeys(c("Puma concolor", "Panthera leo"), confidenceThreshold = 90, rank = "species", strict = TRUE)
#' 
getUsageKeys <- function(names, confidenceThreshold = 95, ...) {
  if(confidenceThreshold < 0 | confidenceThreshold > 100){
    stop("confidenceThreshold must be a value between 0 and 100")
  }
  
  # define function to get keys
  getKey <- function(confidenceThreshold, ...){
    Map(function(ct, ...) {
      args <- list(...)
      if(is.na(args$name)) return(NA)
      backbone <- do.call(rgbif::name_backbone, args)
      
      if (!is.null(backbone$confidence) && backbone$confidence >= ct &
          backbone$matchType != "NONE") {
        return(backbone$usageKey)
      } else {
        return(NA)
      }
    }, ct = confidenceThreshold, ...)
  }
  
  # combine additional arguments with names 
  argsList <- c(confidenceThreshold = confidenceThreshold, list(name = names), list(...))
  
  # get keys 
  keys <- do.call(getKey, argsList)
  
  # return keys 
  if(length(names) == 1) {
    return(keys[[1]])
  } else {
    return(unlist(keys))
  }
}
