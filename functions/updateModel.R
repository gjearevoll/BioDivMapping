
#' @title \emph{updateModel}: Update model parameters quickly

#' @description Here we update any of the model parameters already defined and resave them.
#'
#' @param dateAccessed A character code which defines which run folder we access.
#' @param object A characteer string which gives the name of the object to be changed
#' @param newValue New value should be assigned to the object in the relevant file.
#' 
#' @return No output per se, but your files should reflect the changes made here.
#' 


updateModel <- function(dateAccessed, object = NULL, newValue = NULL, ...){
  folderName <- paste0("data/run_", dateAccessed)
  args <- list(...)
  if(length(args) == 0 & is.null(object)){
    stop("Must specify either 'object' or '...'.")
  } else if(length(args) > 0 & !is.null(object)){
    stop("Cannot provide both 'object' and '...'. Please sepcify one or the other.")
  } else {
    if(!is.null(object)){
      args <- list()
      if(is.null(newValue)) {
        args[object] <- list(newValue)
      } else if (!(object %in% c("myMesh", "prior.range", "prior.sigma"))) {
        args[object] <- newValue
      } else {
        args[[object]] <- newValue
      }
    } 
  }
  for (i in seq_along(args)) {
    object <- names(args[i])
    newValue <- args[[i]]
    # update csv
    if (object %in% c("focalTaxa", "focalCovariates", "metadataSummary", "polyphyleticSpecies")){
      if (is.null(newValue)) {
        message(sprintf("Updating '%s/%s.csv' from 'data/external/%s.csv'", folderName, object, object))
        newValue <- read.csv(sprintf("data/external/%s.csv", object))
        write.csv(newValue, sprintf("%s/%s.csv", folderName, object), row.names = FALSE)
      } else if(inherits(newValue, "data.frame")){
        write.csv(newValue, sprintf("%s/%s.csv", folderName, object), row.names = FALSE)
      } else if(inherits(newValue, "character")){
        if (grepl("\\.csv$", newValue)) {
          # delete original
          unlink(sprintf("%s/%s.csv", folderName, object))
          # move to new folder
          file.copy(newValue, sprintf("%s/%s.csv", folderName, object))
        } else {
          browser() # not file path, not sure what to do
          message("35")
        }
      }
    } else if(object %in% c("downloadKey")){  # update RDS file
      if(inherits(newValue, "character")){
        if (grepl("\\.RDS$|\\.rds$", newValue)) {
          if(file.exists(newValue)){
            rds <- readRDS(newValue)
            saveRDS(rds, sprintf("%s/%s.RDS", folderName, object))
          } else {
            browser()
            message("downloadKey path does not exist")
          }
        } else {
          browser() # not file path, not sure what to do
          message("downloadKey path is not a file")
        }
      } else {
        browser() # not file path, not sure what to do
        message("downloadKey path is not a character")
      }
    } else {
      if(! object %in% c("level", "region", "crs", "predRes", "concavity", "res", "myMesh", "uploadToWallace", "scheduledDownload", 
                         "waitForGbif", "redListThreshold", "minDatasetRecords", "minSpeciesRecords", "redListCategories", 
                         "modelRun", "nSegment", "prior.range", "prior.sigma", "parallelisation", "downloadANOData",
                         "speciesSpatial", "pointsSpatial", "spatialBlock", "groupPO")){
        warning(sprintf("'%s' is not explicitly handled; adding as objcet as element to 'controlPars'.", object))
      }
      # import objects from run folder
      controlPars <- readRDS(paste0(folderName,"/controlPars.RDS"))
      # update from environment
      if(is.null(newValue)){
        if (object %in% ls(envir = .GlobalEnv)) {
          newValue <- get(object, envir = .GlobalEnv)
          warning(sprintf("'newValue' is 'NULL'. Assigning following value from the global environment: \n %s = %s\n", 
                          object,  
                          gsub("\\s{2,}", " ", paste(capture.output(dput(newValue)), collapse = ""))))                
          # update target object using as it appears in global environment
          controlPars[[object]] <- newValue
        } else {
          warning(sprintf("'newValue' is 'NULL' and not defined in the global environment. Assigning 'NULL' value to '%s'\n", 
                          object))                
          controlPars[[object]] <- newValue
        }
      } else {
        # update target object using as it appears in global environment
        controlPars[[object]] <- newValue    
      }
      # save control pars 
      saveRDS(controlPars, paste0(folderName,"/controlPars.RDS"))
    }
  }
}
