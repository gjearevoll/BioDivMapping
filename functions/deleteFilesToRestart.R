#' This function removes any saved data for a particular run date if we wish to restart the compilation of that data
#'
#' @param dateAccessed Date the data was downloaded
#'
#' @return Initialised files and folders are deleted
#'
#' @export
#' 
#' 


deleteFilesToRestart <- function(dateAccessed){
  path <- paste0("data/run_", dateAccessed)
  idTemp <-  grep("temp", dir(path))
  idDownloadKey <- grep("downloadKey", dir(path))
  
  # get the full path of files to delete
  todelete <- dir(path, full.names = TRUE)[-c(idTemp, idDownloadKey)]
  
  cat(paste("Deleting the following files:",  todelete))
  unlink(todelete)
  
  #now I delete folders that are not needed
  unlink(paste0("data/run_", dateAccessed, "/modelOutputs"), recursive = TRUE)
}
