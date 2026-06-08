
#' @title \emph{processCitizenScience}: Thins out multiple citizen science observations in the same pixel in individual datasets

#' @description Overly intense sampling of citizen science within one pixel creates extreme levels of sampling bias that needs to be calmed down for this process.
#'
#' @param dataset The citizen science dataset to be thinned. Should be an sf points data frame.
#' @param regionGeometry A directory in which to save the data downloaded directly from the source.
#' @param crs The crs to be used for projecting onto
#' 
#' @return A new dataset with excess observations removed.
#'
#' @import sf
#' 
#' 
#' 
processCitizenScience <- function(dataset, regionGeometry, crs, tempFolderName, datasetName) {
  
  cat("\tThinning",datasetName, "data\n")
  
  dataset$id <- c(1:nrow(dataset))
  datasetVect <- vect(dataset[,c("acceptedScientificName", "id")])
  
  # Import env data and get the right crs
  e <- ext(vect(regionGeometry))
  emptyRaster <- rast(e, res = 500, crs = "EPSG:25833 ")
  emptyRasterConv <- project(emptyRaster, crs)
  
  dataCell <- setValues(emptyRasterConv, 1:ncell(emptyRasterConv)) 
  
  
  datasetVect$cell <- extract(dataCell, datasetVect)[,2]
  datasetFrame <- as.data.frame(datasetVect)
  dataUnique <- datasetFrame[!duplicated(datasetFrame[,c("acceptedScientificName", "cell")]),]
  #nbicDataSubset$cell <- extract(dataCell, nbicDataSubset)[,2]

  datasetUnique <- dataset[dataset$id %in% dataUnique$id,names(dataset)[!(names(dataset) %in% "id")]]
  
  if (!dir.exists(paste0(tempFolderName,"/", datasetName))) {
    dir.create(paste0(tempFolderName,"/", datasetName))
  }
  
  newDataset <- datasetUnique[,c("acceptedScientificName", "geometry", "dataType", "taxa", "year", "taxonKeyProject")]
  newDataset <- st_transform(newDataset, crs)
  
  saveRDS(newDataset, paste0(tempFolderName,"/", datasetName ,"/processedDataset.RDS"))
  
  return(newDataset)
  
  
}