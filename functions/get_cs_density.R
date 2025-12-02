
#' @title \emph{get_cs_density}: Create a kernel density estimate for sampling bias across Norway

#' @description This function takes the imported species data and create a kernel density
#' estimate to give sampling density across the years specified. It only works for temporal data at the moment.
#'
#' @param dateAccessed Indicates which date folder should be used to find the species data.
#' @param regionGeometry Region for which to calculate density.
#' @param citizenDatasets Names of datasets to derive sampling density from.
#' @param yearInterval Years for which data needs to be derived
#' @param crs CRS for the project
#'
#' @return An aggregated raster containing cs density data for Norway.
#'
#' @import spatstat
#' @import dplyr


get_cs_density <- function(dateAccessed, regionGeometry, citizenDatasets, yearInterval, crs) {
  
  # define repo folder names
  folderName <- paste0("data/run_", dateAccessed)
  tempFolderName <- paste0(folderName, "/temp")
  
  
  speciesDataList <- readRDS(paste0(tempFolderName, "/speciesDataImported.RDS"))[["species"]]
  
  cityOwin <- as.owin(sf::st_as_sf(vect(regionGeometry)))
  
  # Dictate numebr of samples to take
  
  
  processedObsData <- do.call(rbind, speciesDataList[citizenDatasets]) %>%
  sample_frac(0.05)

  negative_number <- function(i) i[i <=0]
  processedObsData$year <- sapply(processedObsData$year, function(x) {x + max(negative_number(yearInterval - x))})
  
  
  KF <- 0.04
  dimensions <- c(3000,2800)
  
  intervalData <- lapply(yearInterval, FUN = function(x) {
    yearData <- processedObsData[processedObsData$year %in% x,]
    #sampleNumber <- if (nrow(yearData) > 200000) 200000 else nrow(yearData)
    sampledData <- yearData %>%
      dplyr::select("acceptedScientificName", "geometry")# %>%
      #sample_n(sampleNumber)
    print("Finished sampling data")
    vectorised <- terra::project(vect(sampledData), paste0("EPSG:", crs))
    points <- terra::crds(vectorised)
    p <- ppp(points[,1], points[,2], window = cityOwin)
    
    print("Finished vectorising data")
    ds <- density(p, adjust = KF, dimyx = dimensions)
    loggedDensity <- log(ds+0.000001)
    rast(loggedDensity)
    
  }) |> setNames(yearInterval)
  
  rast(lapply(rast(intervalData), FUN = function(x) {
    crs(x) <- paste0("EPSG:", crs)
    x
  }))
  
  
}
  

