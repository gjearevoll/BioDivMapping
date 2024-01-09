#' @title \emph{createPredictionData}: Creates a prediction field for the pipeline run to project projections into

#' @description We need to define the resolution and projection of the predictions that are outputted from the intSDM. This function creates that.
#'
#' @param resolution A numeric vector giving the size of the pixels which should be used in the prediction projection.
#' @param regionGeometry An sf multipolygon object giving the region that the projection should cover.
#' @param proj The projection's crs.
#' 
#' 
#' @return An sf object as a collection of points covering the specified region
#'





createPredictionData <- function(resolution, regionGeometry, proj = "Proj.4 +proj=utm +zone=32 +datum=WGS84 +units=km +no_defs +type=crs") {
  Tron <- st_transform(regionGeometry, proj)
  predictionData <- st_make_grid(Tron, c(resolution[1], resolution[2]))
  intersectData <- sapply(st_intersects(predictionData, Tron), function(z) if (length(z)==0) FALSE else TRUE)
  predictionData <- predictionData[intersectData] #if need be, convert to points and select every 5th?
  predictionData <- st_as_sf(predictionData)#, data = data.frame())
  PredictionData <- predictionData %>% st_cast('POINT')
  PredictionData <- PredictionData[seq(1,length(PredictionData), 5)]
  st_geometry(PredictionData) <- 'geometry'
  return(PredictionData)
}
