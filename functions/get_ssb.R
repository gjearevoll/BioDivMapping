
#' @title \emph{get_ssb}: This function downloads human and building density data from SSB

#' @description This function downloads human and building density directly from the Statistisk Sentralbyraa.
#'
#' @param focalParameter The parameter to download - either "human_density" or "building_density".
#' @param resolution The data resolution in metres, either "250" or "1000".
#' 
#' @return An aggregated raster containing anthropogenic data for Norway.
#'

get_ssb <- function(focalParameter, resolution = "1000") {
  
  # Define endpoints for different resolutions and variables
  resolutionFileCheck <- data.frame(human_density = c("http://wfs.geonorge.no/skwms1/wfs.befolkningsstatistikkrutenett250mhistorisk?service=wfs&Version=2.0.0&request=GetCapabilities",
                                                      "https://wfs.geonorge.no/skwms1/wfs.befolkningsstatistikkrutenett1kmhistorisk?service=WFS&Version=2.0.0&request=GetCapabilities"),
                                    building_density = c("https://wfs.geonorge.no/skwms1/wfs.bygningsstatistikkrutenett250m?service=WFS&request=GetCapabilities",
                                                         "https://wfs.geonorge.no/skwms1/wfs.bygningsstatistikkrutenett1000m?service=WFS&Version=2.0.0&request=GetCapabilities"),
                                    row.names = c("250", "1000"))
  focalEndpoint <- resolutionFileCheck[resolution, focalParameter]
  
  message(paste0("Currently downloading ", gsub("_", " ", focalParameter), " at resolution of ", resolution, " from SSB"))
  
  V0 <- vect(paste0("WFS:", focalEndpoint))
  
  # Project onto our rasters
  R0 <- rast("data/external/environmentalCovariates/aspect.tiff")
  parameterField <- ifelse(focalParameter == "human_density", "popTot", "bui0all")
  R2 <- terra::rasterize(project(V0,R0), R0, field = parameterField)
  R3 <- lapp(R2, function(x) ifelse(is.na(x), 0, x))
  
  return(R3)
  
}
