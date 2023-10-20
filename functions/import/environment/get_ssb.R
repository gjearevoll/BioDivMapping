# function derives values found on this page: https://www.ssb.no/natur-og-miljo/geodata and 
# at https://kartkatalog.geonorge.no/metadata/befolkning-paa-rutenett-1000m-2016-2019-wfs/20d06bca-484c-4353-a24b-61046296f5da

get_ssb <- function(focalParameter, resolution = "1000") {
  
  # Define endpoints for different resolutions and variables
  resolutionFileCheck <- data.frame(human_density = c("http://wfs.geonorge.no/skwms1/wfs.befolkningsstatistikkrutenett250mhistorisk?service=wfs&Version=2.0.0&request=GetCapabilities",
                                                      "https://wfs.geonorge.no/skwms1/wfs.befolkningsstatistikkrutenett1kmhistorisk?service=WFS&Version=2.0.0&request=GetCapabilities"),
                                    building_density = c("https://wfs.geonorge.no/skwms1/wfs.bygningsstatistikkrutenett250m?service=WFS&request=GetCapabilities",
                                                         "https://wfs.geonorge.no/skwms1/wfs.bygningsstatistikkrutenett1000m?service=WFS&Version=2.0.0&request=GetCapabilities"),
                                    row.names = c("250", "1000"))
  focalEndpoint <- resolutionFileCheck[resolution, focalParameter]
  
  V0 <- vect(paste0("WFS:", focalEndpoint))
  
  # Project onto our rasters
  R0 <- rast("data/external/environmentalCovariates/aspect.tiff")
  R2 <- terra::rasterize(project(V0,R0), R0, field = "bui0all")
  R3 <- lapp(R2, function(x) ifelse(is.na(x), 0, x))
  
  return(R3)
  
}
