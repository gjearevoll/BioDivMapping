# function derives values found on this page: https://www.ssb.no/natur-og-miljo/geodata and 
# at https://kartkatalog.geonorge.no/metadata/befolkning-paa-rutenett-1000m-2016-2019-wfs/20d06bca-484c-4353-a24b-61046296f5da

get_ssb <- function(focalParameter, rutenett = NA, getRutenett = TRUE, resolution = "1000") {
  
  # Define endpoints for different resolutions and variables
  resolutionFileCheck <- data.frame(human_density = c("https://www.ssb.no/natur-og-miljo/_attachment/389479?_ts=16b45e39840",
                                                      "https://www.ssb.no/natur-og-miljo/_attachment/389480?_ts=16b45e3cef0"),
                                    building_density = c("https://www.ssb.no/natur-og-miljo/_attachment/461715?_ts=17edde4d2e0",
                                                         "https://www.ssb.no/natur-og-miljo/_attachment/461716?_ts=17edde501c0"),
                                    row.names = c("250", "1000"))
  focalEndpoint <- resolutionFileCheck[resolution, focalParameter]
  
  # Check if rutenett is provided, if not, download from SSB and save locally
  if (getRutenett == TRUE) { 
    download.file("https://www.ssb.no/natur-og-miljo/_attachment/375076?_ts=1685c0b8770", 
                  destfile = "data/temp/ssb/rutenett.zip", mode = "wb")
    fileNames <- unzip("data/temp/ssb/rutenett.zip", exdir = "data/temp/ssb", list = TRUE)
    shpFile <- fileNames$Name[grepl(".shp", fileNames$Name) & !grepl(".shp.xml", fileNames$Name)]
    unzip("data/temp/ssb/rutenett.zip", exdir = "data/temp/ssb")
    rutenett <- read_sf(paste0("data/temp/ssb/",shpFile))[,c("geometry", "SSBid")]
    saveRDS(rutenett, "data/temp/ssb/rutenett.RDS")
  }
  
  # Download csv file for relevant variable  with values
  download.file(focalEndpoint, destfile = paste0(tempFolderName, focalParameter, ".zip"), mode= "wb")
  fileName <- unzip(paste0(tempFolderName, focalParameter, ".zip"), list = TRUE)
  unzip(paste0(tempFolderName, focalParameter, ".zip"), exdir = paste0(tempFolderName, "ssb"))
  values <- read.csv(paste0(tempFolderName, "ssb/", fileName$Name), sep = ";") %>%
    dplyr::select(1,2)
  colnames(values) <- c("SSBId", "value")
  
  # Match to rutenett
  rutenett$value <- values$value[match(rutenett$SSBid, values$SSBId)]
  rutenett$value[is.na(rutenett$value)] <- 0
  
  rasterSample <- raster("data/external/environmentalCovariates/aspect.tiff")
  rasterizedVersion <- rast(fasterize(sf = rutenett, raster = rasterSample, field = "value"))
  return(rasterizedVersion)
  
}