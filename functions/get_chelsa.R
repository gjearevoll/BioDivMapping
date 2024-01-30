
#' @title \emph{get_chelsa}: This function downloads and merges different chelsa data based on the link provided

#' @description Chelsa has a range of climate data - this function downloads them based on the parameter chosen. All dataset links can be found here https://envicloud.wsl.ch/#/?prefix=chelsa%2Fchelsa_V2%2FGLOBAL%2F.
#'
#' @param parameter The chosen parameter. The function has the relevant link to the dataset built in.
#'
#' 
#' @return A SpatRaster of Chelsa layers.


get_chelsa <- function(parameter) {
  if (parameter == "snow_cover") {
    focalURL <- "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_scd_1981-2010_V.2.1.tif"
  }
  message(sprintf("Downloading, %s raster from chelsa.", parameter))
  raster <- terra::rast(focalURL)
  return(raster)
}


