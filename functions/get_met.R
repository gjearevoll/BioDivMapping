
#' @title \emph{get_met}: This function downloads MET data from an NC file and turs it to a spatial raster.

#' @description Chelsa has a range of climate data - this function downloads them based on the parameter chosen. All dataset links can be found here https://envicloud.wsl.ch/#/?prefix=chelsa%2Fchelsa_V2%2FGLOBAL%2F.
#'
#' @param parameter The chosen parameter. The function has the relevant link to the dataset built in.
#' @param dataPath The folder where the downloaded nc file should be saved.
#'
#' @import terra
#' 
#' @return A SpatRaster of a climate variable.

get_met <- function(parameter, dataPath) {
  if (parameter == "summer_temperature") {
    focalURL <- "https://thredds.met.no/thredds/fileServer/KSS/Gridded_climate_normals_1991-2020/temperature/tm_normal_jja_1991-2020.nc"
  } else if (parameter == "summer_precipitation") {
    focalURL <- "https://thredds.met.no/thredds/fileServer/KSS/Gridded_climate_normals_1991-2020/precipitation/rr_normal_jja_1991-2020.nc"
  }
  if(!urlFileExist(focalURL)){
    stop("Could not download ",parameter ," parameter. No valid file found at URL: ",focalURL, ". Servers may be inaccessible or path may have changed. Please download files manually.")
  }
  message(sprintf("Downloading, %s raster from met", parameter))
  file_path <- paste0(dataPath, "/", parameter, ".nc")
  download.file(focalURL, destfile = file_path, mode = "wb")
  
  rastNC <- terra::rast(file_path)
  
  return(rastNC)
}


