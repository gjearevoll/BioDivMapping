
#' @title \emph{get_met}: This function downloads MET data from an NC file and turs it to a spatial raster.

#' @description Chelsa has a range of climate data - this function downloads them based on the parameter chosen. All dataset links can be found here https://envicloud.wsl.ch/#/?prefix=chelsa%2Fchelsa_V2%2FGLOBAL%2F.
#'
#' @param focalParameter The chosen parameter. The function has the relevant link to the dataset built in.
#' @param projCRS Project crs
#' @param ncPath The folder where the downloaded nc file should be saved.
#'
#' @import terra
#' 
#' @return A SpatRaster of a climate variable.




get_met <- function(focalParameter, projCRS, ncPath = NA) {
  
  if (focalParameter == "summer_temperature") {
    focalURL <- "https://thredds.met.no/thredds/catalog/KSS/Gridded_climate_normals_1991-2020/temperature/catalog.html?dataset=KSS/Gridded_climate_normals_1991-2020/temperature/tm_normal_jja_1991-2020.nc"
    filePath <- paste0(ncPath, "/tm_normal_jja_1991-2020.nc")
  } else if (focalParameter == "summer_precipitation") {
    focalURL <- "https://thredds.met.no/thredds/catalog/KSS/Gridded_climate_normals_1991-2020/precipitation/catalog.html?dataset=KSS/Gridded_climate_normals_1991-2020/precipitation/rr_normal_jja_1991-2020.nc"
    filePath <- paste0(ncPath, "/rr_normal_jja_1991-2020.nc")
  }
  
  if (!(file.exists(filePath))) {
    stop("nc file not found at specified path. Can be found at ",focalURL, ". File name should be ", filePath, ".")
  }
  
  if (is.na(ncPath)) {
    message("'ncPath' not specified, please select the ", focalParameter," nc file. Data can be downloaded from ", focalURL, 
            ". Select the HTTP server data.")
    filePath <- file.choose()
  }
  
  message(sprintf("Uploading %s raster", focalParameter))
  rastNC <- terra::rast(filePath) %>%
    terra::project(projCRS)

  return(rastNC)
}



