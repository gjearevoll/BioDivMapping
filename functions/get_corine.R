
#' @title \emph{get_worldclim}: This function downloads and merges CORINE land cover data for a study area

#' @description This function unzips and constructs a raster of land cover data. It requires the zip file with CORINE data to already be downloaded from https://land.copernicus.eu/en/products/corine-land-cover/clc2018 and saved as corineRaw in the data/external/environmentalCovariates folder. 
#'
#' @param zip_path folder where downloaded CORINE zip file is stored
#' @param output_path path for where to save downloaded data (with default = NA, data is downloaded into temporary folder that is later deleted).
#'
#' 
#' @return A SpatRaster of WorldClim layers.
library(terra)

get_corine <- function(zip_path, output_path = NA) {
  
  if(!file.exists(zip_path)) {
    stop("CORINE land cover data cannot be found. Data can be downloaded from https://land.copernicus.eu/en/products/corine-land-cover/clc2018.")
  }

  # create temporary working directory to process zip file
  temp_wd <- file.path(tempdir(), "corine")
  unlink(temp_wd, recursive = T)
  dir.create(temp_wd)
  
  # Unzip all folders and download raster
  unzip(zip_path, exdir = temp_wd)
  
  # unzip remaining files recursively
  keep_unzipping <- T
  while(keep_unzipping){
    zip_files <- list.files(temp_wd, "\\.zip$", full.names = T, recursive = T)
    if(length(zip_files) > 0){
      # unzip and delete
      for(file in zip_files){
        unzip(file, exdir = temp_wd, junkpaths = F)
        unlink(file, recursive = TRUE)
      }
    } else {
      keep_unzipping <- F
    }
  }
  
  # by country (tested on norway)
  if("Info" %in% list.dirs(temp_wd, F, F)){
    # load
    corine <- rast(list.files(temp_wd, ".tif$", full.names = T))
    
    # read raster legend information and relabel values
    legend_path <- list.files(file.path(temp_wd, "Info/Legend", "Raster"), ".txt$", full.names = T,recursive = T)
    legend <- read.delim(legend_path, sep =  ",", header = FALSE)
    values(corine) <- legend$V6[values(corine)[,1]]
  } else {
    corine_path <- list.files(file.path(temp_wd, list.dirs(temp_wd, F,F),
                                        "DATA"), "\\.tif$", full.names = T)
    # load raster
    corine <- rast(corine_path)
    
    # remove commas from all category labels 
    levels(corine)[[1]][,2] <- gsub(",", "", levels(corine)[[1]][,2])
  }
  
  if(!is.na(output_path)) {
    writeRaster(corine, file.path(output_path, "corine.tif"))
  }
  
  out <- corine
  return(out)
}


