
#' @title \emph{get_worldclim}: This function downloads and merges CORINE land cover data for a study area

#' @description This function unzips and constructs a raster of land cover data. It requires the zip file with CORINE data to already be downloaded from https://land.copernicus.eu/en/products/corine-land-cover/clc2018 and saved as corineRaw in the data/external/environmentalCovariates folder. 
#'
#' @param zip_path folder where downloaded CORINE zip file is stored
#' @param output_path path for where to save downloaded data (with default = NA, data is downloaded into temporary folder that is later deleted).
#' 
#' 
#' @return A SpatRaster of WorldClim layers.
library(terra)

get_corine <- function(zip_path = NA, output_path = NA, reclassify = TRUE, temporal = FALSE, yearInterval = NA) {
  # define unzipping function (outputs location of unzipped file)
  unzip_all <- function(zip_path) {
    # create temporary working directory to process zip file
    temp_wd <- tempfile("corine_")
    dir.create(temp_wd, recursive = TRUE, showWarnings = FALSE)
    
    # Unzip all folders and download raster
    unzip(zip_path, exdir = temp_wd)
    
    # unzip remaining files recursively
    repeat {
      zips <- list.files(temp_wd, "\\.zip$", full.names = TRUE, recursive = TRUE)
      if (length(zips) == 0) break
      for (zf in zips) {
        unzip(zf, exdir = temp_wd, junkpaths = FALSE)
        unlink(zf)
      }
    }
    temp_wd
  }
  
  if (temporal) {
    if (!all(!is.na(yearInterval))) stop("Please specify 'yearInterval' when 'temporal = TRUE'")
    if (is.na(output_path)) {
      message("Select location of Corine land cover data.")
      output_path <- rstudioapi::selectDirectory(path = here::here())
    }
    # load for each year
    corine <- list()
    for (i in seq_along(yearInterval)) {
      corine_yr_path <- file.path(output_path, yearInterval[i])
      # find existing file paths 
      tif_paths <- list.files(corine_yr_path, "\\.tif$", full.names = TRUE)
      zip_paths <- list.files(corine_yr_path, "\\.zip$", full.names = TRUE)
      
      # load (with preference for tifs)
      if (length(tif_paths) == 1) {  # tif
        r <- rast(tif_paths)[[1]]
      } else if (length(zip_paths) == 1) {  # zip
        # recursive unzip
        temp_wd <- unzip_all(zip_paths)
        # load raster
        tifs <- list.files(file.path(temp_wd, list.dirs(temp_wd, FALSE, FALSE), "DATA"),
                           "\\.tif$", full.names = TRUE)
        r <- rast(tifs)[[1]]
      }
      # remove commas from all category labels 
      levels(r)[[1]][,2] <- gsub(",", "", levels(r)[[1]][,2])
      # combine with prior years
      corine[[i]] <- r
    }
    # combine layers 
    corine <- rast(corine) %>% setNames(yearInterval)
  } else {
    
    if(is.na(zip_path)){
      message("'zip_path' not specified, please select CORINE land cover zip file. Data can be downloaded from 'https://land.copernicus.eu/en/products/corine-land-cover'.")
      message("Enter full path to CORINE land cover zip file:\n")
      zip_path <- readline()
      zip_path <- gsub('"', '', zip_path)
    }
    
    if (!file.exists(zip_path)) {
      message("'zip_path' does not exist. Please provide CORINE land cover zip file path.")
      message("Enter full path to CORINE land cover zip file:\n")
      zip_path <- readline()
      zip_path <- gsub('"', '', zip_path)
    }

    # unzip 
    temp_wd <- unzip_all(zip_path)
    
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
  }
  
  # If we want, we can now reclassifiy CORINE's layers. We do this using a csv file name "corineReclassification" that should be
  # uploaded to the same data/temp/corine folder you are storing corine rasters. The file should have two columns, one names 
  # corineCategory (with existing corine categories) and the second (with your reclassifications) called newCategory.
  if(!file.exists("data/temp/corine/corineReclassification.csv")) {
    warning("No CORINE reclassification system has been provided. Using all 52 potential categories.")
  } else if (reclassify == TRUE) {
    corineReclassification <- read.csv("data/temp/corine/corineReclassification.csv", header = TRUE)
    reclassTable <- levels(corine)[[1]]
    names(reclassTable) <- c("value", "label")
    reclassTable$newLabel <- corineReclassification$newCategory[match(reclassTable$label, corineReclassification$corineCategory)]
    reclassTable <- reclassTable[,c("value", "newLabel")] %>% rename(label = newLabel)
    for (co in 1:nlyr(corine)) {
      levels(corine)[[co]][,2] <- reclassTable[,2]
    }
    if (temporal) {names(corine) <- yearInterval}
  }
  
  # 
  # if(!is.na(output_path)) {
  #   writeRaster(corine, file.path(output_path, "corine.tif"))
  # }
  
  return(corine)
}


