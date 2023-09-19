
### GeoNorge Import Script ###

# This script imports elevation data from GBIF and converts it to a usable tiff file

# First check to see if geonorge elevation data has already been imported. If not, import.

if (file.exists("data/temp/geonorge/elevationRaster.tiff")) { 
  elevation <- rast("data/temp/geonorge/elevationRaster.tiff")
} else {
  
  # load necessary libraries
  library(rvest)  # web scraping
  library(httr)  # making HTTP requests
  library(utils)  # various utility functions, including `unzip`
  library(dplyr)
  
  # define the data repository 
  # see others: https://nedlasting.geonorge.no/geonorge/
  repo <- "Basisdata"
  # specify the data source of interest
  # see available: https://nedlasting.geonorge.no/geonorge/Basisdata/
  data_name <- "DTM10UTM33"
  # specify the target directory for storing raw data
  target_dir <- paste0("data/temp/geonorge/")
  
  # extend the target directory path with the data name
  target_dir <- paste0(target_dir, data_name, "/") 
  
  # create the target directory if it doesn't exist
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }
  
  # construct the base URL for the specified data repository and data name
  base_url <- paste0("https://nedlasting.geonorge.no/geonorge/", repo, "/", 
                     data_name, "/")
  
  # scrape the base URL to obtain a list of available data formats (directories)
  formats <- base_url %>%
    read_html() %>%
    html_nodes(xpath = "//a[contains(@href, '/')]") %>%  # extract all links that contain a slash (directories)
    html_attr("href") %>%  # get the href attribute of the links
    gsub(pattern = ".*/([^/]+)/$", replacement = "\\1", x = .) %>%  # extract the format name using regex
    .[!grepl(pattern = "^[\\.]{1,2}$", x = .)] %>%  # remove any entries that are just dots (like "..")
    gsub(pattern = "/$", replacement = "", x = .) %>% # remove trailing slashes
    .[. != ".."]  # exclude '..' 
  
  # print the list of available data formats
  print(formats)
  
  # specify the desired data format
  format <- "TIFF"
  
  # construct the URL containing the list of zip files for the selected format
  url <- paste0(base_url, format, "/")
  
  # scrape the data URL to obtain a list of zip files
  zip_links <- url %>%
    read_html() %>%
    html_nodes(xpath = "//a[contains(@href, '.zip')]") %>% # Extract all links that end with '.zip'
    html_attr("href") %>%  # Get the href attribute of these links
    paste0(url, .) # Concatenate the base URL with each zip link to form the full URL
  
  # (quietly) download each zip file, unzip it, and then delete the zip file
  invisible(lapply(zip_links, function(link) {
    # specify the temporary zip file name
    zip_name <- paste0(target_dir, "temp.zip")
    
    # download the zip file to the specified location
    download.file(link, destfile = zip_name, mode = "wb")
    
    # unzip the downloaded file to the target directory
    unzip(zip_name, exdir = target_dir)
    
    # delete the zip file
    file.remove(zip_name)
  }))
  
  dirNames <- list.files("data/temp/geonorge/DTM10UTM33", full.names = TRUE)
  
  # Aggregate each file to 100m and add to list, then merge
  DTMList <- lapply(dirNames, FUN = function(x) {
    aggregate(rast(x), fact = 10)
  })
  elevation <- do.call(mosaic, DTMList)
  writeRaster(elevation, "data/temp/geonorge/elevationRaster.tiff", overwrite=TRUE)
}

# Now get the raster you're actually looking for
if (focalParameter == 'elevation') {
  rasterisedVersion <- elevation
} else {
  rasterisedVersion <- terra::terrain(elevation, v=focalParameter, unit='degrees', neighbors=8)
}

