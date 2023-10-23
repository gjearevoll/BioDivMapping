
# load necessary libraries
library(rvest)  # web scraping
library(httr)  # making HTTP requests
library(utils)  # various utility functions, including `unzip`
library(dplyr)

# Check out the full list of data repositories here: https://nedlasting.geonorge.no/geonorge/
# See available data sources: https://nedlasting.geonorge.no/geonorge/Basisdata/

get_geonorge <- function(repo = "Basisdata", dataName = "DTM10UTM33", targetDir) {

  # extend the target directory path with the data name
  targetDirExt <- file.path(targetDir, dataName)
  
  # create the target directory if it doesn't exist
  if (!dir.exists(targetDirExt)) {
    dir.create(targetDirExt, recursive = TRUE)
  }
  
  # construct the base URL for the specified data repository and data name
  base_url <- paste0("https://nedlasting.geonorge.no/geonorge/", repo, "/", 
                     dataName, "/")
  
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
  
  # construct the URL containing the list of zip files for the selected format
  url <- paste0(base_url, "TIFF/")
  
  # scrape the data URL to obtain a list of zip files
  zip_links <- url %>%
    read_html() %>%
    html_nodes(xpath = "//a[contains(@href, '.zip')]") %>% # Extract all links that end with '.zip'
    html_attr("href") %>%  # Get the href attribute of these links
    paste0(url, .) # Concatenate the base URL with each zip link to form the full URL
  
  # (quietly) download each zip file, unzip it, and then delete the zip file
  invisible(lapply(zip_links, function(link) {
    # specify the temporary zip file name
    zip_name <- file.path(targetDirExt, "temp.zip")
    
    # download the zip file to the specified location
    download.file(link, destfile = zip_name, mode = "wb")
    
    # unzip the downloaded file to the target directory
    unzip(zip_name, exdir = targetDirExt)
    
    # delete the zip file
    file.remove(zip_name)
  }))
  
  dirNames <- list.files(targetDirExt, full.names = TRUE)
  
  # Aggregate each file to 100m and add to list, then merge
  DTMList <- lapply(dirNames, FUN = function(x) {
    aggregate(rast(x), fact = 10)
  })
  elevation <- do.call(mosaic, DTMList)
  return(elevation)
}


