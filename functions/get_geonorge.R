
#' @title \emph{get_geonorge}: This function downloads geographical data directly from Geonorge

#' @description This function downloads geographical data direct from geonorge to be used in calculating slope, elevation and aspect across Norway
#'
#' @param repo The geonorge repo containing our data.
#' @param dataName Name of the data we want to download.
#' @param targetDir The directory we want to save the new data in.
#' 
#' @return An aggregated raster containing elevation data for Norway.
#'
#' @import rvest
#' @import httr
#' @import utils
#' @import dplyr 
#' 

# Check out the full list of data repositories here: https://nedlasting.geonorge.no/geonorge/
# See available data sources: https://nedlasting.geonorge.no/geonorge/Basisdata/

get_geonorge <- function(repo = "Basisdata", dataName = "DTM10UTM33", targetDir, dataFormat) {

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
    rvest::read_html() %>%
    rvest::html_nodes(xpath = "//a[contains(@href, '/')]") %>%  # extract all links that contain a slash (directories)
    rvest::html_attr("href") %>%  # get the href attribute of the links
    gsub(pattern = ".*/([^/]+)/$", replacement = "\\1", x = .) %>%  # extract the format name using regex
    .[!grepl(pattern = "^[\\.]{1,2}$", x = .)] %>%  # remove any entries that are just dots (like "..")
    gsub(pattern = "/$", replacement = "", x = .) %>% # remove trailing slashes
    .[. != ".."]  # exclude '..' 
  
  # construct the URL containing the list of zip files for the selected format
  url <- paste0(base_url, paste0(dataFormat, "/"))
  
  # scrape the data URL to obtain a list of zip files
  zip_links <- url %>%
    rvest::read_html() %>%
    rvest::html_nodes(xpath = "//a[contains(@href, '.zip')]") %>% # Extract all links that end with '.zip'
    rvest::html_attr("href") %>%  # Get the href attribute of these links
    paste0(url, .) # Concatenate the base URL with each zip link to form the full URL
  
  # If using RGDB data, just download the Norwegian data
  if (dataFormat == "FGDB") {
    zip_links <- zip_links[grepl("0000_Norge", zip_links, fixed = TRUE)]
  }
  
  # Else (quietly) download each zip file, unzip it, and then delete the zip file
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
  
  if (dataFormat == "TIFF") {
    # If elevation, aggregate to Aggregate each file to 100m and add to list, then merge
    DTMList <- lapply(dirNames, FUN = function(x) {
      aggregate(rast(x), fact = 10)
    })
    elevation <- do.call(mosaic, DTMList)
    return(elevation)
    
  } else if (dataFormat == "FGDB") {
    # get links with gdb ending
    # List all files in the directory that match the parameter
    filePattern <- "_.*\\.gdb$"
    fileList <- dir(path = targetDirExt, pattern = filePattern, full.names = TRUE)
    correctLayer <- ifelse(focalParameter == "distance_water", "N250_Arealdekke_omrade", "N250_Samferdsel_senterlinje")
    vectorData <- vect(fileList, layer = correctLayer)
    
    # Narrow down to correct category
    if (focalParameter == "distance_water") {
      vectorData <- terra::subset(vectorData, vectorData$objtype %in% c("Innsjø", "Elv", "InnsjøRegulert"))
    } else {
      vectorData <- terra::subset(vectorData, vectorData$typeveg %in% "enkelBilveg")
    }
    
    return(vectorData)
  }
}


