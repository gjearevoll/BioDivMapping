# Load necessary library
library(httr)

#' Check if a File at a URL Exists
#'
#' This function checks if a file at the specified URL exists by making an HTTP HEAD request.
#' It examines the HTTP status code in the response. If the status code is 200 (HTTP OK),
#' it indicates that the file exists.
#'
#' @param url A character string specifying the URL of the file to check.
#'
#' @return A logical value: TRUE if the file exists (HTTP status 200), FALSE otherwise.
#'
#' @examples
#' urlFileExist("https://www.example.com/file.pdf")
#' urlFileExist("https://www.example.com/nonexistentfile.pdf")
#'
#' @export
urlFileExist <- function(url) {
  # Define constant for HTTP status OK
  HTTP_STATUS_OK <- 200
  
  # Make an HTTP HEAD request to the URL
  hd <- httr::HEAD(url)
  
  # Extract the status code from the response
  status <- hd$all_headers[[1]]$status
  
  # Return TRUE if status code is HTTP OK, FALSE otherwise
  return(status == HTTP_STATUS_OK)
}
