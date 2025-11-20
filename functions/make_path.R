# Create missing directories for a vector of paths
make_path <- function(...){
  paths <- file.path(...)
  cache <- sapply(paths, function(path) {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
    }
  })
  return(paths)
}