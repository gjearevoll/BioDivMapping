
reset_environments <- function(obj) {
  if (is.null(obj)) {
    # Return NULL immediately if the object itself is NULL
    return(NULL)
  } else if (is.environment(obj)) {
    # Clear all objects within this environment to free memory
    rm(list = ls(envir = obj), envir = obj)
    # Set the parent environment to a base environment
    parent.env(obj) <- baseenv()
  } else if (is.list(obj)) {
    # Iterate over list elements, skipping NULLs safely
    for (i in seq_along(obj)) {
      if (!is.null(obj[[i]])) {  # Check for NULL elements before proceeding
        obj[[i]] <- tryCatch({
          reset_environments(obj[[i]])
        }, error = function(e) obj[[i]])  # Skip elements that cause "subscript out of bounds" error
      }
    }
  } else if (is.function(obj)) {
    # Reset environment of function objects
    environment(obj) <- baseenv()
  }
  # Return the object with attributes and class retained
  return(obj)
}

