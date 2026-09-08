
install_uv <- function() {
  message("uv not found -- installing via https://astral.sh/uv/install.sh ...")
  if (nzchar(Sys.which("curl"))) {
    cmd <- "curl -LsSf https://astral.sh/uv/install.sh | sh"
  } else if (nzchar(Sys.which("wget"))) {
    cmd <- "wget -qO- https://astral.sh/uv/install.sh | sh"
  } else {
    stop("Neither curl nor wget is available to fetch the uv installer. ",
         "Install uv manually: https://docs.astral.sh/uv/getting-started/installation/")
  }
  
  status <- system2("sh", args = c("-c", cmd), stdout = "", stderr = "")
  if (!identical(status, 0L)) {
    stop(sprintf(
      "uv installer failed (exit %s). Install uv manually: https://docs.astral.sh/uv/getting-started/installation/",
      status
    ))
  }
  
  installed <- path.expand("~/.local/bin/uv")
  if (!file.exists(installed)) {
    stop(sprintf(
      "uv installer ran but %s wasn't found afterwards -- check the installer output above.",
      installed
    ))
  }
  message(sprintf("uv installed to %s", installed))
  installed
}


# Now install the python uv 
find_uv <- function() {
  candidates <- c(Sys.which("uv"), path.expand("~/.local/bin/uv"), "/usr/local/bin/uv")
  found <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(found) > 0) found[[1]] else ""
}