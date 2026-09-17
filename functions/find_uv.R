# This file deals with "uv" -- a small program that installs and manages the
# Python environment for the pipeline's Python scripts (e.g. the canopy
# height / soil / BIOPAR data downloaders). Think of uv like a helper that
# makes sure the right version of Python and all the Python packages those
# scripts need are ready to go, without you having to set any of that up by
# hand. These two functions find uv if it's already installed, and install
# it if it isn't.

# Downloads and installs uv onto this computer.
# Only called when find_uv() (below) couldn't locate an existing copy.
install_uv <- function() {
  message("uv not found -- installing via https://astral.sh/uv/install.sh ...")

  # uv's own installer is a small script that Astral (the people who make uv)
  # publish online. We fetch it with whichever download tool is available
  # (curl or wget -- most computers have at least one of these) and run it.
  if (nzchar(Sys.which("curl"))) {
    cmd <- "curl -LsSf https://astral.sh/uv/install.sh | sh"
  } else if (nzchar(Sys.which("wget"))) {
    cmd <- "wget -qO- https://astral.sh/uv/install.sh | sh"
  } else {
    # Neither tool is available, so we can't download anything automatically.
    stop("Neither curl nor wget is available to fetch the uv installer. ",
         "Install uv manually: https://docs.astral.sh/uv/getting-started/installation/")
  }

  # Actually run the download-and-install command.
  status <- system2("sh", args = c("-c", cmd), stdout = "", stderr = "")
  if (!identical(status, 0L)) {
    # A non-zero status means the command failed for some reason.
    stop(sprintf(
      "uv installer failed (exit %s). Install uv manually: https://docs.astral.sh/uv/getting-started/installation/",
      status
    ))
  }

  # uv's installer always puts the program in this specific folder, so we
  # check it actually landed there before declaring success.
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


# Looks for an existing copy of uv on this computer, without installing
# anything. Checks the most likely places one might be, in order, and
# returns the path to the first one it finds. Returns an empty string ("")
# if none of those places have it -- it's then up to whoever called this
# function to decide whether to install it (see install_uv() above).
find_uv <- function() {
  candidates <- c(Sys.which("uv"), path.expand("~/.local/bin/uv"), "/usr/local/bin/uv")
  found <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(found) > 0) found[[1]] else ""
}
