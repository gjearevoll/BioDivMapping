# These two functions are how the R pipeline hands work off to the Python
# scripts (canopy height, soil, BIOPAR data downloads) and gets the results
# back. R doesn't run Python code directly -- instead it starts Python as a
# separate program ("subprocess") via uv (see find_uv.R), waits for it to
# finish, and checks whether it succeeded.

#' Run a script inside the biodiv-data uv project.
#'
#' Streams stdout/stderr straight to the console by default -- the tile
#' downloads are long-running and you want to watch progress. Pass
#' capture = TRUE to get the output back as a character vector instead (used
#' internally for short/diagnostic calls).
run_python_script <- function(script, script_args = character(),
                              uv_bin, capture = FALSE) {
  # Turn the script's path into a full, unambiguous path (e.g.
  # "/home/you/project/functions/foo.py" instead of just "foo.py"). This
  # matters because uv uses the script's location to figure out which
  # Python project it belongs to, and R's "current folder" at the moment
  # this runs might not be where you'd expect.
  #
  # Deliberately NOT using `uv --directory`, which also relocates the
  # *subprocess's* working folder -- that would break the relative
  # --boundary/--out-dir paths callers pass, which are relative to R's
  # actual working directory, not to this script's folder.
  out <- system2(
    uv_bin,
    args = c("run", normalizePath(script, mustWork = FALSE), script_args),
    stdout = if (capture) TRUE else "",
    stderr = if (capture) TRUE else ""
  )

  # system2() reports success/failure differently depending on whether we
  # asked it to capture the output or just print it live, so work out the
  # actual exit status either way.
  status <- if (capture) {
    s <- attr(out, "status"); if (is.null(s)) 0L else s
  } else {
    out
  }

  # An exit status of 0 means "it worked"; anything else means the Python
  # script hit an error, so we stop R here too rather than carrying on as
  # if nothing went wrong.
  if (!identical(status, 0L)) {
    msg <- sprintf("%s failed (exit %s)", script, status)
    if (capture) msg <- sprintf("%s:\n%s", msg, paste(out, collapse = "\n"))
    stop(msg)
  }
  if (capture) out else invisible(NULL)
}

# Makes sure the Python side has everything it needs installed before we try
# to run any of the Python scripts -- like checking all the ingredients are
# in the kitchen before starting to cook. "uv sync" reads the project's list
# of required Python packages and installs whichever ones are missing.
ensure_python_env <- function(uv_bin, wd) {
  message("Syncing Python environment (uv sync)...")
  status <- system2(uv_bin, args = c("sync"), stdout = "", stderr = "")
  if (!identical(status, 0L)) stop(sprintf("uv sync failed (exit %s)", status))
  invisible(NULL)
}
