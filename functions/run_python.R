
#' Run a script inside the biodiv-data uv project.
#'
#' Streams stdout/stderr straight to the console by default -- the tile
#' downloads are long-running and you want to watch progress. Pass
#' capture = TRUE to get the output back as a character vector instead (used
#' internally for short/diagnostic calls).
run_python_script <- function(script, script_args = character(),
                              uv_bin, capture = FALSE) {
  out <- system2(
    uv_bin,
    args = c("run", script, script_args),
    stdout = if (capture) TRUE else "",
    stderr = if (capture) TRUE else ""
  )
  status <- if (capture) {
    s <- attr(out, "status"); if (is.null(s)) 0L else s
  } else {
    out
  }
  if (!identical(status, 0L)) {
    msg <- sprintf("%s failed (exit %s)", script, status)
    if (capture) msg <- sprintf("%s:\n%s", msg, paste(out, collapse = "\n"))
    stop(msg)
  }
  if (capture) out else invisible(NULL)
}

ensure_python_env <- function(uv_bin, wd) {
  message("Syncing Python environment (uv sync)...")
  status <- system2(uv_bin, args = c("sync"), stdout = "", stderr = "")
  if (!identical(status, 0L)) stop(sprintf("uv sync failed (exit %s)", status))
  invisible(NULL)
}