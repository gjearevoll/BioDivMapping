#' Reclassify and aggregate categories in a categorical SpatRaster
#'
#' @param catRast           A categorical SpatRaster
#' @param oldCats     vector of original factor names
#' @param newCats     vector of new factor names
#'
#' @return The input SpatRaster with relabelled (and optionally aggregated)
#'   categories. Categories to be ignored get a label of "". Their underlying cell values are
#'   left untouched, so they still count as absence (0) when other categories
#'   are aggregated.
reclassRasterCats <- function(catRast, oldCats, newCats) {
  if (!is.factor(catRast)) stop("Input raster must be categorical (factor).")

  reclass_table <- levels(catRast)[[1]]
  names(reclass_table) <- c("value", "label")

  # Map old labels to new labels
  reclass_table$new_label <- newCats[
    match(reclass_table$label, oldCats)
  ]

  # Blank new label = intentional "ignore" class -> set to NA (no warning)
  reclass_table$new_label <- trimws(reclass_table$new_label)
  reclass_table$new_label[reclass_table$new_label == ""] <- NA

  # Warn only for labels genuinely absent from the reclassification table
  unmatched <- !reclass_table$label %in% oldCats
  n_unmatched <- sum(unmatched)
  if (n_unmatched > 0) {
    warning(sprintf(
      "%d categor%s not found in reclassification table and will be set to NA: %s",
      n_unmatched,
      if (n_unmatched == 1) "y" else "ies",
      paste(reclass_table$label[unmatched], collapse = ", ")
    ))
  }
  
  # Apply new labels across all layers
  new_levels <- reclass_table[, c("value", "new_label")]
  names(new_levels)[2] <- "label"
  for (lyr in seq_len(nlyr(catRast))) {
    levels(catRast)[[lyr]] <- new_levels
  }
  
  catRast
}