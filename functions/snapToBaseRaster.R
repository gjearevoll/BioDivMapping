
#' @title \emph{snapToBaseRaster}: Takes coordinates and snaps them to the centrepoint of the base raster being used

#' @description Obfuscates coordingates using the base raster. 
#'
#' @param points sf object with points for species occurrences.
#' @param baseRaster The base raster for the pipeline run.
#' 
#' @return A download key for a GBIF download.
#'


snapToBaseRaster <- function(points, baseRaster, dropOutside = TRUE) {
  
  # match the raster's CRS (projCRS from defineRegionGeometry)
  points <- st_transform(points, crs(baseRaster))
  
  cells <- cellFromXY(baseRaster, st_coordinates(points))
  outside <- is.na(cells)
  
  if (dropOutside) {
    points <- points[!outside, ]
    cells <- cells[!outside]
  }
  
  centres <- xyFromCell(baseRaster, cells)
  st_geometry(points) <- st_sfc(lapply(seq_len(nrow(centres)),
                                       function(i) st_point(centres[i, ])),
                                crs = st_crs(points))
  points$cellID <- cells
  points
}
