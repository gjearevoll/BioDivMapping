
#' @title \emph{get_ngu}: This function loads locally stored vector data and creates soil type (losmasse) data for a region

#' @description This function loads locally stored vector data of soil types in Norway, and rasterizes it for a given region
#'
#' @return An aggregated raster containing elevation data for Norway.
#'
#' @import terra
#' 



get_ngu <- function(regionGeometry, projCRS) {
  if (!file.exists("data/temp/ngu/Losmasse1000")) {
    cat("Correct file does not exist. If you do not have access to the simplified losmasse N1000 file, contact sam.perrin@ntnu.no.")
    return(NULL)
  }
  losmasseData <- read_sf(dsn = "data/temp/ngu/Losmasse1000/LosmFlate_N1000.shp") %>%
    st_transform(projCRS)
  
  # Get elevation raster
  # get  base raster with expanded buffer (as closest road or lake may be over county lines)
  baseRasterDistance <- regionGeometry |>
    st_buffer(40000) |>
    st_transform(projCRS) |> 
    vect()
  baseWaterRaster <- terra::rast(extent = ext(baseRasterDistance), res = 1000, crs = projCRS)
  baseWaterRaster$cellId <- paste0("cell", 1:ncell(baseWaterRaster))
  
  r <- terra::rasterize(losmasseData, baseWaterRaster, field = "jorda_navn", fun="max")

  return(r)
  
}