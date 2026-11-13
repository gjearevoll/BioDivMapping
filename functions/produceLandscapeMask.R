
produceLandscapeMask <- function(corinePath, maskedCats, baseRaster, crs, res) {
  corineUnclassified <- checkAndImportRast("land_cover_corine", baseRaster, "data/temp/corine", quiet = TRUE, FALSE, yearInterval)
  urbanWater <- ifel(corineUnclassified %in% maskedCats, 1, 0)
  
  baseRaster <-rast(ext = ext(vect(regionGeometry))+10, res = res)
  crs(baseRaster) <- crs(vect(regionGeometry))
  rastToMap <- terra::project(baseRaster, paste0("EPSG:", crs))
  #rastToMap <- rast(crs = "EPSG:32633", resolution = 0.5, extent = ext(baseRaster))
  
  out <- terra::project(urbanWater, rastToMap, method = "average")
  mask100 <- ifel(out == 1, 0, 1)
}
