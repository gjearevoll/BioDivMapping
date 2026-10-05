## Download Copernicus GLO-30 DEM tiles covering a regionGeometry from AWS Open
## Data (no account or credentials needed), then aggregate to a 100 m grid in
## the region's CRS, masked to the region.


get_copernicus <- function(regionGeometry, dataPath, res = 100) {
  
  # Switch to planar geometry and ensure it switches back to default when function ends
  oldS2 <- sf_use_s2(FALSE)
  on.exit(sf_use_s2(oldS2), add = TRUE)
  
  # Define url and locations
  bucketURL <- "https://copernicus-dem-30m.s3.amazonaws.com"
  tileFolder <- file.path(dataPath, "tiles")
  if (!dir.exists(tileFolder)) {dir.create(tileFolder, showWarnings = FALSE, recursive = TRUE)}
  options(timeout = max(600, getOption("timeout")))
  
  ## 1x1 degree tile grid over the region, keeping only tiles that touch the polygon
  regionLL <- st_transform(regionGeometry, 4326)
  bb <- st_bbox(regionLL)
  tileGrid <- st_make_grid(regionLL, cellsize = 1,
                           offset = c(floor(bb[["xmin"]]), floor(bb[["ymin"]])))
  tileGrid <- tileGrid[lengths(st_intersects(tileGrid, regionLL)) > 0]
  
  ## Tiles are named by their south-west corner
  corners <- t(sapply(tileGrid, function(g) round(st_bbox(g)[c("xmin", "ymin")])))
  lonTag <- sprintf("%s%03d_00", ifelse(corners[, "xmin"] >= 0, "E", "W"), abs(corners[, "xmin"]))
  latTag <- sprintf("%s%02d_00", ifelse(corners[, "ymin"] >= 0, "N", "S"), abs(corners[, "ymin"]))
  tileNames <- paste0("Copernicus_DSM_COG_10_", latTag, "_", lonTag, "_DEM")
  
  ## Ocean-only tiles don't exist in the bucket, so check against its tile list
  tileList <- readLines(file.path(bucketURL, "tileList.txt"))
  tileNames <- tileNames[tileNames %in% tileList]
  message(length(tileNames), " DEM tiles intersect the region")
  
  ## Download tiles, skipping any already on disk (via a temp file, so an
  ## interrupted download doesn't leave a partial tile behind)
  tilePaths <- file.path(tileFolder, paste0(tileNames, ".tif"))
  for (i in seq_along(tileNames)) {
    if (file.exists(tilePaths[i])) next
    message("Downloading ", i, "/", length(tileNames), ": ", tileNames[i])
    tmpPath <- paste0(tilePaths[i], ".part")
    download.file(file.path(bucketURL, tileNames[i], paste0(tileNames[i], ".tif")),
                  tmpPath, mode = "wb", quiet = TRUE)
    file.rename(tmpPath, tilePaths[i])
  }
  
  ## Mosaic as a VRT. Above 50N the tiles' longitude spacing varies with
  ## latitude, so keep the finest resolution rather than GDAL's default average
  dem <- vrt(tilePaths, filename = file.path(tileFolder, "copernicusDEM.vrt"),
             options = c("-resolution", "highest"), overwrite = TRUE)
  
  ## Target grid: region extent snapped outward to multiples of res
  regionExt <- as.vector(ext(vect(regionGeometry)))
  template <- rast(ext(floor(regionExt[1] / res) * res, ceiling(regionExt[2] / res) * res,
                       floor(regionExt[3] / res) * res, ceiling(regionExt[4] / res) * res),
                   resolution = res, crs = st_crs(regionGeometry)$wkt)
  
  ## Reproject and aggregate in one step (mean of all 30 m cells in each 100 m cell)
  demAgg <- project(dem, template, method = "average", threads = TRUE)
  demAgg <- mask(demAgg, vect(regionGeometry))
  names(demAgg) <- "elevation"
  return(demAgg)
}

