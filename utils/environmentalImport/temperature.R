# Set extent for download
regionExtent <- ext(regionGeometry_buffer)


if (!exists("dataSource")) {
  dataSource <- "worldClim"
}

print(paste0("Downloading temperature from ", dataSource))

if (dataSource == "worldClim") {
  source("utils/environmentalImport/get_worldclim.R")
  annualStack <- get_worldclim(regionExtent, "tavg", 0.5)
  rasterisedVersion <- mean(annualStack)
} else if (dataSource == "cerra") {
  rasterisedVersion <- "to be added"
}
 