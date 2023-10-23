

### DEFINE REGION ###

# This source file defines the region inside which all our occurrences and envrionmental data will be contained

library(csmaps)

defineRegion <- function(level = "county", region = "50", runBuffer = FALSE, extentCoords = NA) {

# Now assign a region geometry based on the Norwegian political maps found in the package csmaps.
if (level == "municipality") {
  regionCode <- paste0("municip_nor", region)
  regionGeometry <- nor_municip_map_b2020_default_sf$geometry[nor_municip_map_b2020_default_sf$location_code == regionCode]
} else if (level == "county") {
  regionCode <- paste0("county_nor", region)
  regionGeometry <- nor_county_map_b2020_default_sf$geometry[nor_county_map_b2020_default_sf$location_code == regionCode]
} else if (level == "country") {
  world <- giscoR::gisco_get_countries()
  Norway <- world[world$NAME_ENGL == 'Norway',]
  regionGeometry <- st_transform(Norway$geometry,crs = "+proj=longlat +datum=WGS84 +no_defs")
} else {
    ## create a matrix of coordinates that also 'close' the polygon
    res <- matrix(c(extentCoords['north'], extentCoords['west'],
                    extentCoords['north'], extentCoords['east'],
                    extentCoords['south'], extentCoords['east'],
                    extentCoords['south'], extentCoords['west'],
                    extentCoords['north'], extentCoords['west'])  ## need to close the polygon
                  , ncol =2, byrow = T
    )
    ## create polygon objects
    regionGeometry <- st_polygon(list(res))
    rm('res')
}

if (runBuffer == TRUE) {
  regionGeometry <- st_buffer(regionGeometry, dist = 1)
}

# Align project coordinates with the rest of our polygons.
regionGeometry <- st_transform(regionGeometry, crs =  "+proj=longlat +ellps=WGS84")

return(regionGeometry)
}