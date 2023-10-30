
#' @title \emph{defineRegion}: function to create relevant geometry object for surveyed region

#' @description This function extracts creates an sf object based on a few parameters in order to define the region we want to create species models for.
#'
#' @param level The spatial level we want to use. Can be either "municipality", "county", "country" or "box".
#' @param region The code/name for the region that you would like to download. For municipality or county, this should be either a 2 or 4 digit number. For a country, it should be the country name.
#' @param runBuffer Whether or not to add a small buffer to the sf object
#' @param extentCoords If you choose to use a box, you need to add in coordinates for the vertices of the box. These should be clockwise, starting from from north-west.
#' 
#' @import csmaps
#' 
#' @return An sf object
#'

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
  country <- world[world$NAME_ENGL == region,]
  regionGeometry <- st_transform(country$geometry,crs = "+proj=longlat +datum=WGS84 +no_defs")
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