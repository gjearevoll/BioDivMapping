

### DEFINE REGION ###

# This source file defines the region inside which all our occurrences and envrionmental data will be contained

library(csmaps)

# Figure out if region and level are supplied. If they are not, check whether a speciesDataList exists in the
# environment, and use the attributes to automatically find these parameters.
if (!exists("region") | !exists("level")) {
  if (!exists("speciesDataList")) {
    stop("You have not defined a region and/or level")
  } else {
    warning("You have not provided a region and/or level. These are being pulled from the speciesDataList currently loaded.")
    missingValues <- c("region", "level")[c("region", "level") %in% attributes(speciesDataList$species)]
    if (length(missingValues) > 0) {
      stop(paste0("You need to define the following: ", paste(missingValues, sep = ", ")))
    } else {
      region <- attr(speciesDataList$species, "region")
      level <- attr(speciesDataList$species, "level")
    }
  }
}

# Now assign a region geometry based on the Norwegian political maps found in the package csmaps.
if (level == "municipality") {
  regionCode <- paste0("municip_nor", region)
  regionGeometry <- nor_municip_map_b2020_default_sf$geometry[nor_municip_map_b2020_default_sf$location_code == regionCode]
} else if (level == "county") {
  regionCode <- paste0("county_nor", region)
  regionGeometry <- nor_county_map_b2020_default_sf$geometry[nor_county_map_b2020_default_sf$location_code == regionCode]
} else if (level == "country") {
  regionGeometry <- st_combine(nor_county_map_b2020_default_sf$geometry)
} else {
    ## create a matrix of coordinates that also 'close' the polygon
    res <- matrix(c(points['north'], points['west'],
                    points['north'], points['east'],
                    points['south'], points['east'],
                    points['south'], points['west'],
                    points['north'], points['west'])  ## need to close the polygon
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
