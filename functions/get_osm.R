
#' @title \emph{get_osm}: This function downloads road or water data from OSM and converts it to a useable metric.

#' @description The function enters a SQL query and downloads either road or water polygons/vectors from the Open Street Map service, then rasterizes them and converts their presence to densities.
#'
#' @param focalParameter The chosen parameter. The function has the relevant link to the dataset built in.
#' @param dataPath The folder where the downloaded files should be saved.
#' @param baseRaster The raster to project the final density onto.
#' @param place The character vector defining the area to project onto.
#' 
#'
#' @import terra
#' 
#' @return A SpatRaster of a climate variable.

library(osmextract)

get_osm <- function(baseRaster, focalParameter, dataPath, place) {
  
  
  baseRasterHR <- baseRaster
  res(baseRasterHR) <- res(baseRasterHR) / 5
  
  if (focalParameter == "density_water") {
    if (!dir.exists(file.path(dataPath,"water"))) {dir.create(file.path(dataPath,"water"))}
    
    # Define SQL queries
    sqlQueryPolygons <- "SELECT osm_id, \"natural\", water, landuse, geometry
                FROM multipolygons
                WHERE (\"natural\" = 'water' AND (water IS NULL OR water IN ('lake','reservoir','river')))
                   OR landuse = 'reservoir'"
    sqlQueryStreams <- "SELECT osm_id, waterway, geometry
                FROM lines
                WHERE waterway IN ('river','stream','canal')"
    cat("\nDownloading water bodies")
    waterPolygons <- oe_get(place = "Norway", layer = "multipolygons", extra_tags = "water",
                            force_vectortranslate = TRUE,
                            query = sqlQueryPolygons,
                            download_directory = file.path(dataPath,"water"),
                            quiet = FALSE)
    waterStreams <- oe_get("Norway", layer = "lines", force_vectortranslate = TRUE,
                           query = sqlQueryStreams,
                           download_directory = file.path(dataPath,"water"),
                           quiet = FALSE)
    
    cat("\nConverting water polygons to vector format")
    vectPolygons <- project(vect(waterPolygons), baseRaster)
    vectStream <- project(vect(waterStreams), baseRaster)
    
    cat("\nRasterizing water bodies/streams")
    rasterisedPolygons <- rasterize(vectPolygons, baseRasterHR, background = 0)
    rasterisedStreams <- rasterize(vectStream, baseRasterHR, background = 0)
    fullGrid <- rasterisedPolygons + rasterisedStreams
    
    
  } else if (focalParameter  == "density_roads") {
    
    
    roadClasses <- c("primary", "secondary",
                     "tertiary", "unclassified", "residential",
                     "service", "track", "living_street",
                     "pedestrian", "busway","footway","path")
    
    highwayList <- paste(sprintf("'%s'", roadClasses), collapse = ", ")
    sqlQuery <- sprintf("SELECT osm_id, highway, geometry FROM 'lines' WHERE highway IN (%s)", highwayList)
    
    message(sprintf("Downloading/reading OSM extract for '%s' (roads filtered server-side via SQL push-down)...", "Norway"))
    roadsSf <- oe_get(
      place = "Norway",
      layer = "lines",
      query = sqlQuery,
      download_directory = dataPath,
      quiet = FALSE
    )
    cat("\nConverting roads to vector format")
    returnVector <- project(vect(roadsSf), baseRaster)
    cat("\nRasterizing roads vector")
    fullGrid <- rasterize(returnVector, baseRasterHR, background = 0)
  }
  
  cat("\nConverting to density measurements")
  rasterisedVersion <- potential_GPU(fullGrid,
                                     alphas = dist_to_alpha(dist = 1000,
                                                            thresh = 0.05,
                                                            shape = "gaus"),
                                     shape = "gaus", device = "cpu")
  rasterisedVersion <- terra::project(rasterisedVersion, baseRaster, method = "mean")
  return(rasterisedVersion)
  
}


