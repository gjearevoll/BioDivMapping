
#' @title \emph{meshTest}: Visualise an INLA mesh before starting a model run

#' @description This function allows you to visualise an INLA mesh to check its parameters before using it in a model run.
#'
#' @param meshList A vector of GBIF taxon keys.
#' @param regionGeometry An sf object encompassing our region of study, as produced by defineRegion.
#' @param print Whether or not you want to plot the mesh automatically
#' @param crs coordinate reference system 
#' 
#' @return A ggplot of an INLA mesh
#'

meshTest <- function(meshList, regionGeometry, print = TRUE, crs = NULL) {
  # define crs 
  if(is.null(crs)){
    # get crs from regionGeometry
    crs <- st_crs(regionGeometry)
    # convert lat/long projection to UTM
    if(st_crs(crs)$units_gdal == "degree"){
      crs <- utmCRS(regionGeometry)
    }
  } else {
    crs <- sf::st_crs(crs)$wkt  # standardise CRS into well-known text format
  }
  
  mesh <- INLA::inla.mesh.2d(boundary = inlabru::fm_as_inla_mesh_segment(sf::st_transform(regionGeometry, crs)),
                             crs = inlabru::fm_crs(crs),
                             cutoff= meshList$cutoff, 
                             max.edge=meshList$max.edge, 
                             offset= meshList$offset)
  meshPlot <- ggplot2::ggplot()+
    ggplot2::geom_sf(data = sf::st_transform(regionGeometry, crs), fill = "white") +
    inlabru::gg(mesh)
  if (print == TRUE) {print(meshPlot)}
  return(mesh)
}

# inla mesh to sf
inlaMeshToSf <- function(mesh){
  # Extract mesh vertices and triangles
  vertices <- mesh$loc
  triangles <- mesh$graph$tv
  # Create a list to store the polygons representing mesh triangles
  tri_list <- lapply(1:nrow(triangles), function(i) {
    list(rbind(vertices[triangles[i,], 1:2], vertices[triangles[i, 1], 1:2])) # Closing the triangle by adding the first vertex again
  })
  # Convert list of polygons to sf object
  sf_mesh <- st_sf(geometry = st_sfc(lapply(tri_list, st_polygon), crs = mesh$crs$input))
  # View the resulting sf object
  return(sf_mesh)
}

# get best UTM crs for object x
utmCRS <- function(x){
  
  if (!inherits(x, c("sf", "sfc"))) {
    stop("'utmCRS' currently, only supports 'sf' and 'sfc' object.")
  }
  
  # ensure current crs is in lat/long projection
  crs <- st_crs(x)
  if(st_crs(crs)$units_gdal != "degree"){
    x <- st_transform(x, 4326)
  } 
  # get mean mean lat/long and corresponding UTM zone
  meanLon <- mean(st_bbox(x)[c(1,3)])  # mean longitude
  meanLat <- mean(st_bbox(x)[c(2,4)])  # mean latitude
  utmZone <- 1 + ((meanLon + 180) %/% 6)  # UTM zone
  
  # define UTM crs (assuming northern hemisphere)
  crs <- sprintf("+proj=utm +zone=%d +datum=WGS84 +units=m +no_defs +%s", 
                 utmZone, if(meanLat >= 0) "north" else "south")
  return(crs)
}