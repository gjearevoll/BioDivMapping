
#' @title \emph{meshTest}: Visualise an INLA mesh before starting a model run

#' @description This function allows you to visualise an INLA mesh to check its parameters before using it in a model run.
#'
#' @param meshList A vector of GBIF taxon keys.
#' @param regionGeometry An sf object encompassing our region of study, as produced by defineRegion.
#' @param print Whether or not you want to plot the mesh automatically
#' 
#' @return A ggplot of an INLA mesh
#'

meshTest <- function(meshList, regionGeometry, print = TRUE) {
  modelCRS <- "Proj.4 +proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs" # 32632  # epsg code corresponding to same projection
  mesh <- INLA::inla.mesh.2d(boundary = inlabru::fm_as_inla_mesh_segment(sf::st_transform(regionGeometry, modelCRS)),
                             crs = inlabru::fm_crs(modelCRS),
                             cutoff= meshList$cutoff, 
                             max.edge=meshList$max.edge, 
                             offset= meshList$offset)
  meshPlot <- ggplot2::ggplot()+
    ggplot2::geom_sf(data = sf::st_transform(regionGeometry, modelCRS), fill = "white") +
    inlabru::gg(mesh)
  if (print == TRUE) {print(meshPlot)}
  return(mesh)
}

# inla mesh to sf
inla.mesh_to_sf <- function(mesh){
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