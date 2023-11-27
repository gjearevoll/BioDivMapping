
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