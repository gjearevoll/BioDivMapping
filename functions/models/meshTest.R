

### Create and test Mesh ###

# THis file creates and visualises a very quick INLA Mesh


# myMesh <- list(cutoff = 11000, max.edge=c(39000, 46000), offset= 80000)

meshTest <- function(meshList, regionGeometry, print = TRUE) {
  
  mesh <- INLA::inla.mesh.2d(boundary = inlabru::fm_as_inla_mesh_segment(regionGeometry),
                             crs = inlabru::fm_crs('+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'),
                             cutoff= meshList$cutoff, 
                             max.edge=meshList$max.edge, 
                             offset= meshList$offset)
  
  meshPlot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = st_transform(regionGeometry, mesh$crs)) +
    inlabru::gg(mesh)
  if (print == TRUE) {print(meshPlot)}
  return(meshplot)
}
