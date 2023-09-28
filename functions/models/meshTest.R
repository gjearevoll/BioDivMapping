

### Create and test Mesh ###

# THis file creates and visualises a very quick INLA Mesh


# myMesh <- list(cutoff = 11000, max.edge=c(39000, 46000), offset= 80000)

meshTest <- function(meshList, regionGeometry, print = TRUE) {
  modelCRS <- "Proj.4 +proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs" # 32632  # epsg code corresponding to same projection
  mesh <- INLA::inla.mesh.2d(boundary = inlabru::fm_as_inla_mesh_segment(st_transform(regionGeometry, modelCRS)),
                             crs = inlabru::fm_crs(modelCRS),
                             cutoff= meshList$cutoff, 
                             max.edge=meshList$max.edge, 
                             offset= meshList$offset)
  meshPlot <- ggplot2::ggplot()+
    ggplot2::geom_sf(data = st_transform(regionGeometry, modelCRS), fill = "white") +
    inlabru::gg(mesh)
  if (print == TRUE) {print(meshPlot)}
}