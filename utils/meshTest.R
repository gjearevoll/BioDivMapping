

### Create and test Mesh ###

# THis file creates and visualises a very quick INLA Mesh

mesh <- INLA::inla.mesh.2d(boundary = inlabru::fm_as_inla_mesh_segment(regionGeometry),
                           crs = inlabru::fm_crs('+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'),
                           cutoff= myMesh$cutoff, 
                           max.edge=myMesh$max.edge, 
                           offset= myMesh$offset)

print(ggplot2::ggplot() +
  ggplot2::geom_sf(data = st_transform(regionGeometry, mesh$crs)) +
  inlabru::gg(mesh))
