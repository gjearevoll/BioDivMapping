

### Create and test Mesh ###

# THis file creates and visualises a very quick INLA Mesh

workflow <- startWorkflow(
  Projection = '+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs',
  Species = "none",
  saveOptions = list(projectDirectory = "data", projectName =  "test"), Save = FALSE
)
workflow$addArea(Object = st_sf(regionGeometry), resolution = '60')
workflow$addMesh(cutoff= myMesh$cutoff, max.edge=myMesh$max.edge, offset= myMesh$offset)
print(workflow$plot(Mesh = TRUE))
