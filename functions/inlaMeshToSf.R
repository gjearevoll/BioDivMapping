#' Convert an INLA Mesh to an 'sf' Object
#'
#' This function converts a mesh object from the INLA framework into an 'sf' (simple features) object.
#' This is useful for visualizing INLA mesh structures using libraries that support 'sf' objects.
#' The function extracts vertices and triangles from the INLA mesh, creates polygons representing the mesh triangles,
#' and then converts these polygons into an 'sf' object.
#'
#' @param mesh An INLA mesh object. This should be a list containing at least 'loc' for locations (vertices) and
#' 'graph' for the mesh graph structure, including a 'tv' element that describes the triangles of the mesh.
#'
#' @return An 'sf' object representing the mesh. Each feature in the returned 'sf' object corresponds to a triangle in the INLA mesh.
#' The 'sf' object contains geometries of type POLYGON.
#'
#' @export
#'
#' @examples
#' # Assuming 'my_mesh' is a valid INLA mesh object
#' # sf_mesh <- inlaMeshToSf(my_mesh)
#' # plot(sf_mesh)
#'
#' @importFrom sf st_sf st_sfc st_polygon

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
