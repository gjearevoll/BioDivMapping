
#### The Open Biodiversity Mapper ####

# This is the master script for the Open Data Biodiversity Mapper. You can read more about the project,
# including what you need to set up before running this script, at the link below. The link also includes
# an example of the finished product.

# https://swp-data-projects.shinyapps.io/hotspotMaps2/

# You can access the GitHub repo directly here: https://github.com/gjearevoll/BioDivMapping

# Now, before you hit play and get stuck in, you need to make sure you've got all necessary R packages installed,
# of which there are quite a few. Luckily, we've set up this utils file for just that purpose (although since R
# doesn't always automatically acquiesce when setting up new packages you may need to go through it yourself).

source("pipeline/installAllPackages.R")

sapply(list.files("functions", full.names = TRUE, recursive = TRUE), source)

# Let's get started! The first script to run is the speciesImport.R script, which requires that you
# define a spatial level on which to run the pipeline, as well as a region within Norway. The options are
# "country", "county", "municipality", or "points", which is a box created by latitudinal and longitudinal 
# points (example given below). The default options are set for the county of Trøndelag. Codes for
# different municipalities and couunites in Norway can be found at this link:
# https://kartverket.no/til-lands/kommunereform/tekniske-endringer-ved-sammenslaing-og-grensejustering/komendr2020

# pointsExample <- c(4.641979, 57.97976, 31.05787, 71.18488)
# names(pointsExample) <- c("north", "south", "east", "west")

level <- "country"
region <- "Norway"

crs <- 25833  # as accepted by terra::cr
res <- 10000 # in m
crs <- 25833  # as accepted by sf::st_crs()
res <- 10000 # resolution in units of CRS (eg m in UTM, or degrees in lat/long)

# You also need to define whether or not you want to use a scheduled download. Scheduled downloads produce a DOI,
# and enable handling of much larger datasets. If you're playing around with a small dataset, you can probably hit 
# FALSE here.

scheduledDownload <- TRUE
waitForGbif <- TRUE
source("pipeline/import/taxaImport.R")

# Next we run the environmental import script, which brings in a set of rasters that apply to the region
# we defined in the last step.

myMesh <- list(cutoff = 11000, max.edge=c(42000, 49000), offset= 80000)
mesh <- meshTest(myMesh, regionGeometry, print = F) %>% 
  inla.mesh_to_sf()

source("pipeline/import/environmentalImport.R")

# Next we start on data processing, which adds extra information to our datasets.

source("pipeline/integration/speciesDataProcessing.R")

# We then run our models. NOTE: This is the point where defining a Mesh becomes important. You can read
# more about what a Mesh is, and how it works in the README.md file in the head of the repository, or in the
# FAQ page of the shiny app. If you want to try out some potential meshes, you can do so using the
# util file and editing the default list below.

meshTest(myMesh, regionGeometry)

# Once you've figured that out, you can start running the models. Remember that this script is the one that's 
# likely to take the longest, so grab a coffee or other beverage of choice.

source("pipeline/models/speciesModelRuns.R")

# And you're done! Now all that's left to do is to open up the app, which you can do by opening either the
# server.R or ui.R file in the visualisation/hotspotMaps folder and hitting "Run App".
