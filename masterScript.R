
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

# Before we begin, we will define all control parameters up front, which will be saved 
# in the working folder for reproducibility.

# Date of analysis from which working directory will be create/access
dateAccessed <- "2025-02-03"

# There are instances you want to re-initialise repository and delete some files that should be re-run
refresh <- FALSE

# spatial level on which regionGeometry will be defined as accepted by defineRegion()
level <- "county"
# specific region to be used as accepted by defineRegion()
region <- "50" 
# coordinate reference system to use for project. as accepted by sf::st_crs()
crs <- 32633 
# resolution in units of CRS (eg m in UTM, or degrees in lat/long)
res <- 500        # Resolution that covariates should be modelled at
# Parameters to define mesh for random fields
myMesh <- list(cutoff = 176, max.edge=c(26385, 175903), offset= c(1760, 18))
# whether to use schedule download for GBIF data
scheduledDownload <- TRUE
# whether to wait and automatically download GBIF data when it is ready
waitForGbif <- FALSE
# which categories are to be used for filtering/analysing red list species
redListCategories <- c("VU", "EN", "CR")
# number of species per group in richness model:
nSegment <- 10
speciesOccurenceThreshold <- 50
datasetOccurreneThreshold <- 5000
# model priors
prior.range <- c(100, 0.01)
prior.sigma <- c(0.8, 0.01)
# Indicates whether you want to run the model in parallel
parallelisation <- FALSE

# Indicates whether we want to download the ANOData or use the data from file
downloadANOData <- TRUE

# If we have already run some code with the same dateAccessed and we want to 
# re-start the initialisation process:
if(refresh){
  deleteFilesToRestart(dateAccessed)
}

# Let's get started! The first script initialiseRepository.R, which will create 
# a folder for the specified dateAccessed, filters focalTaxa for taxa to be analyzed 
# and assigns missing usageKeys. Last, it saves a copy of focalTaxa.csv, polyphyleticSpecies.csv,
# metadataSummary.csv, and focalCovariates.csv in the working folder for reproducibility.

source("pipeline/import/initialiseRepository.R")

# Next, we will run defineRegionGeometry.R, which will create the requires that you
# define a spatial level on which to run the pipeline, as well as a region within Norway. The options are
# "country", "county", "municipality", or "points", which is a box created by latitudinal and longitudinal 
# points (example given below). The default options are set for the whole of Norway. Codes for
# different municipalities and couunites in Norway can be found at this link:
# https://kartverket.no/til-lands/kommunereform/tekniske-endringer-ved-sammenslaing-og-grensejustering/komendr2020

# pointsExample <- c(4.641979, 57.97976, 31.05787, 71.18488)
# names(pointsExample) <- c("north", "south", "east", "west")

source("pipeline/import/defineRegionGeometry.R")

# You also need to define whether or not you want to use a scheduled download. Scheduled downloads produce a DOI,
# and enable handling of much larger datasets. If you're playing around with a small dataset, you can probably hit 
# FALSE above, at line 40.

source("pipeline/import/taxaImport.R")

# Next we run the environmental import script, which brings in a set of rasters that apply to the region
# we defined in the last step.

source("pipeline/import/environmentalImport.R")

# Next we start on data processing, which adds extra information to our datasets.

source("pipeline/integration/speciesDataProcessing.R")

# We then run our models. NOTE: This is the point where defining a Mesh becomes important. You can read
# more about what a Mesh is, and how it works in the README.md file in the head of the repository, or in the
# FAQ page of the shiny app. If you want to try out some potential meshes, you can do so using the
# util file and editing the default list below. We've pre-defined a mesh here which is suitable for Norway.
myMesh <- list(cutoff = 176, max.edge=c(26850, 175903), offset= c(1760, 1200)*10)
meshTest(myMesh, regionGeometry, print = TRUE, crs = crs)

# Once you've figured that out, you can start running the models. Remember that this stage will be the longest.
# If you are running this for all of Norway, at this point automation is unfortunately not an option, and you
# will need to ignore the following two scripts and log onto Sigma2 to use their High Performance Computing 
# infrastructure - you can find the files necessary to use this service in pipeline/parallelModelRun.
source("pipeline/models/speciesModelRuns.R")

# Once the models are run, you can run the prediction scripts.
source("pipeline/models/speciesPredictionRuns.R")

# Now that the computing intensive scripts are finished, you can come back to the comfort of working in R.
# We need to compute sampling densities, which can be done with the following script.
source("pipeline/models/samplingDensityProduction.R")


