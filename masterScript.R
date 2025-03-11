
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
dateAccessed <- "2024-01-26"  

# There are instances you want to re-initialise repository and delete some files that should be re-run
refresh <- FALSE

# spatial level on which regionGeometry will be defined as accepted by defineRegion()
level <- "country"  
# specific region to be used as accepted by defineRegion()
region <- "Norway" 
# coordinate reference system to use for project. as accepted by sf::st_crs()
crs <- 25833 
# resolution in units of CRS (eg m in UTM, or degrees in lat/long)
res <- 1000 
# Parameters to define mesh for random fields
myMesh <- list(cutoff = 30000, max.edge=c(4000000, 380000), offset= c(4000, 10000))
# Defiine whether or not we want to upload this data to Wallace
uploadToWallace <- FALSE
# whether to use schedule download for GBIF data
scheduledDownload <- TRUE
# whether to wait and automatically download GBIF data when it is ready
waitForGbif <- TRUE
# minimum number of points for a species to be retained in the analysis
redListThreshold <- 30
# which categories are to be used for filtering/analysing red list species
redListCategories <- c("VU", "EN", "CR")
# the type of model that will be fitted to the data
modelRun <- "richness"  # one of: "redListSpecies", "redListRichness", "richness", or "allSpecies"
# number of species per group in richness model:
nSegment <- 10
# model priors
prior.range <- c(10, 0.01)
prior.sigma <- c(1, 0.01)

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
# FALSE here.

source("pipeline/import/taxaImport.R")

# Next we run the environmental import script, which brings in a set of rasters that apply to the region
# we defined in the last step.

source("pipeline/import/environmentalImport.R")

# Next we start on data processing, which adds extra information to our datasets.

source("pipeline/integration/speciesDataProcessing.R")

# We then run our models. NOTE: This is the point where defining a Mesh becomes important. You can read
# more about what a Mesh is, and how it works in the README.md file in the head of the repository, or in the
# FAQ page of the shiny app. If you want to try out some potential meshes, you can do so using the
# util file and editing the default list below.

meshTest(myMesh, regionGeometry, crs = crs)

# Once you've figured that out, you can start running the models. Remember that this script is the one that's 
# likely to take the longest, so grab a coffee or other beverage of choice. There are three choices of modelRun, 
# 'richness' (estimates species richness), 'redListRichness' (same but only for red-listed species) and 'redListSpecies'
# (individual species models for red-listed species). We suggest running these individually.
source("pipeline/models/speciesModelRuns.R")

# Now that all the necessary data has been produced, we can compile and export it for use in the app. Just use the 
# function below to compile and move the necessary results into the visualisation folder. Here, date accessed is a 
# required input.

source("pipeline/models/utils/modelResultsCompilation.R")

# And you're done! Now all that's left to do is to open up the app, which you can do by opening either the
# server.R or ui.R file in the visualisation/hotspotMaps folder and hitting "Run App".
