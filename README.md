# Biodiversity mapping

This pipeline is dedicated to the production of biodiversity mapping and associated metrics for Norway. It imports, processes, and models species
data from open-source data repositories, including GBIF and ANO. Currently the pipeline is in a draft format, producing species maps for 
three vascular plant species across Trondheim.

## Pipeline structure

When the pipeline is running at an acceptable scope or level a more detailed instruction guide will be provided, however for the moment the description below serves as a brief overview of the pipeline structure. A viualisation of the workflow can be found [at this Miro board](https://miro.com/app/board/uXjVMCkk6YI=/).

#### [speciesImport.R](https://github.com/gjearevoll/BioDivMapping/blob/main/pipeline/import/speciesImport.R)

This is the starting point for the entire pipeline. There are a few inputs that should be defined at the top of the script before running. 
These are necessary imports for the defineRegion.R util script, which defines the spatial region we are looking at during the pipeline run.
These will later be converted to function arguments.

- level - This defines the spatial level for which the pipeline will run. Options are 'municipality', 'county', 'country' or 'points'.
- region - Defines the region. If 'municipality' or 'county', use Norwegian standard numerical codes to define the region 
([found here](https://kartverket.no/til-lands/kommunereform/tekniske-endringer-ved-sammenslaing-og-grensejustering/komendr2020)).

The script uses two csv files as well.

- focalTaxa.csv - Defines the taxonomic groups we are focussing on and their necessary codes in GBIF.
- focalSpecies.csv - Defines the species we are using in the pipeline.

Thereafter the script imports data from GBIF using the spatial region defined above. It also imports necessary data from the ANO database.

**Outputs**: Full species data list (sf class), regional geometry file (sf class).

#### [environmentalImport.R](https://github.com/gjearevoll/BioDivMapping/blob/main/pipeline/import/environmentalImport.R)

The environmentalImport.R script is then used to import environmental data across the same region. The focalCovariates.csv file defines which covariates we are using in the model. The environmental data has been provided by Dr. Ron Togunov. Further information on the data sources used can be found in the [external/data/environmentalCovariates](https://github.com/gjearevoll/BioDivMapping/tree/main/data/external/environmentalCovariates) description.

**Outputs**: List of environmental datasets (raster class).

#### [speciesProcessing.R](https://github.com/gjearevoll/BioDivMapping/blob/main/pipeline/processing/speciesDataProcessing.R)

Since some of the databases are listed as presence only but are in fact presence absence, there is additional processing required. This 
script takes those datasets and checks which species were surveyed for. If those species do not show up in an event, they are considered as
absences in that event.

Additionally, since this script provides the final dataset which will be used in the modelling, there is an option in this script to upload the dataset to Wallace. This is set to FALSE as a default for now.

**Outputs**: List of processed datasets (sf).

#### [speciesModelRuns.R](https://github.com/gjearevoll/BioDivMapping/blob/main/pipeline/models/speciesModelRuns.R)

The speciesModelRuns.R data scripts are then used to coalesce the data into a format so that it can be inputted into Philip Mosert's
[intSDM package](https://github.com/PhilipMostert/intSDM). The utils script modelPreparation.R also plays a large role here, merging the species and environmental data into an R6 Environment object.*

**Important**: The properts of the INLA Mesh need to be adjusted whenever the region is changed. Best to run a few trials whenever you do this using `workflow$plot(Mesh = TRUE)`.

**Outputs**: Each species run through the model gets its own folder with a map and set of predictions.

#### [biodiversityMetricEstimation.R](https://github.com/gjearevoll/BioDivMapping/blob/main/pipeline/processing/biodiversityMetricEstimation.R)

The biodiversityMetricEstimation.R script is then used to aggregate the results of the integrated species models to a biodiversity 
metric. This, along with the results from individual species models, is saved in the data folder, as well as directly into the
visualisation folder.

#### [visualisation/hotspotMaps](https://github.com/gjearevoll/BioDivMapping/tree/main/visualisation/hotspotMaps)

The visualisation/hotspotMap folder contains a shiny app which shows species occurrence data, modelled species intensity data, and
biodiversity metrics. THe output can be previewed in **[this shiny app](https://swp-data-projects.shinyapps.io/hotspotMaps/)**.

## Folder structure

- data
  + external - This is a temporary repository for any data we currently have stored locally, which will in the future be accessed externally (it. GBIF data, environmental data downloaded from external servers)
  + temp - Another temporary data folder, this is where all data will be stored that would normally be uploaded to our server and then accessed again later in the pipeline
  + run_xxxx-xx-xx - Model output data for each species group. This folder is created automatically when models are initiated.
- pipeline - this folder contains all scripts which need to be run anually from the command line
  + imports - contains the scripts necessary for importing species and environmental data
  + models - contains scripts relevant to running integrated species distribution models
  + processing - contains scripts that process data for input or output
- utils - contains scripts which perform action that do not require manual inputs. Will be turned into functions in the future.
- visualisations - contains the shiny app (hotspotMaps) which visualise biodiversity, species intensities and species occurrences
