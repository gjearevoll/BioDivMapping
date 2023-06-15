# Hotspots

This pipeline is dedicated to the production of biodiversity hotspot metrics for Norway. It imports, processes, and models species
data from open-source data repositories, including GBIF and ANO. Currently the pipeline is in a draft format, producing species maps for 
three vascular plant species across Trondheim.

## Pipeline structure

When the pipeline is running at an acceptable scope or level a more detailed instruction guide will be provided, however for the moment the description below serves as a brief overview of the pipeline structure. A viualisation of the workflow can be found [at this Miro board](https://miro.com/app/board/uXjVMCkk6YI=/).

The first step is the speciesImport.R script, which imports species data from different sources and characterises them using
their data type (presence/absence, occurrence-only or abundance) and data source (GBIF, ANO, other). In this step we also define the 
region across which we want to measure hotspots.

The environmentalImport.R script is then used to import environmental data across the same region.

The speciesModelRuns.R data scripts are then used to coalesce the data into a format whereby it can be input into Philip Mosert's
[intSDM package](https://github.com/PhilipMostert/intSDM). The script creates a new subfolder in the data folder based (unless otherwise 
specified) on the date that the models are being run (run_xxxx-xx-xx). All output data is saved here, with species models nested under 
taxa groups.

The biodiversityMetricEstimation.R script is then used to aggregate the results of the integrated species models to a biodiversity 
metric. This, along with the results from individual species models, is saved in the data folder, as well as directly into the
visualisation folder.

The visualisation/hotspotMap folder contains a shiny app which shows species occurence data, modelled species intensity data, and
biodiversity metrics.

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
