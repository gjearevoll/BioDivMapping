# Biodiversity mapping

This pipeline is dedicated to the production of biodiversity mapping and associated metrics for Norway. It imports, processes, and models species 
data from open-source data repositories, including GBIF and ANO. The current end product, the Hotspots App can be viewed 
[**at this link**](https://swp-data-projects.shinyapps.io/hotspotApp/).

## Pipeline structure

To run the pipeline yourself, see the How-To guide provided in [**the documentation folder**](https://github.com/gjearevoll/BioDivMapping/blob/main/documentation/odbmPipelineDocumentation.Rmd). 

## Folder structure

- data
  + external - This repository contains any data which needs to be inputted manually, including our species, dataset and covariate list, as well as the environmental covariate folder.
  + run_xxxx-xx-xx - Model output data for each species group. This folder is created automatically when models are initiated.
    * temp - This is where all data that is required during the pipeline but not required for the ODBM resides, such as our unzipped endpoint data files and GBIF lists.
    * modelOutputs - This contains a nested list of all species model outputs before they have been processed and formatted for the Hotspots App
  + temp - This is a separate temporary folder, used to store large environmental datasets locally to avoid having to constantly download them (eg. large elevation rasters or the rutenett for SSB data)
- documentation - this  folder contains documentation (in markdown format) for both the data pipleine and the Hotspots app
  + images - contains images that are used in the documentation
- pipeline - this folder contains all scripts which need to be run annually from the command line
  + imports - contains the scripts necessary for importing species and environmental data
  + integration - contains scripts that process data for input or output
  + models - contains scripts relevant to running integrated species distribution models
    * utils - each of the above folder contain a utils folder for processes that are one-off actions that further process our data but do not fall into the class of functions.
- functions - contains scripts which perform actions that are either a) useful for people outside of the project or b) repeated throughout the script or workflow.
- visualisations - contains the shiny app (hotspotMaps) which visualises biodiversity, species intensities and species occurrences
  + data - contains all data necessary for production of the ODBM
  + www - contains all one-off multimedia files for the ODMB

