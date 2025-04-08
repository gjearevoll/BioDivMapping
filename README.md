# Biodiversity mapping

This pipeline is part of the Hotspots project, and is dedicated to the production of biodiversity mapping and associated metrics 
for Norway. It imports, processes, and models species data from open-source data repositories, including GBIF and ANO. The current
end product, the Hotspots App can be viewed [**at this link**](https://swp-data-projects.shinyapps.io/hotspotApp/). The most
recent report regarding the project (submitted to Miljodirektoratet) can be viewed [**at this link**](https://www.miljodirektoratet.no/publikasjoner/2024/januar-2024/modellering-av-heildekkande-utbreiingskart-for-arter/).

## Pipeline structure

To run the pipeline yourself, see the How-To guide provided in [**the documentation folder**](https://github.com/gjearevoll/BioDivMapping/blob/main/documentation/odbmPipelineDocumentation.Rmd). Please note that
running the final section of the pipeline (the modelling and predictions) without a High Powered Computing set-up
will only work for smaller regions of Norway.

## Folder structure

- data
  + external - This repository contains any data which needs to be inputted manually, including our species, dataset and covariate list, as well as the environmental covariate folder.
  + run_xxxx-xx-xx - Model output data for each species group. This folder is created automatically when models are initiated.
    * temp - Much larger data is stored here. This data is all necessary for the running of the pipeline but gives more detailed
    information than is required for the production of metadata after the pipeline has been run, and we've demmed it unnecessary to
    push to GitHub.
    * modelOutputs - This contains a nested list of all species model outputs before they have been processed and formatted for the Hotspots App
  + temp - This is a separate temporary folder, used to store large environmental datasets locally to avoid having to constantly download them (eg. large elevation rasters or the rutenett for SSB data)
- documentation - this  folder contains documentation (in markdown format) for both the data pipeline
  + images - contains images that are used in the documentation
- pipeline - this folder contains all scripts which need to be run annually from the command line
  + imports - contains the scripts necessary for importing species and environmental data
  + integration - contains scripts that process data for input or output
  + models - contains scripts relevant to running integrated species distribution models
  + 
    * utils - each of the above folder contain a utils folder for processes that are one-off actions that further process our data but do not fall into the class of functions.
- functions - contains scripts which perform actions that are either a) useful for people outside of the project or b) repeated throughout the script or workflow.

