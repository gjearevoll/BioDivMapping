
### Define Dataset Processing ####

# This script assigns different functions to the datasets that they were created for. The functions herein
# reside in functions/integration.

# 1. The national insect Monitoring in Norway dataset
if (datasetName == "National insect monitoring in Norway") {
  focalEndpoint <- metadata$DWCEndpoint[metadata$name == datasetName]
  newDataset <- processNationalInsectMonitoring(focalEndpoint, tempFolderName, focalSpecies)
  
# 2. ANO Data
} else if (datasetName == "ANOData") {
  newDataset <- processANOData(focalData, regionGeometry, focalSpecies)
}