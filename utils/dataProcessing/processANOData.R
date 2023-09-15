
# ANO data has a different nedpoint and has already been taken care of in the utils/ANOIntegration scropt
newDataset <- focalData[,c("simpleScientificName", "SHAPE", "individualCount", "dataType")]
newDataset$taxa <- focalSpecies$taxonomicGroup[match(newDataset$simpleScientificName, focalSpecies$species)]
newDataset <- rename(newDataset, geometry = SHAPE)