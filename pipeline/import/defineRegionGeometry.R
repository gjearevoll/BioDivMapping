

#### DEFINE REGION GEOMETRY ####

# The following script imports defines region geometry using 'defineRegion' and 
# saves it the working folder

library(sf)
library(dplyr)

# Import local functions
sapply(list.files("functions", full.names = TRUE), source)

###-----------------###
### 1. Preparation  ###
###-----------------###

# if it is not already, define dateAccessed
if (!exists("dateAccessed")) {
  dateAccessed <- as.character(Sys.Date())
}
folderName <- paste0("data/run_", dateAccessed)

# import project control parameters into the environment
readRDS(paste0(folderName,"/controlPars.RDS")) %>% 
  list2env(envir = .GlobalEnv)

###------------------###
### 2. define Region ###
###------------------###

# Run script to define geographical region and resolution we are working with 
#extentCoords <- c(4.641979, 57.97976, 31.05787, 71.18488)
#names(extentCoords) <- c("north", "south", "east", "west")
if (!exists("level")) {level <- "country"}  # level can be country, county, municipality, or points (examples of points given below)
if (!exists("region")) {region <- "Norway"}
if (level == "box") {
  regionGeometry <- defineRegion("box", extentCoords = extentCoords)
} else {
  regionGeometry <- defineRegion(level, region, dataSource = "external", runBuffer = TRUE)
}

# save into working folder
saveRDS(regionGeometry, paste0(folderName, "/regionGeometry.RDS"))
