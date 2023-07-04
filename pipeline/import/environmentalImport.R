

#### ENVIRONMENTAL DATA IMPORT ####

# The following script imports our various forms of environmental data and processes them, based on the type of data
# and other specifications related to the source.

###-----------------###
### 1. Preparation ####
###-----------------###

# Run script to define geographical region and resolution we are working with 
# Initialise folders for storage of all run data
if (!exists("dateAccessed")) {
  dateAccessed <- as.character(Sys.Date())
}

speciesDataList <- readRDS(paste0("data/run_", dateAccessed, "/temp/speciesDataImported.RDS"))
regionGeometry <- speciesDataList$geometry

###--------------------###
### 2. Dataset Import ####
###--------------------###

# Import temperature data
load('data/external/MeanSpringSummerTemp.rda')
newproj <- "+proj=longlat +ellps=WGS84 +no_defs"
temp2 <- raster::projectRaster(MeanSpringSummerTemp, crs=newproj, res=0.1)
names(temp2) <- "temperature"
temperaturecropped <- crop(temp2, raster::extent(st_bbox(regionGeometry)))
temperature <- scale(temperaturecropped)

# Import precipitation data
load('data/external/MeanSpringSummerPrec.rda')
newproj <- "+proj=longlat +ellps=WGS84 +no_defs"
prec2 <- raster::projectRaster(MeanSpringSummerPrec, crs=newproj, res=0.1)
names(prec2) <- "precipitation"
precipitationcropped <- crop(prec2, raster::extent(st_bbox(regionGeometry)))
precipitation <- scale(precipitationcropped)

###--------------------###
### 3. Dataset Upload ####
###--------------------###

# For now we're just doing this to the data/temp folder, later this will go to Wallace
dataList <- list(precipitation = precipitation, 
                 temperature = temperature)
saveRDS(dataList, paste0("data/run_", dateAccessed,"/temp/environmentalDataImported.RDS"))
