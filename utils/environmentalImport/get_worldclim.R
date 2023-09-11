# Set extent for download
regionExtent <- ext(regionGeometry_buffer)

print(paste0("Downloading ", focalParameter," from ", dataSource))

source("utils/environmentalImport/function_get_worldclim.R")

# prep download variables
recode_vector <- c("temperature" = "tavg",
                   "minimum temperature" = "tmin",
                   "maximum temperature" = "tmax",
                   "precipitation" = "prec",
                   "bio" = "bio",
                   "bioc" = "bioc",
                   "elevation" = "elev",
                   "wind speed" = "wind",
                   "vapor pressure" = "vapr",
                   "solar radiation" = "srad")

var <- recode_vector[focalParameter]

# download
annualStack <- get_worldclim(regionExtent, var, 0.5)

# average
rasterisedVersion <- mean(annualStack)
