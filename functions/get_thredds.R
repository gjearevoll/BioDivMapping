#' @title \emph{get_thredds}: Download and aggregate NGCD summer climate data from MET Norway THREDDS

#' @description Downloads daily files of the Nordic Gridded Climate Dataset (NGCD; 1 km, Norway/Sweden/Finland,
#' doi 10.24381/cds.e8f4a10c) straight from MET Norway's THREDDS server (no login needed), averages each year's
#' chosen months, and crops/masks the result to the region. Works through one year at a time: it downloads that
#' year's daily files, averages them, caches the yearly mean as a GeoTIFF, then deletes the daily files.
#' Cached years are reused, so an interrupted run carries on where it stopped.
#'
#' @param yearInterval Years to process. Default (NA) is the last ten complete years before the current one.
#' @param regionGeometry sf/SpatVector of the study region.
#' @param focalParameter One of the names in ngcdParameters below: "summer_temperature" (mean of daily mean
#'   temperature TG, Kelvin) or "summer_precipitation" (total of daily precipitation RR over the months, mm).
#'   To add another variable, add an entry to ngcdParameters.
#' @param dataPath Folder for the cached yearly rasters (and the daily files while they're being processed).
#' @param temporalFactor If TRUE, return one layer per year. If FALSE, return a single layer averaged over all years.
#' @param months Months to average over. Default is June-August.
#' @param ngcdType NGCD product type, "type1" or "type2".
#' @param ngcdVersion NGCD version folder on THREDDS.
#'
#' @return A SpatRaster in EPSG:3035 with layers named by year (temporalFactor = TRUE), or one averaged layer.
#library(terra)

# How each focalParameter maps onto NGCD: which variable to download and how to combine the daily values
# within a year. Across years, the yearly values are always averaged.

get_thredds <- function(yearInterval = NA, regionGeometry, focalParameter, dataPath, temporalFactor,
                           months = 6:8, ngcdType = "type2", ngcdVersion = "26.09") {
  
  ngcdParameters <- list(
    summer_temperature   = list(variable = "TG", dailyAggregation = "mean"),
    summer_precipitation = list(variable = "RR", dailyAggregation = "sum")
  )
  
  
  if (all(is.na(yearInterval))) {
    currentYear <- as.integer(format(Sys.Date(), "%Y"))
    yearInterval <- (currentYear - 10):(currentYear - 1)
  }
  if (!focalParameter %in% names(ngcdParameters)) {
    stop("focalParameter must be one of: ", paste(names(ngcdParameters), collapse = ", "))
  }
  ngcdVariable <- ngcdParameters[[focalParameter]]$variable
  dailyAggregation <- ngcdParameters[[focalParameter]]$dailyAggregation
  baseUrl <- "https://thredds.met.no/thredds/fileServer/ngcd"
  
  cacheDir <- file.path(dataPath, "ngcd")
  dir.create(cacheDir, showWarnings = FALSE, recursive = TRUE)
  
  # Region in the NGCD projection (ETRS89 / LAEA Europe)
  regionVect <- project(vect(regionGeometry), "EPSG:3035")
  
  yearRasters <- lapply(yearInterval, function(year) {
    cacheFile <- file.path(cacheDir, sprintf("NGCD_%s_%s_%s_v%s_%d_months%s.tif", ngcdVariable, dailyAggregation,
                                             ngcdType, ngcdVersion, year, paste(months, collapse = "-")))
    if (file.exists(cacheFile)) {
      cat("Using cached NGCD", ngcdVariable, "for", year, "\n")
      return(rast(cacheFile))
    }
    
    # Every day in the chosen months of this year
    monthStarts <- as.Date(sprintf("%d-%02d-01", year, months))
    dates <- do.call(c, lapply(monthStarts, function(m) {
      seq(m, seq(m, by = "month", length.out = 2)[2] - 1, by = "day")
    }))
    
    dayDir <- file.path(cacheDir, paste0("daily_", year))
    dir.create(dayDir, showWarnings = FALSE)
    
    cat("Downloading", length(dates), "daily NGCD", ngcdVariable, "files for", year, "\n")
    dayFiles <- vapply(dates, function(d) {
      fileName <- sprintf("NGCD_%s_%s_version_%s_%s.nc", ngcdVariable, ngcdType, ngcdVersion, format(d, "%Y%m%d"))
      url <- paste(baseUrl, paste0("version_", ngcdVersion), ngcdVariable, ngcdType,
                   format(d, "%Y"), format(d, "%m"), fileName, sep = "/")
      destFile <- file.path(dayDir, fileName)
      if (file.exists(destFile)) return(destFile)
      
      # Download to a .part file first so a broken download never looks like a finished one
      partFile <- paste0(destFile, ".part")
      for (attempt in 1:3) {
        status <- tryCatch(download.file(url, partFile, mode = "wb", quiet = TRUE),
                           error = function(err) 1)
        if (status == 0) break
        Sys.sleep(5 * attempt)
      }
      if (status != 0) {
        unlink(partFile)
        stop("Failed to download ", url, " after 3 attempts. Check https://thredds.met.no/thredds/catalog/ngcd/catalog.html")
      }
      file.rename(partFile, destFile)
      destFile
    }, character(1))
    
    # Combine the days (mean or sum), then crop and mask to the region
    dailyStack <- rast(dayFiles)
    crs(dailyStack) <- "EPSG:3035"  # the files carry an unnamed LAEA definition; their metadata says EPSG:3035
    yearValue <- app(crop(dailyStack, regionVect), dailyAggregation)
    yearValue <- mask(yearValue, regionVect)
    names(yearValue) <- as.character(year)
    
    writeRaster(yearValue, cacheFile, overwrite = TRUE)
    unlink(dayDir, recursive = TRUE)
    rast(cacheFile)
  })
  
  climateRaster <- rast(yearRasters)
  names(climateRaster) <- as.character(yearInterval)
  
  if (!temporalFactor) {
    climateRaster <- mean(climateRaster)
    names(climateRaster) <- paste(range(yearInterval), collapse = "_")
  }
  climateRaster
}
