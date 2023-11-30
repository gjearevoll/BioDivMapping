
#' @title \emph{get_worldclim}: This function downloads and merges corine land cover data for a study area

#' @description This function unzips and constructs a raster of land cover data. It requires the zip file with corine data to already be downloaded from https://land.copernicus.eu/en/products/corine-land-cover/clc2018 and saved as corineRaw in the data/external/environmentalCovariates folder. 
#'
#' @param regionGeometryBuffer Spatial vector representing region of interest
#' @param targetDir folder where all unzipped data should be saved
#'
#' 
#' @return A SpatRaster of WorldClim layers.


get_corine <- function(regionGeometryBuffer, targetDir) {
  if(!file.exists("data/external/environmentalCovariates/corineRaw")) {
    warning("CORINE land cover data cannot be found. Data can be downloaded from https://land.copernicus.eu/en/products/corine-land-cover/clc2018.
            Once downloaded, the zip file should be unzipped and the resulting folder saved to data/external/environmentalCovariates and renamed 
            corineRaw.")
    rasterisedVersion <- baseRaster  
  } else {
    
    # Unzip all folders and download raster
    unzip("data/external/environmentalCovariates/corineRaw/EEA.zip", exdir = targetDir)
    unzip(paste0(targetDir, "/U2018_CLC2018_V2020_20u1_raster100m_tiled_doc.zip"), 
          exdir = targetDir)
    unzip(paste0(targetDir, "/U2018_CLC2018_V2020_20u1.zip"), 
          exdir = targetDir)
    corine <- rast(paste0(targetDir,"/U2018_CLC2018_V2020_20u1.tif"))
    
    # Crop raster to region specified
    corineSpecified <- crop(corine,
                       as.polygons(terra::project(regionGeometryBuffer, corine),  extent = T), 
                       snap = "out", mask = T)
    
    #Import legend data from info
    importLegend <- read.delim(paste0(targetDir,"/Info/Legend/Vector/clc_legend_qgis.txt"), 
                               sep =  ",", header = FALSE, skip = 2)
    legendTranslation <- data.frame(mapValue = importLegend$V1,
                                    corineCodeText = substr(importLegend$V6, 7, nchar(importLegend$V6)))
    
    # Create discrete values for raster
    rasterValues <- data.frame(mapValue = values(corineSpecified))
    values(corineSpecified) <- legendTranslation$corineCodeText[match(rasterValues$U2018_CLC2018_V2020_20u1, legendTranslation$mapValue)]
    rasterisedVersion <- corineSpecified
    }
  
  return(rasterisedVersion)
  
}


