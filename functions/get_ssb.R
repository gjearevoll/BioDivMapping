
#' @title \emph{get_ssb}: This function downloads human and building density data from SSB

#' @description This function downloads human and building density directly from the Statistisk Sentralbyraa.
#'
#' @param focalParameter The parameter to download - either "human_density" or "building_density".
#' @param resolution The data resolution in metres, either "250" or "1000".
#' @param year Integer. The reference year for the grid layer to download.
#'   Layers on the SSB WFS are year-specific; defaults to 2024.
#'
#' @return An aggregated raster containing anthropogenic data for Norway.
#'
#' @details
#' The old geonorge endpoints (e.g. \code{wfs.befolkningsstatistikkrutenett1kmhistorisk})
#' were retired: the server now returns
#' "UKJENT APPLIKASJON" / "Applikasjon ... er ukjent og kan ikke rutes videre".
#' SSB has moved the WMS/WFS services to the kart.ssb.no MapServer endpoints,
#' where each year is exposed as a separate FeatureType (e.g.
#' \code{ms:befolkning_1km_2024}, \code{ms:bygningsmassen_250m_2024}).
#' Attribute names also changed:
#' population total is now \code{pop_tot} (was \code{popTot}), and total
#' buildings is \code{Bygninger_i_alt} (was \code{bui0all}).
#'

get_ssb <- function(focalParameter, resolution = "1000", year = 2024) {
  # New base WFS endpoints (kart.ssb.no MapServer)
  wfsBase <- list(
    human_density    = "https://kart.ssb.no/api/mapserver/v1/wfs/befolkning_paa_rutenett",
    building_density = "https://kart.ssb.no/api/mapserver/v1/wfs/bygninger_paa_rutenett"
  )

  # FeatureType (layer) name follows the pattern ms:<theme>_<res>_<year>
  # Note: SSB uses "1km" (not "1000") in layer names, but "250m" as written.
  resTag <- switch(as.character(resolution),
                   "250"  = "250m",
                   "1000" = "1km",
                   stop("resolution must be '250' or '1000'"))

  themeTag <- switch(focalParameter,
                     human_density    = "befolkning",
                     building_density = "bygningsmassen",
                     stop("focalParameter must be 'human_density' or 'building_density'"))

  layerName <- sprintf("ms:%s_%s_%d", themeTag, resTag, year)

  capabilitiesUrl <- paste0(wfsBase[[focalParameter]],
                            "?service=WFS&version=2.0.0&request=GetCapabilities")

  message(sprintf("Downloading %s at %s resolution (%d) from SSB [layer=%s]",
                  gsub("_", " ", focalParameter), resTag, year, layerName))

  # GDAL's WFS driver enumerates FeatureTypes as layers; select the year we want.
  V0 <- terra::vect(paste0("WFS:", capabilitiesUrl), layer = layerName)


  # Updated attribute names on the new SSB service
  parameterField <- switch(focalParameter,
                           human_density    = "pop_tot",
                           building_density = "Bygninger_i_alt")
  V1 <- st_as_sf(V0)
  V2 <- vect(st_transform(V1, crs = 25833))
  
  # define reference raster based on resolution
  R0 <- rast(res = as.numeric(resolution),
             ext = ext(V2), crs = crs(V2)) 
  # Project onto our reference rasters
  R2 <- terra::rasterize(terra::project(V2, R0), R0, field = parameterField)
  R3 <- terra::lapp(R2, function(x) ifelse(is.na(x), 0, x))

  return(R3)

}
