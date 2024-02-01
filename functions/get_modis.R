

library(MODISTools)
library(MODIStsp)


get_modis <- function(regionGeometry, projCRS, parameter) {
  
  Noreg_si <- st_transform(regionGeometry, crs="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs") #Correct
  ll <- st_bbox(Noreg_si)
  ll <- ll + c(-10000, -10000, 10000, 10000)
  
  products <- MODIStsp_get_prodnames() # just a list of the available MODIS products
  
  ### 1. Net primary productivity ####
  
  if (parameter == "net_primary_productivity") {
    
    # To get the NPP data
    bands <- MODIStsp_get_prodlayers("Net_PP_GapFil_Yearly_500m (M*D17A3HGF)") # The different bands in the NPP data
    dates <- mt_dates(product = "MYD17A3HGF", lat = 42, lon = -110) # not very important, annual data all are 01.01.YYYY
    
    if (!dir.exists("data/temp/MODIS/nppMod")) {
      test <- MODIStsp(gui=FALSE,
                       out_folder="data/temp/MODIS",
                       out_folder_mod="data/temp/MODIS/nppMod",
                       selprod="Net_PP_GapFil_Yearly_500m (M*D17A3HGF)",
                       sensor="Terra",
                       bandsel = "Npp_500",
                       user = "mstp_test" ,
                       password = "MSTP_test_01",
                       start_date="2002.01.01",
                       end_date="2022.01.01", 
                       spatmeth="bbox",
                       bbox=ll,
                       reprocess = TRUE,
                       delete_hdf=FALSE,
                       verbose = TRUE
      )
    }
    
    # 2020 is missing, skip that year
    for (i in c(2002:2019,2021,2022)) {
      NPPfiles <- list.files('data/temp/modis/nppMod',
                             paste0("A", i), full.names = T)
      NPPrast <- sprc(lapply(NPPfiles, function(x) rast(x, lyrs="Npp_500m")))
      NPPrastall <- merge(NPPrast)
      if(i == 2002)
      {
        NoregNPP <- crop(NPPrastall, vect(Noreg_si), mask=TRUE)
      } else {
        NoregNPP <- rast(list(crop(NPPrastall, vect(Noreg_si), mask=TRUE), NoregNPP))
      }
      print(i)
      flush.console()
    }
    
    
    # Values as stored as integers, and must be multiplied with 0.0001 to get the
    # correct unit: kgC/m²/year
    # documentation here: https://lpdaac.usgs.gov/products/mod17a3hgfv006/
    NoregNPP <- NoregNPP * 0.0001
    
    # Fill values (ice, water etc) are values > 32760*0.0001. Set these to NA
    NoregNPP[NoregNPP > 3.276] <- NA
    
    # mean over all years, the final product
    NoregMeanNPP <- scale(app(NoregNPP, fun=function(x) mean(x, na.rm=TRUE)))
    finalMap <- terra::project(NoregMeanNPP, projCRS)
    return(finalMap)
    
  } 
  
  ### 2. Peak NDVI ####
  
  else if (parameter == "ndvi_peak") {
    # Extract NDVI for spring and summer, and get the peak value (maximum NDVI during the year)
    bands <- MODIStsp_get_prodlayers("Vegetation Indexes_16Days_250m (M*D13Q1)")
    
    # we want "250m_16_days_NDVI", fill value is -3000, scale factor = 0.0001
    # documentation: https://lpdaac.usgs.gov/products/mod13q1v061/
    if (!dir.exists("data/temp/modis/ndviMod")) {
    test <- MODIStsp(gui=FALSE,
                     out_folder="data/temp/modis",
                     out_folder_mod="data/temp/modis/ndviMod",
                     selprod="Vegetation Indexes_16Days_250m (M*D13Q1)",
                     sensor="Terra",                                  
                     bandsel = "NDVI",
                     user = "mstp_test" ,
                     password = "MSTP_test_01",
                     start_date="2002.04.01",
                     end_date="2022.11.01", 
                     download_range = "Seasonal", # means that we only download data between 1 April and 1 November
                     spatmeth="bbox",
                     bbox=ll,
                     delete_hdf=FALSE,
                     verbose = TRUE
    )}
    
    # this takes some time, but when delete_hdf=FALSE, you don't need to rerun
    # everything next time. It starts where it was aborted. Large files, so make
    # sure you have storage. These data is 250*250m2, so four times the size
    # of the NPP (500*500), and also 14 datasets per year compared to 1 for NPP
    
    ## Next step:
    # Generate one raster per year per 16-days period, and then
    # get the maximum value per year
    
    # need a list of years and daynumbers for the 16-day period
    NDVIfiles <- list.files('data/temp/modis/ndviMod/',
                            full.names = T)
    years <- unlist(unique(lapply(NDVIfiles, function(x) substr(strsplit(x, "MOD13Q1.")[[1]][2], start=2, stop=5))))
    
    # Process each year
    peakNDVI <- lapply(years, function(year) {
      # Get file paths for the year
      yearFiles <-  list.files('data/temp/modis/VI_16Days_250m_v61/NDVI/',
                                year, full.names = T)
      
      # Load and find the maximum NDVI for the year
      yearmax <- max(rast(yearFiles), na.rm = TRUE)
      
      return(yearmax)
    }) |>
      rast() |>
      # mean over all years, the final product
      mean(na.rm = TRUE) 
    
    # # set lower NDVI limit to 0
    # peakNDVI <- peakNDVI |>
    #   clamp(lower = 0, values = FALSE)
    
    # Scale NDVI values
    finalMap <- peakNDVI  * 0.0001
    
    return(finalMap)
  }
}