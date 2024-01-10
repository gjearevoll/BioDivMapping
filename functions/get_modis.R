

library(MODISTools)
library(MODIStsp)


get_modis <- function(regionGeometry, projCRS) {
  
  
  Noreg_si <- st_transform(regionGeometry, crs="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs") #Correct
  ll <- st_bbox(Noreg_si)
  ll <- ll + c(-10000, -10000, 10000, 10000)

  
  # To get the NPP data
  products <- MODIStsp_get_prodnames() # just a list of the available MODIS products
  bands <- MODIStsp_get_prodlayers("Net_PP_GapFil_Yearly_500m (M*D17A3HGF)") # The different bands in the NPP data
  dates <- mt_dates(product = "MYD17A3HGF", lat = 42, lon = -110) # not very important, annual data all are 01.01.YYYY
  
  test <- MODIStsp(gui=FALSE,
                   out_folder="data/temp/modis",
                   out_folder_mod="data/temp/modis/mod",
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
  
  
  
  for (i in c(2002:2019,2021,2022)) {
    NPPfiles <- list.files('data/temp/modis/mod',
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