## Data source description for environmental covariates change

There are two types of data that are currently used in this model run: internal and external.

- **Internal** - These datasets have been pre-processed by ROn Togunov and are available at a 1km by 1km resolution for the whole of Norway.
They can be found in the folder https://github.com/gjearevoll/BioDivMapping/tree/main/data/external/environmentalCovariates. A description of them can be found below.
- **External** - These are datasets for which we have constructed scripts that allow a user to download them directly from the source from R,
with as limited manual work outside of the R environment required as possible. The end goal of the project is to have all environmental data 
available in this format.

### Internal data description
The data model found in the R SHiny app currently encompasses five covariates. Annual precipitation and mean temperature are from the WorldClim Version 
2 at a 1 km resolution. Elevation data for Norway was derived from the 5m ArcticDEM [(Claire et al., 2022)](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/C98DVS) 
and the 10m Norwegian national DEM, obtained from [Mapzen terrain tiles](https://registry.opendata.aws/terrain-tiles/). 
Above-ground forest biomass at 30 m resolution was obtained from the SAT-SKOG dataset is produced by the Norwegian Institute for Forests and Landscape [(Gjertsen & Nilsen, 2012)](https://nibio.brage.unit.no/nibio-xmlui/handle/11250/2453917). Soil pH and soil organic carbon were obtained from the SoilGrids V 2.0 dataset at 250m resolution [(Poggio et al., 2021)](https://soil.copernicus.org/articles/7/217/2021/). Surface net solar radiation was obtained from the ECMWF's ERA5 global climate model with a 25 km resolution [(Hersbach et al., 2023)](https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=overview). Net solar radiation was averaged from four daily estimates during April-September of 2015-2020. All data were standardised to a 1 km grid, with plans for higher resolution interpolations in future models.

### External data description

Currently there are 5 different sources of external data. The relevant scripts for downloading each source are linked below.

- [**GeoNorge**](https://github.com/gjearevoll/BioDivMapping/blob/main/functions/get_geonorge.R) - GeoNorge is currently used to provide a digital elevation model, from which we compute elevation, slope and aspect for all of Norway.
- [**WorldClim**](https://github.com/gjearevoll/BioDivMapping/blob/main/functions/get_worldclim.R) - WorldClim is currently used to provide climate data - however it should be noted that we have decided that WorldClim's 
interpolation methods are too coarse and this will be replaced by local Norwegian sources in the near future.
- [**SSB**](https://github.com/gjearevoll/BioDivMapping/blob/main/functions/get_ssb.R) - SSB (the Sental Statistikkbyrå) is used to provide data on both human and building density throughout Norway. 
- [**MODIS**](https://github.com/gjearevoll/BioDivMapping/blob/main/functions/get_modis.R) - Modis (Moderate Resolution Imaging Spectroradiometer) data provides us with data on NDVI and peak primary production. Note that the
downlaod for NDVI data is particularly large.
- [**CORINE**](https://github.com/gjearevoll/BioDivMapping/blob/main/functions/get_corine.R) - Corine is the data source we've chosen for land cover at this point. Land cover is given in categorical variables. This is the
variable that requires the most manual work.


### Citations
Hersbach, H., Bell, B., Berrisford, P., et al. 2023. ERA5 hourly data on single levels from 1940 to present. Copernicus Climate Change Service (C3S) Climate Data Store (CDS).

Fick, S.E., & Hijmans, R.J. 2017. WorldClim 2: new 1‐km spatial resolution climate surfaces for global land areas. International Journal of Climatology, 37(12), 4302-4315.

Claire, et al. 2022. ArcticDEM – Strips, Version 4.1. Harvard Dataverse, V1. https://doi.org/10.7910/DVN/C98DVS.

Gjertsen, A.K., & Nilsen, J.E. 2012. SAT-FOREST: A forest map based on the interpretation of satellite imagery. Report from Forest and Landscape. 1-60.

Poggio, L., De Sousa, L.M., Batjes, N.H., et al. 2021. SoilGrids 2.0: producing soil information for the globe with quantified spatial uncertainty. Soil, 7(1), 217-240.
