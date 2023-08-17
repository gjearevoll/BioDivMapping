## Data source description for environmental covariates

The data model found in the R SHiny app currently encompasses five covariates. Annual precipitation and mean temperature are from the WorldClim Version 2 at a 1 km resolution. Elevation data for Norway was derived from the 5m ArcticDEM [(Claire et al., 2022)](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/C98DVS) and the 10m Norwegian national DEM, obtained from [Mapzen terrain tiles](https://registry.opendata.aws/terrain-tiles/). Above-ground forest biomass at 30 m resolution was obtained from the SAT-SKOG dataset is produced by the Norwegian Institute for Forests and Landscape [(Gjertsen & Nilsen, 2012)](https://nibio.brage.unit.no/nibio-xmlui/handle/11250/2453917). Soil pH and soil organic carbon were obtained from the SoilGrids V 2.0 dataset at 250m resolution [(Poggio et al., 2021)](https://soil.copernicus.org/articles/7/217/2021/). Surface net solar radiation was obtained from the ECMWF's ERA5 global climate model with a 25 km resolution [(Hersbach et al., 2023)](https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=overview). Net solar radiation was averaged from four daily estimates during April-September of 2015-2020. All data were standardised to a 1 km grid, with plans for higher resolution interpolations in future models.


### Citations
Hersbach, H., Bell, B., Berrisford, P., et al. 2023. ERA5 hourly data on single levels from 1940 to present. Copernicus Climate Change Service (C3S) Climate Data Store (CDS).

Fick, S.E., & Hijmans, R.J. 2017. WorldClim 2: new 1‐km spatial resolution climate surfaces for global land areas. International Journal of Climatology, 37(12), 4302-4315.

Claire, et al. 2022. ArcticDEM – Strips, Version 4.1. Harvard Dataverse, V1. https://doi.org/10.7910/DVN/C98DVS.

Gjertsen, A.K., & Nilsen, J.E. 2012. SAT-FOREST: A forest map based on the interpretation of satellite imagery. Report from Forest and Landscape. 1-60.

Poggio, L., De Sousa, L.M., Batjes, N.H., et al. 2021. SoilGrids 2.0: producing soil information for the globe with quantified spatial uncertainty. Soil, 7(1), 217-240.
