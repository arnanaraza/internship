### Function to resample stacked raster inputs and plot biomass
if (!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal)

ResRasPlot <- function(raster, res) { #res should be divisible by 25 (i.e. 50 or 0.5ha, 100 or 1ha)
  raster <- projectRaster(raster, crs="+proj=utm +zone=51 ellps=WGS84")
  reso <- aggregate(raster, fact=(as.numeric(res)/25)) #fact is a multiplier
  raster <- resample (raster, reso, method='ngb')

}

