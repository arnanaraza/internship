### Script to mask, resample, stack raster covariates ###

PP.Rasters <- function (slave, master) {
  
  if (length(names(slave) == 4)) {
    #ph.bounds <- readOGR(dsn = 'D:/Biomass/data/general/Country', layer = "Country")
    #slave <- setExtent(slave, master)
    #slave <- crop(slave, extent(master))
    #slave <- mask (slave, master)
    slave1 <- resample (slave, master, method = 'ngb')
    stacked <- stack(slave1,master)
    plot(stacked)
    return(stacked)
  }
  
  else {
    stacked <- stack(slave,master)
    plot(stacked)
    return(stacked)
  }
}

