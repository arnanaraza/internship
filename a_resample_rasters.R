### Script to mask, resample, stack raster covariates ###

PP.Rasters <- function (slave, master) {
  if (length(names(slave) == 4)) { #a trick to say it's Landsat
    slave1 <- resample (slave, master, method = 'ngb')
    stacked <- stack(slave1,master)
    return(stacked)
  }
  
  else {
    stacked <- stack(slave,master)
    return(stacked)
  }
  setwd(mydir)
}

