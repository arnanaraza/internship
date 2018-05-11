
# Compute biomass pixels at covariates resolution and write it
BiomassPixel <- function(spdf, str, resolution) {
  spdf$biomass.fin <- spdf$biomass.total * ((resolution*resolution)/(250*20)) #250*20 is the survey plot size = tons/625m2
  spdf$biomass.ton <- (spdf$biomass.fin/1000)*16
  print(summary(spdf$biomass.ton))
  if (str == 03){write.csv(spdf, paste0(mydir,'/mid-results/td03',resolution,'.csv'))
    bubble(spdf, zcol='biomass.ton', main=paste0('biomass (t/ha)', str))
    writeOGR(obj=spdf, dsn=paste0(mydir,'/mid-results'), driver="ESRI Shapefile", layer='allplots_03fin', overwrite_layer = T)}
  if (str == 14){write.csv(spdf, paste0(mydir,'/mid-results/td14',resolution,'.csv'))  
    bubble(spdf, zcol='biomass.ton', main=paste0('biomass (t/ha)', str))
    writeOGR(obj=spdf, dsn=paste0(mydir,'/mid-results'), driver="ESRI Shapefile", layer='allplots_14fin', overwrite_layer = T)}
 
  
  
  return(spdf)
  
}


# Reclassify biomass values into 12 classes using quantile classification
## done using "reclassify" tool of ArcMap, 