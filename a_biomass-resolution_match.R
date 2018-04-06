
# Compute biomass pixels at covariates resolution and write it
BiomassPixel <- function(spdf, str, resolution) {
  spdf$biomass.fin <- spdf$biomass * ((resolution*resolution)/(250*20)) #250*20 is the survey plot size
  spdf$biomass.ton <- spdf$biomass.fin/1000
  print(summary(spdf$biomass.ton))
  if (str == 03){write.csv(spdf, paste0(mydir,'/mid-results/td03',resolution,'.csv'))}
  if (str == 14){write.csv(spdf, paste0(mydir,'/mid-results/td14',resolution,'.csv'))}
  bubble(spdf, zcol='biomass.ton', main=paste0('biomass (t/ha)', str))
  return(spdf)
  
}


# Reclassify biomass values into 12 classes using quantile classification
## done using "reclassify" tool of ArcMap, will figure out how to do it here