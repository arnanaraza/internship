## Function to train points inside forests only ##

fmask <- readOGR(dsn =paste0(mydir,'/data/'), layer = 'ptd_ver1')


#clip with forest mask 
crs(fmask) <- crs(td.14.rec)
cf.mask <- raster::intersect(td.14.rec, fmask)
bubble(cf.mask, zcol='bioclass', main=paste0('biomass (t/ha)'))

