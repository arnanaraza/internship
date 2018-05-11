rint(fmask[fmask$OBJECTID == 6,])

fmask <- readOGR(dsn =paste0(mydir,'/data/'), layer = 'ptd_ver1')


#clip with forest mask 
crs(fmask) <- crs(td.14.rec)
cf <- fmask[fmask$AGG14 == 'Closed Forest',]
of <- fmask[fmask$AGG14 == 'Open Forest',]

cf.mask <- raster::intersect(td.14.rec, cf)
bubble(cf.mask, zcol='bioclass', main=paste0('biomass (t/ha)'))
of.mask <- raster::intersect(td.14.rec, of)
bubble(of.mask, zcol='bioclass', main=paste0('biomass (t/ha)'))


#get statistics per class-fores type
td.14.df <- as.data.frame(td.14.mask)
td.14.cf <- subset(td.14.df, td.14.df$AGG14 ==  'Closed Forest')
summary(td.14.cf$biomass.ton)
td.14.of <- subset(td.14.df, td.14.df$AGG14 ==  'Open Forest')
summary(td.14.of$biomass.ton)

#get number of points per plot
ptd.cf <- spsample(cf,n=200,type="random")
ptd.cf <- SpatialPointsDataFrame(ptd.cf, td.14.rec@data[c(1:200),])
ptd.cf@proj4string <- td.14.rec@proj4string
ptd.cf$long <- ptd.cf@coords[,1]
ptd.cf$lat <- ptd.cf@coords[,2]
ptd.cf@bbox <- td.14.rec@bbox
sc <- td.14.rec@coords[,c(1,2)]
td.14.rec@coords <- ptd.cf@coords
td.14.rec@coords <- sc
ptd.cf$bioclass <- 6
with6 <- rbind(td.14.rec,ptd.cf)


#get number of points per plot
ptd.of <- spsample(of,n=200,type="random")
ptd.of <- SpatialPointsDataFrame(ptd.of, td.14.rec@data[c(1:200),])
ptd.of@proj4string <- td.14.rec@proj4string
ptd.of$long <- ptd.of@coords[,1]
ptd.of$lat <- ptd.of@coords[,2]
ptd.of@bbox <- td.14.rec@bbox
sc <- with6@coords[,c(1,2)]
with6@coords <- ptd.of@coords
with6@coords <- sc
ptd.of$bioclass <- 5
with56 <- rbind(with6,ptd.of)


source("http://bioconductor.org/biocLite.R")
biocLite("CopywriteR")

writeOGR(obj=asdff, dsn=paste0(mydir,'/temp'), layer="torn5", driver="ESRI Shapefile") # this is in geographical projection
writeOGR(obj=td.14.rec, dsn=paste0(mydir,'/temp'), layer="torn1", driver="ESRI Shapefile") # this is in geographical projection
writeOGR(obj=hybrid, dsn=paste0(mydir,'/temp'), layer="with56", driver="ESRI Shapefile", overwrite=T) # this is in geographical projection
