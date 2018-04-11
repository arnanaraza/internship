### Main script to call all sub-scripts ###

## Preliminaries
rm(list=ls()) 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(unixtools)
mydir <- ('/media/sarvision/InternshipFilesAraza/BiomassPhilippines') #just change if working with Windows
setwd(mydir)
unixtools::set.tempdir('/media/sarvision/InternshipFilesAraza/test/')



## Mask and mosaic* pre-processed PALSAR and Sentinel 1 from external directory 
setwd(paste0(mydir,'/scripts/'))
source('a_mosaic_rasters.R')
extdir <- 'media/sarvision' #sample external disk location working for both linux and windows
orbit061 <- S1Files('bin', '061', paste0(extdir,'/Projects/Phillippines_All/PhilippinesTotal_Stats'))
palsar <- PalsarFiles('bin',  paste0(extdir,'/Projects/Phillippines_All/PhilippinesTotalPalsar'))
landsat <- LandsatFiles('tif', 'last',  paste0(extdir,'Projects/Phillippines_All/Philippines_Landsat'))
dem <- DEMFiles('bin',  paste0(extdir,'/SRTM/SRTM_Whole_Philippines_30m'))
lcov <-LcovFiles('tif', paste0(mydir, '/data/LandCover2014'))
#### mask layer (ph_bounds) not yet in drive ####
setwd(mydir)

## Open pre-processed rasters 

#Open list of rasters
setwd(paste0(mydir,'/scripts/'))
source('a_openfiles.R')
#Open stand alone rasters - DEM and Land Cover
RasFiles <- FolderFiles ('raw', 'tif')
LcovFiles <- list.files(RasFiles[2])
setwd(RasFiles[2])
lcov <- raster(LcovFiles[2]) 

#Open "satellite rasters"
RasFiles <- FolderFiles ('intermediate', 'tif')

#Stack each main input (landsat, palsar, sentinel1)
landsat <- stack(RasFiles[[3]])
names(landsat) <- c("red" ,  "nir",   "swir1", "swir2")
palsar <- stack(RasFiles[[6]])
names(palsar) <- c('hh1', 'hv1', 'hh2', 'hv2')
s1 <- stack(RasFiles[[5]])
names(s1) <- c('min.vh', 'min.vv', 'mean.vh', 'mean.vv', 'max.vh', 'max.vv', 'sd.vh', 
               'sd.vv', 'range.vh', 'range.vv', 'variab.vh', 'variab.vv')
dem <- raster(RasFiles[1])



## Use test masks (***OPTIONAL***)
setwd(paste0(mydir,'/scripts/'))
source('zz_test_mask.R')
l.mask <- MaskForTest (landsat)
p.mask <- MaskForTest(palsar)
s.mask <- MaskForTest(s1)
d.mask <- MaskForTest(dem)
lc.mask <- MaskForTest(lcov)


## Downscale resolution of rasters (***OPTIONAL***)    
setwd(paste0(mydir,'/scripts/'))
source('a_downscale_rasters.R') 
res <- 1000
landsat.1km <- ResRas(landsat, res)
setwd(mydir)


## Resample everything (slave) using s1 (master) at 25m 
setwd(paste0(mydir,'/scripts/'))
beginCluster(n=6)
landsat <- resample(landsat, s1[[1]], method='ngb')
palsar <- resample (palsar, s1[[1]], method='ngb')
dem <- resample (dem, s1[[1]], method='ngb')
endCluster()


## Create and stack other covariates(vegetation indices, polarization ratios, slope from DEM, and NA-free land cover)
#vegetation indices, and dual-pol ratio
setwd(paste0(mydir,'/scripts/'))
source('a_VIs_ratios.R') 
l.intr <- RasProd(landsat,'landsat')
p.intr <- RasProd(palsar, 'palsar')
s.intr <- RasProd(s1, 's1')
d.intr <- RasProd(d, 'dem')
lc.intr <- RasProd(lcov, 'lcov')
setwd(mydir)

#texture
source('a_VIs_ratios.R') 
s.intr <- subset(s.intr, order(c(1,3,2,4,5,6,7,8,9,10,11,12))) #re-arrange layer so all texture source band in layer 2
r.list <- list (l.intr,p.intr,s.intr)
all.tex <- TexProd (r.list, 9, 2)


## Stack everything
beginCluster(n=6)
all.covs <- stack(l.intr, p.intr, s.intr, all.dem, d.intr, lc.intr) #add r.list check first)
writeRaster(all.covs,  'all.tif')
names(all) <- c("red" ,  "nir",   "swir1", "swir2", "ndvi",  "savi"  ,"evi",  
                'rvi', "wdvi", 'svi','hh1', 'hv1', 'hh2', 'hv2', 'hv/hh1', 'hv/hh2',
                'min.vh', 'min.vv', 'mean.vh', 'mean.vv', 'max.vh', 'max.vv', 'sd.vh', 
                'sd.vv', 'range.vh', 'range.vv', 'variab.vh', 'variab.vv','vh/vv',
                'dem', 'slope','lcov','tex.lsat','tex.pals','tex.s1')
endCluster()


## Compute biomass per plot at adapting to allometric equation 
setwd(paste0(mydir,'/scripts/'))
source('a_preprocess_nfi1.R') 
all.plots.03 <- NFI('Chave14', 03, 'no')
all.plots.14 <- NFI('Chave14', 14,'no')


## Adjust biomass according to raster input resolution
setwd(paste0(mydir,'/scripts/'))
source('a_biomass-resolution_match.R') 
td.03 <- BiomassPixel(all.plots.03, 03, res)
td.14 <- BiomassPixel(all.plots.14, 14, res)


## Open reclassified plots
td <- readOGR(dsn =paste0(mydir,'/mid-results/'), layer = "bio14a")
td <- as.data.frame(td[,c(1:2)]) #change td.mask to td.14 for national-scale test 
td <- td[1:2]


## Modeling proper
setwd(paste0(mydir,'/scripts/'))
source('b_train-test.R') 



#multiple linear regression
#random forest
#neural network

