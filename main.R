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
t.list <- list(landsat,palsar,s1,dem,lcov)
n.list <- c('landsat','palsar','s1','dem','lcov')
t.mask <- MaskForTestPar1(t.list, n.list)
landsat <- t.mask[[1]]
palsar <- t.mask[[2]]
s1 <- t.mask[[3]]
dem <- t.mask[[4]]
lcov <- t.mask [[5]]
setwd(mydir)

    #texture
    intr.files <- list.files(paste0(mydir, '/temp'),pattern='.tif')
    setwd(paste0(mydir, '/temp'))
    l.intr <- stack(intr.files[[11]])
    p.intr <- stack(intr.files[[15]])
    s.intr <- stack(intr.files[[18]])
    r.list.msk <- list (l.intr,p.intr,s.intr)
    n.list.msk <- c('l_tex_mask', 'p_tex_mask', 's_tex_mask')
    setwd(paste0(mydir,'/scripts/'))
    source('a_texture1.R') 
    all.tex <- TexProd1 (r.list.msk, 3, n.list.msk)
    closeAllConnections()

    ## Downscale resolution of rasters (***OPTIONAL***)    
    setwd(paste0(mydir,'/scripts/'))
    source('a_downscale_rasters.R') 
    res <- 1000
    landsat.1km <- ResRas(landsat, res)
    setwd(mydir)

  
    ## Resample everything (slave) using s1 (master) at 25m 
    setwd(paste0(mydir,'/scripts/'))
    beginCluster(n=4)
    landsat <- resample(landsat, s1[[1]], method='ngb')
    palsar <- resample (palsar, s1[[1]], method='ngb')
    dem <- resample (dem, s1[[1]], method='ngb')
    lcov <- resample(lcov, s1[[1]], method='ngb')
    endCluster()
    

## Create and stack other covariates(vegetation indices, polarization ratios, slope from DEM, and NA-free land cover)
#vegetation indices, and dual-pol ratio
setwd(paste0(mydir,'/scripts/'))
source('a_VIs_ratios.R') 
system.time(tahoe_ndvi <- focal_hpc(x=landsat, fun=RasProd(landsat, 'landsat')))
start.time <- Sys.time()
l.intr <- RasProd(landsat,'landsat')
p.intr <- RasProd(palsar, 'palsar')
s.intr <- RasProd(s1, 's1')
d.intr <- RasProd(dem, 'dem')
lc.intr <- RasProd(lcov, 'lcov')
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
closeAllConnections()

  #write rasters
  source('zz_batch_writing.R') 
  r.list <- list(l.intr,p.intr,s.intr,d.intr)  
  each.name <- c('l.intr.msk', 'p.intr.msk', 's.intr.msk', 'd.intr.msk')
  write.intr <- WriteRas(r.list,each.name)

  #open intr rasters
  intr.files <- list.files(paste0(mydir, '/mid-results/subprod'),pattern='.tif')
  setwd(paste0(mydir, '/mid-results/subprod'))
  l.intr <- stack(intr.files[[3]])
  p.intr <- stack(intr.files[[5]])
  s.intr <- stack(intr.files[[7]])
  d.intr <- stack(intr.files[[1]])
  setwd(mydir)
  
  
  
#texture
source('a_texture1.R')
s1 <- subset(s1, order(c(1,3,2,4,5,6,7,8,9,10,11))) #re-arrange layer so all texture source band in layer 2
r.list <- list (landsat,palsar,s1)
n.list <- c('ĺ.tex', 'p.tex', 's.tex')
asdf <- TexProd1 (r.list, 3, n.list)
closeAllConnections()

#resample
all.intr <- list(l.intr, p.intr, s.intr,d.intr)
all.raw <- do.call(c, list(all.intr,all.tex)) 
start.time <- Sys.time()
all.res <- ResTex(all.raw)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
closeAllConnections()

#stack everything
all.covs <- stack(all.res)
writeRaster(all.covs,  'all.tif')
names1 <- c("red",  "nir",   "swir1", "swir2", "ndvi",  "savi"  ,"evi",  
            'rvi', "wdvi", 'svi','hh1', 'hv1', 'hh2', 'hv2', 'hv/hh1', 'hv/hh2',
            'min.vh',  'mean.vh', 'min.vv', 'mean.vv', 'max.vh', 'max.vv', 'sd.vh', 
            'sd.vv', 'range.vh', 'range.vv', 'variab.vh', 'variab.vv','dem', 'slope')
names2 <- names(all.covs[[32:length(names(all.covs))]])
allnames <- c(names1,names2)
names(all.covs) <- allnames
setwd(mydir)

setwd(paste0(mydir,'/mid-results/subprod/'))
writeRaster(all.covs, 'covs.tif')
setwd(mydir)

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

