### Main script to call all sub-scripts ###

## Preliminaries
rm(list=ls()) 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(unixtools)
mydir <- ('/media/sarvision/InternshipFilesAraza/BiomassPhilippines') #just change if working with Windows
setwd(mydir)
dir.create(file.path('/media/sarvision/InternshipFilesAraza/tempfiles'), showWarnings = FALSE)
unixtools::set.tempdir('/media/sarvision/InternshipFilesAraza/tempfiles')


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
landsat03 <- stack(RasFiles[[2]])
names(landsat) <- c("red" ,  "nir",   "swir1", "swir2")
names(landsat03) <- c("red" ,  "nir",   "swir1", "swir2")

palsar <- stack(RasFiles[[6]])
names(palsar) <- c('hh1', 'hv1', 'hh2', 'hv2')
s1 <- stack(RasFiles[[5]])
names(s1) <- c('min.vh', 'min.vv', 'mean.vh', 'mean.vv', 'max.vh', 'max.vv', 'sd.vh', 
               'sd.vv', 'range.vh', 'range.vv', 'variab.vh', 'variab.vv')
dem <- raster(RasFiles[1])
  
  
    ## Use test masks (***OPTIONAL***)
    setwd(paste0(mydir,'/scripts/'))
    t.list <- list(landsat,palsar,s1,dem,lcov)
    n.list <- c('l.mask','p.mask','s.mask','d.mask','lc.mask')
    source('zz_test_mask.R')
    start.time <- Sys.time()
    t.mask <- MaskForTestPar1(t.list, n.list)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken
    
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
    res <- 25
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
l.intr.03 <- RasProd(landsat03,'landsat')
setwd(paste0(mydir, '/mid-results/subprod'))
writeRaster(l.intr.03, 'l.intr03.tif')
setwd(mydir)

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
  
  
#texture
source('a_texture1.R')
s1 <- subset(s1, order(c(1,3,2,4,5,6,7,8,9,10,11))) #re-arrange layer so all texture source band in layer 2
r.list <- list (landsat,palsar,s1)
n.list <- c('ĺ.tex', 'p.tex', 's.tex')
asdf <- TexProd1 (r.list, 3, n.list)
closeAllConnections()
  
#open intr rasters
library(raster, rgdal, rgeos)
intr.files <- list.files(paste0(mydir, '/mid-results/subprod'),pattern='.tif')
setwd(paste0(mydir, '/mid-results/subprod'))
l.intr <- stack(intr.files[[5]])
p.intr <- stack(intr.files[[12]])
s.intr <- stack(intr.files[[19]])
d.intr <- stack(intr.files[[1]])
l.tex <- stack(intr.files[[10]])
p.tex <- stack(intr.files[[17]])
s.tex <- stack(intr.files[[24]])

tex.names <- c("mean", "variance", "homogeneity", "contrast", "dissimilarity", "entropy", 
                 "second_moment", "correlation")
names(l.tex) <- tex.names
names(p.tex) <- tex.names
names(s.tex) <- tex.names
setwd(mydir)
  
  
    ## Use test masks (***OPTIONAL***)
    setwd(paste0(mydir,'/scripts/'))
    t.list <- list(l.intr,p.intr,s.intr,d.intr,l.tex,p.tex,s.tex)
    n.list <- c('l.intr','p.intr','s.intr','d.intr','l.tex','p.tex','s.tex')
    source('zz_test_mask.R')
    start.time <- Sys.time()
    all.mask <- MaskForTestPar1(t.list, n.list)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken
    setwd(mydir)

    #open masked rasters
    library(raster)
    intr.files.mask <- list.files(paste0(mydir, '/temp/allmask'),pattern='.tif')
    setwd(paste0(mydir, '/temp/allmask'))
    l.intr.mask <- stack(intr.files.mask[[2]])
    p.intr.mask <- stack(intr.files.mask[[4]])
    s.intr.mask <- stack(intr.files.mask[[6]])
    d.intr.mask <- stack(intr.files.mask[[1]])
    l.tex.mask <- stack(intr.files.mask[[3]])
    p.tex.mask <- stack(intr.files.mask[[5]])
    s.tex.mask <- stack(intr.files.mask[[7]])

    #resample masked samples if stackable
    setwd(paste0(mydir,'/scripts/'))
    source('a_resample.R')
    all.raw.mask <- list(l.intr.mask, p.intr.mask,s.intr.mask,d.intr.mask,l.tex.mask,p.tex.mask,s.tex.mask)
    all.raw.names <- c('l.intr.mask', 'p.intr.mask','s.intr.mask','d.intr.mask','l.tex.mask','p.tex.mask','s.tex.mask')
    start.time <- Sys.time()
    all.res <- ResTex1(all.raw.mask,all.raw.names)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken
    closeAllConnections()

    
    
    #open resampled rasters
    library(raster)
    intr.files.mask <- list.files(paste0(mydir, '/mid-results/stackable'),pattern='.tif')
    setwd(paste0(mydir, '/mid-results/stackable'))
    l.intr.mask <- stack(intr.files.mask[[2]])
    p.intr.mask <- stack(intr.files.mask[[4]])
    s.intr.mask <- stack(intr.files.mask[[6]])
    d.intr.mask <- stack(intr.files.mask[[1]])
    l.tex.mask <- stack(intr.files.mask[[3]])
    p.tex.mask <- stack(intr.files.mask[[5]])
    s.tex.mask <- stack(intr.files.mask[[7]])
    
    test.stack <- stack(l.intr.mask, p.intr.mask,s.intr.mask,d.intr.mask,l.tex.mask,p.tex.mask,s.tex.mask)
    
#resample
setwd(paste0(mydir,'/scripts/'))
source('a_resample.R')
all.intr <- list(l.intr, p.intr, s.intr,d.intr)
all.tex <- list(l.tex,p.tex,s.tex)
all.raw <- do.call(c, list(all.intr,all.tex)) 
n.list <- c('l.intr.res','p.intr.res','s.intr.res','d.intr.res','l.tex.res','p.tex.res','s.tex.res')

fin.files <- list.files(paste0(mydir, '/temp'),pattern='.tif')
setwd(paste0(mydir, '/temp'))
f.fin <- raster(fin.files[[10]])
f.finc <- crop(f.fin,extent(l.fin[[1]]))
f.finm <- mask(f.finc, l.fin[[1]])
f.finr <- resample(f.finc, l.fin[[1]])
setwd(paste0(mydir, '/mid-results/stackable'))
writeRaster(f.finr,'fmask2.tif',overwrite=T)

fin.files <- list.files(paste0(mydir, '/mid-results/subprod'),pattern='.tif')
setwd(paste0(mydir, '/mid-results/subprod'))
s.fin <- stack(fin.files[[38]])
s.fin <- s.fin[[3:4]]

  #palsar separate
  pals.res <- list(p.intr,p.tex,s.tex)
  pals.name <- c('p.intr.res', 'p.tex.res', 's.tex.res1')
  pals.res <- ResTex1(pals.res,pals.name, s.intr)
  
start.time <- Sys.time()
all.res <- ResTex1(all.raw, n.list)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
closeAllConnections()

#open final rasters
library(raster)
final.files <- list.files(paste0(mydir, '/mid-results/stackable'),pattern='.tif')
setwd(paste0(mydir, '/mid-results/stackable'))
l.fin <- stack(final.files[[3]])
#p.fin <- stack(final.files[[12]])
s.fin <- stack(final.files[[5]])
d.fin <- stack(final.files[[2]])
lt.fin <- stack(final.files[[4]])
#pt.fin <- stack(final.files[[5]])
st.fin <- stack(final.files[[7]])

#stack everything
all.files <- list(l.fin,s.fin,d.fin,lt.fin,st.fin)
all.covs <- stack(all.files)
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
res <- 25
source('a_biomass-resolution_match.R') 
td.03 <- BiomassPixel(all.plots.03, 03, res)
td.14 <- BiomassPixel(all.plots.14, 14, res)


## Open reclassified plots
setwd(paste0(mydir,'/scripts/'))
source('a_reclassify_biomass.R') 
td.03.rec <- RecPlot (td.03, 5, 'quantile')
bubble(td.03.rec, zcol='bioclass', main=paste0('biomass (t/ha)'))
td.14.rec <- RecPlot (td.14, 5, 'quantile') #choice of 5, 8, 12, 15, 20
bubble(td.14.rec, zcol='bioclass', main=paste0('biomass (t/ha)'))

  #remove erroneous plots
  setwd(paste0(mydir,'/scripts/'))
  source('a_remove_erroneous.R') 
  td.14.noerr <- NoErr (td.14)
  #reclassify 
  td.14.rec <- RecPlot(td.14.noerr, 4, 'quantile') #choice of 5, 8, 12, 15, 20

  #create pseudo TD ***OPTIONAL***
  setwd(paste0(mydir,'/scripts/'))
  source('b_pseudoTD.R')
  wtih345 <- PTD(td.14.rec, 5, 'ptd_ver1', 50)
  

## Assemble covariates  
setwd(paste0(mydir,'/scripts/'))
source('b_covs_assembly.R') 
vt.14 <- ExtractInfo(td.14.rec, 4, 'yes', 'yes', 'all') # arguments are: spdf, classes, buffer?, fmask?, covs
  
  #create VT per covs selection
  vt.ps <- ExtractInfo(td.14.rec, 4, 'yes', 'yes', 'ps')
  vt.lp <- ExtractInfo(td.14.rec, 4, 'yes', 'yes', 'lp')
  vt.l <- ExtractInfo(td.14.rec, 4, 'yes', 'yes', 'l')
  vt.p <- ExtractInfo(td.14.rec, 4, 'yes', 'yes', 'p')
  vt.s <- ExtractInfo(td.14.rec, 4, 'yes', 'yes', 's')

  
  
  
## Train valuetable
setwd(paste0(mydir,'/scripts/'))
source('b_train-test.R') 

  #get best model 
  models <- c('rf', 'nnet', 'svmRadial') 
  all.ml <- lapply (models, function(x) TrainCovs(vt.14, x, 'cv', 'no'))  # arguments are: VT, model, cross-validation, parallel?
  all.ml
  names(all.covs) <- names(vt.14[-1])
  all.pred <- lapply (all.ml, function(x) predict(all.covs, x, na.rm=T))
  setwd('/media/sarvision/InternshipFilesAraza/BiomassPhilippines/mid-results/test_predictions')
  lapply (all.pred, function(x) writeRaster(x, paste0('pred5_q_50_', x ,'.tif')))
  setwd(mydir)
  

## Train per covs input
  
  
  
  rf.ps <- TrainCovs(vt.ps, 'rf', 'LOOCV', 'no')
  rf.lp <- TrainCovs(vt.lp, 'rf', 'LOOCV', 'no')
  rf.l <- TrainCovs(vt.l, 'rf', 'LOOCV', 'no')
  rf.p <- TrainCovs(vt.p, 'rf', 'LOOCV', 'no')
  rf.s <- TrainCovs(vt.s, 'rf', 'LOOCV', 'no')
  rf.s <- TrainCovs(vt.s, 'rf', 'cv', 'no')
  