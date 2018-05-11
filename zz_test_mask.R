#### Script to mask rasters for preprocessing tests ####

if (!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, parallel, foreach,doSNOW)

# Open files
MaskForTest <- function(raster) {
  
  #beginCluster(n=cores)
  mask <- readOGR(dsn =paste0(mydir,'/temp/'), layer = "test_mask")
  ras.masked <- crop(raster, extent(mask))
  ras.masked <- mask(ras.masked,mask)
  
  #endCluster()
  
  return(ras.masked)
  }


MaskForTestPar <- function(r_list) {
  
  msk <- readOGR(dsn =paste0(mydir,'/data/'), layer = "ptd_ver1")
  
  # Use cores
  UseCores <- detectCores() -1
  cl<- makeSOCKcluster(UseCores)
  registerDoSNOW(cl)
  
  # Parallel per raster 
  rasterOptions(tmpdir=file.path(paste0(mydir,'/tempfiles'))) 
  return(foreach (i=1:length(r_list), .packages='raster')  %dopar% { 
    mask(crop(r_list[[i]], extent(msk)), msk)})
  
  unlink(file.path(paste0(mydir,'/tempfiles')), recursive = TRUE,force = TRUE) #delete temporary files
  stopCluster(cl)
  
}
all.covs <- list(l.fin,p.fin,d.fin,lt.fin,pt.fin,r.fin,f.fin)
all.covs1 <- MaskForTestPar(all.covs)

# Use cores
UseCores <- length(t.list)
cl<- makeSOCKcluster(UseCores)
registerDoSNOW(cl)

MaskForTestPar1 <- function(r_list, n_list) {
  
  msk <- readOGR(dsn =paste0(mydir,'/temp/'), layer = "test_mask")

  
  # Parallel per raster 
  foreach (i=1:length(r_list), .packages='raster')  %dopar% { 
    rasterOptions(tmpdir=file.path(paste0(mydir,'/tempfiles')))  #sets temp directory - this is important b/c it can fill up a hard drive if you're doing a lot of polygons
    ras <- mask(crop(r_list[[i]], extent(msk)), msk)
    setwd('/media/sarvision/InternshipFilesAraza/BiomassPhilippines/temp/allmask')
    writeRaster(ras, paste0(n_list[[i]],'.tif'), overwrite=T)
  }
  unlink(file.path(paste0(mydir,'/tempfiles')), recursive = TRUE,force = TRUE) #delete temporary files
stopCluster(cl)
  }


