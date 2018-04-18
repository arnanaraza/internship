#### Script to mask rasters for preprocessing tests ####

if (!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, parallel, foreach,doSNOW)

# Open files
MaskForTest <- function(raster) {
  
  #beginCluster(n=cores)
  mask <- readOGR(dsn =paste0(mydir,'/temp/'), layer = "ldb")
  ras.masked <- crop(raster, extent(mask))
  ras.masked <- mask(ras.masked,mask)
  
  #endCluster()
  
  return(ras.masked)
  }


MaskForTestPar <- function(r_list) {
  
  msk <- readOGR(dsn =paste0(mydir,'/temp/'), layer = "ldb")
  
  # Use cores
  UseCores <- detectCores() -1
  cl<- makeSOCKcluster(UseCores)
  registerDoSNOW(cl)

  # Parallel per raster 
  return(foreach (i=1:length(r_list), .packages='raster')  %dopar% { 
    mask(crop(r_list[[i]], extent(msk)), msk)})
  
  stopCluster(cl)
  
}

# Use cores
UseCores <- length(t.list)
cl<- makeSOCKcluster(UseCores)
registerDoSNOW(cl)

MaskForTestPar1 <- function(r_list, n_list) {
  
  msk <- readOGR(dsn =paste0(mydir,'/temp/'), layer = "ldb")

  
  # Parallel per raster 
  foreach (i=1:length(r_list), .packages='raster')  %dopar% { 
    rasterOptions(tmpdir=file.path("/media/sarvision/InternshipFilesAraza/test/"))  #sets temp directory - this is important b/c it can fill up a hard drive if you're doing a lot of polygons
    ras <- mask(crop(r_list[[i]], extent(msk)), msk)
    setwd('/media/sarvision/InternshipFilesAraza/BiomassPhilippines/temp')
    writeRaster(ras, paste0(n_list[[i]],'.tif'), overwrite=T)
  }
  unlink(file.path("/media/sarvision/InternshipFilesAraza/test/"), recursive = TRUE,force = TRUE) #delete temporary files
stopCluster(cl)
  }


