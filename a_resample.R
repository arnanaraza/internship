if (!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, parallel, foreach,doSNOW)



ResTex1 <- function(r_list, n_list, master) {
  UseCores <- detectCores() -1
  cl<- makeSOCKcluster(UseCores)
  registerDoSNOW(cl)
  
  
  # Parallel per raster 
  foreach (i=1:length(r_list), .packages=c('raster', 'foreach'))  %dopar% { 
    rasterOptions(tmpdir=file.path('/media/sarvision/InternshipFilesAraza/tempfiles'))  #sets temp directory - this is important b/c it can fill up a hard drive if you're doing a lot of polygons
    ras <- resample(r_list[[i]], master[[1]], method='ngb')
    setwd('/media/sarvision/InternshipFilesAraza/BiomassPhilippines/mid-results/stackable')
    writeRaster(ras, paste0(n_list[[i]],'.tif'), overwrite=T)
  }
  unlink(file.path('/media/sarvision/InternshipFilesAraza/tempfiles'), recursive = TRUE,force = TRUE) #delete temporary files
  stopCluster(cl)
}