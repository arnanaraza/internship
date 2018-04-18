if (!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, parallel, foreach,doSNOW)


TexProd1 <- function(r_list, wdow, n_list) {
  UseCores <- length(n_list)
  cl<- makeSOCKcluster(UseCores)
  registerDoSNOW(cl)

  
  # Parallel per raster 
  foreach (i=1:length(r_list), .packages=c('raster','glcm', 'foreach'))  %dopar% { 
    rasterOptions(tmpdir=file.path("/media/sarvision/InternshipFilesAraza/test/"))  #sets temp directory - this is important b/c it can fill up a hard drive if you're doing a lot of polygons
    ras <- glcm(raster(r_list[[i]], layer=2),window = c(wdow,wdow), shift=list(c(1,1), c(-1,-1)), na_opt='ignore')
    setwd('/media/sarvision/InternshipFilesAraza/BiomassPhilippines/mid-results/subprod')
    writeRaster(ras, paste0(n_list[[i]],'.tif'), overwrite=T)
  }
  #unlink(file.path("/media/sarvision/InternshipFilesAraza/test/"), recursive = TRUE,force = TRUE) #delete temporary files
  stopCluster(cl)
}