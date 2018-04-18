if (!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, parallel, foreach, doSNOW, unixtools)
setwd(mydir)
unixtools::set.tempdir('/media/sarvision/InternshipFilesAraza/test/')

setwd(paste0(mydir,'/temp'))
WriteRas <- function(r_list, each_name) {
  
  # Use cores
  UseCores <- detectCores() -1
  cl<- makeSOCKcluster(UseCores, outfile="")
  registerDoSNOW(cl)
  
  # Parallel per raster 
  
  
  return(foreach (i=1:length(r_list), .packages='raster')  %dopar% { 
    writeRaster(r_list[[i]], paste0(each_name[[i]], '.tif'))})
  stopCluster(cl)
}