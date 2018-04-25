### Script to seperately compute for GLCM texture

if (!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, doParallel, foreach, doSNOW, unixtools)

ResTex <- function(r_list) {
  
  # Use cores
  UseCores <- detectCores() -1
  cl<- makeSOCKcluster(UseCores, outfile="")
  registerDoSNOW(cl)
  rasterOptions(tmpdir=file.path(paste0(mydir,'/tempfiles')))  #sets temp directory - this is important b/c it can fill up a hard drive if you're doing a lot of polygons
  
  # Parallel per raster 

    return(foreach (i=1:length(r_list), .packages='raster')  %dopar% { 
    resample(r_list[[i]], r_list[[3]][[1]])})  #SENTINEL 1 MASTER, EVERYTHING SLAVE -indexed at 3 already!!!
  
  unlink(file.path(paste0(mydir,'/tempfiles')), recursive = TRUE,force = TRUE) #delete temporary files
  stopCluster(cl)
}


