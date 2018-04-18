### Script to seperately compute for GLCM texture

if (!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, doParallel, foreach, doSNOW, unixtools)
setwd(mydir)
unixtools::set.tempdir('/media/sarvision/InternshipFilesAraza/test/')

# Use cores

TexProd <- function(r_list, wdow) {

  rasterOptions(tmpdir=file.path("/media/sarvision/InternshipFilesAraza/test/"))
  
  # Parallel per raster 
  return(foreach (i=1:length(r_list), .packages=c('raster','glcm'))  %dopar% { 
    glcm(raster(r_list[[i]], layer=2),window = c(wdow,wdow), shift=list(c(1,1)), na_opt='ignore')})
  
  unlink(file.path("/media/sarvision/InternshipFilesAraza/test/"), recursive = TRUE,force = TRUE) #delete temporary files
}

stopCluster(cl)

### look for S1 and resample everything from S1 --- note that am doing resample to the last!!!

ResTex <- function(r_list) {
  
  # Use cores
  UseCores <- detectCores() -1
  cl<- makeSOCKcluster(UseCores, outfile="")
  registerDoSNOW(cl)

  # Parallel per raster 

    return(foreach (i=1:length(r_list), .packages='raster')  %dopar% { 
    resample(r_list[[i]], r_list[[3]][[1]])})  #SENTINEL 1 MASTER, EVERYTHING SLAVE -indexed at 3 already!!!
  stopCluster(cl)
}



####
# Use cores
cores <- detectCores() -1
cl <- makeCluster(cores, output="") #output should make it spit errors
registerDoParallel(cl)


TexProd1 <- function(r_list, wdow) {
  
  # Parallel per raster 
  foreach(i=1:cores, .packages= c("raster","glcm","foreach"), .combine = rbind) %dopar% {
    rasterOptions(tmpdir=file.path("/media/sarvision/InternshipFilesAraza/test/"))
    
    return(foreach (i=1:length(r_list))  %do% { 
      glcm(raster(r_list[[i]], layer=2),window = c(wdow,wdow), shift=list(c(1,1)), na_opt='ignore')})

       #delete temporary files
        unlink(file.path("/media/sarvision/InternshipFilesAraza/test/"), recursive = TRUE,force = TRUE)}
  
  error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
}
all.tex <- TexProd1 (r.list, 90)


stopCluster(cl)
