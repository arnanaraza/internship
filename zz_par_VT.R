#### Script to parallelize VT assembly  ####

if (!require("pacman")) install.packages("pacman")
pacman::p_load(raster, parallel,plyr, doSNOW)

ParVT <- function(vt_list) {
  setwd(paste0(mydir,'/scripts/'))
  source('b_covs_assembly.R') 
  vt_list <- c('all', 'ps', 'lp', 'l','p','s')
  
  # Use cores
  UseCores <- detectCores() -1
  cl<- makeSOCKcluster(UseCores)
  registerDoSNOW(cl)
  
  # Parallel per raster 
  rasterOptions(tmpdir=file.path(paste0(mydir,'/tempfiles'))) 
  return(foreach (i=1:length(vt_list), .packages='raster')  %dopar% { 
    ExtractInfo(td.14.rec, 4, 'yes', 'yes', 'all')})
  unlink(file.path(paste0(mydir,'/tempfiles')), recursive = TRUE,force = TRUE) #delete temporary files
  stopCluster(cl)
  
}

setwd(paste0(mydir,'/scripts/'))
source('b_covs_assembly.R') 
vt_list <- c('all', 'ps', 'lp', 'l','p','s')
start.time <- Sys.time()
all.VT <- mclapply(vt_list, function(x) ExtractInfo(td.14.rec, 4, 'yes', 'yes', x), mc.preschedule = TRUE, mc.set.seed = TRUE,
                   mc.silent = FALSE, mc.cores = getOption("cores",6), mc.cleanup = TRUE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


# Train
setwd(paste0(mydir,'/scripts/'))
source('b_train-test.R') 

  #train per ml method, get best ml
  models <- c('rf', 'nnet', 'svmRadial') 
  all.ml <- mclapply(1:length(models), function(x) TrainCovs(all.VT[[1]], as.character(models[[x]]), 'cv', 'no'),
                      mc.preschedule = TRUE, mc.set.seed = TRUE,
                      mc.silent = FALSE, mc.cores = getOption("cores",6), mc.cleanup = TRUE)  
                      # arguments are: VT, model, cross-validation, parallel?
  
  #train per covs
  all.c <- mclapply (all.VT, function(x) TrainCovs(x, 'rf', 'cv', 'no'),
                      mc.preschedule = TRUE, mc.set.seed = TRUE,
                      mc.silent = FALSE, mc.cores = getOption("cores",6), mc.cleanup = TRUE)  
                      # arguments are: VT, model, cross-validation, parallel?

  
# Save accuracy
setwd(paste0(mydir,'/final/'))
w <- function(m,x,y){
  if (m == 'm'){write.csv((all.ml[[x]]$results), file=paste0(all.ml[[x]]$method,'_', y, '_', Sys.time(), '.csv'))}
  if (m == 'c'){write.csv((all.c[[x]]$results), file=paste0(all.c[[x]]$method,'_', y, '_', Sys.time(), '.csv'))}}
mapply(w, m='m', x=1:length(all.ml), y=models)
mapply(w, m='c', x=1:length(all.c), y=vt_list)
setwd(mydir)


# Predict 

  #predict per ml
  names(all.covs) <- names(all.VT[[1]][-1])
  all.pred.ml <- mclapply(1:length(all.ml), function(x) predict(all.covs, all.ml[[x]], na.rm=T),
                       mc.preschedule = TRUE, mc.set.seed = TRUE,
                       mc.silent = FALSE, mc.cores = getOption("cores",6), mc.cleanup = TRUE) 
  
  #predict per covs
  idx.c <- lapply(1:length(all.VT), function(x) which(names(all.covs) %in% names(all.VT[[x]])))
  r <- function(x,y){
    p <- predict(all.covs[[x]], y, na.rm=T)
    return(p)}
  
  all.pred.c <- mcmapply(r, x=idx.c, y=all.c,
                          mc.preschedule = TRUE, mc.set.seed = TRUE,
                          mc.silent = FALSE, mc.cores = getOption("cores",6), mc.cleanup = TRUE) 


# Write 
unixtools::set.tempdir('/media/sarvision/InternshipFilesAraza/tempfiles')
  
setwd(paste0(mydir,'/final/'))
wp <- function(x,y){
  writeRaster(x, file=paste0(y, '_', Sys.time(), '.csv'))
}
mcmapply(wp, x=all.pred.c, y=models,
         mc.preschedule = TRUE, mc.set.seed = TRUE,
         mc.silent = FALSE, mc.cores = getOption("cores",6), mc.cleanup = TRUE)

mcmapply(wp, x=all.pred.ml, y=vt_list,
         mc.preschedule = TRUE, mc.set.seed = TRUE,
         mc.silent = FALSE, mc.cores = getOption("cores",6), mc.cleanup = TRUE)

setwd(mydir)



