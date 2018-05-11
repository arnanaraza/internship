fin.files <- list.files(paste0(mydir, '/mid-results/stackable'),pattern='.tif')
print(fin.files)
setwd(paste0(mydir, '/mid-results/stackable'))
l.fin <- stack(fin.files[[4]])
p.fin <- stack(fin.files[[8]])
p.fin <- p.fin[[c(1:4)]]
#s.fin <- stack(fin.files[[6]])
#s.fin <- s.fin[[3:4]]

s.files <- list.files(paste0(mydir, '/mid-results/subprod'),pattern='.tif')
setwd(paste0(mydir, '/mid-results/subprod'))
s.fin <- stack(s.files[[38]])
s.fin <- s.fin[[3:4]]

setwd(paste0(mydir, '/mid-results/stackable'))
d.fin <- stack(fin.files[[1]])
lt.fin <- stack(fin.files[[5]])
lt.fin <- lt.fin[[c(1,2,5,7)]]
pt.fin <- stack(fin.files[[9]])
pt.fin <- pt.fin[[c(1,2,5,7)]]
r1 <- raster(fin.files[[6]])
r2 <- raster(fin.files[[13]])
r.fin <- stack(r1,r2)
f.fin <- raster(fin.files[[2]])

MaskForTestPar <- function(r_list) {
  
  msk <- readOGR(dsn =paste0(mydir,'/data/'), layer = "ptd_ver1")
  
  # Use cores
  UseCores <- detectCores() -1
  cl<- makeSOCKcluster(UseCores)
  registerDoSNOW(cl)
  
  # Parallel per raster 
  rasterOptions(tmpdir=file.path(tempdir()))
  return(foreach (i=1:length(r_list), .packages='raster')  %dopar% { 
    mask(crop(r_list[[i]], extent(msk)), msk)})
  
  unlink(file.path(tempdir()), recursive = TRUE,force = TRUE) #delete temporary files
  stopCluster(cl)
  
}
all.covs <- list(l.fin,p.fin,s.fin,d.fin,lt.fin,pt.fin,r.fin,f.fin)
all.covs1 <- MaskForTestPar(all.covs)