### Script to create covariate set ###
pacman::p_load(raster,randomForest)

ExtractInfo <- function(spdf, nclass, buffer, fmask_only, model){
  set.seed(1234)
  # Open preprocessed rasters
  fin.files <- list.files(paste0(mydir, '/mid-results/stackable'),pattern='.tif')
  print(fin.files)
  setwd(paste0(mydir, '/mid-results/stackable'))
  l.fin <- stack(fin.files[[3]])
  p.fin <- stack(fin.files[[7]])
  p.fin <- p.fin[[c(1:4)]]
  s.fin <- stack(fin.files[[10]])
  s.fin <- s.fin[[3:4]]
  d.fin <- stack(fin.files[[1]])
  lt.fin <- stack(fin.files[[4]])
  lt.fin <- lt.fin[[c(1,2,7)]]
  pt.fin <- stack(fin.files[[8]])
  pt.fin <- pt.fin[[c(1,2,7)]]
  r1 <- raster(fin.files[[5]])
  r2 <- raster(fin.files[[13]])
  r.fin <- stack(r1,r2)
  f.fin <- raster(fin.files[[2]])
  all.covs <- stack(l.fin,p.fin,s.fin,r.fin,d.fin,lt.fin,pt.fin,f.fin)
  names <- c('red',  "nir",   "swir1", "swir2", "ndvi",  "savi"  ,"evi",  
              'rvi', "wdvi", 'svi','hh1', 'hv1', 'hh2', 'hv2', 'vv', 'vh','hhhv','svvh', 'dem', 'slope',
              "l.mean", "l.variance", "l.second_moment", "p.mean", "p.variance", "p.second_moment", 'f.mask')
  names(all.covs) <- names
  
  if (fmask_only == 'no'){
    
    # Extract values
    td <- spdf[,c(1:2)]
    print(paste0('total points are:', ' ', length(spdf)))
    
    if (buffer == 'yes'){
      #extract pixel info
      pixel.value <- extract(all.covs, td@data, df=T, buffer=250, fun=mean)
      class <- spdf$bioclass
      covs <- cbind(pixel.value, class) 
      valuetable <- covs
      
      #impute NAs
      print(colSums(is.na(valuetable)))
      valuetable$class <- factor(valuetable$class, levels = c(1:nclass))
      valuetable.i <- rfImpute(x=valuetable[ ,c(2:28)], y=valuetable$class)
      
      #name covariates properly
      names1 <- c('class', 'red',  "nir",   "swir1", "swir2", "ndvi",  "savi"  ,"evi",  
                  'rvi', "wdvi", 'svi','hh1', 'hv1', 'hh2', 'hv2', 'vv', 'vh','hhhv','svvh', 'dem', 'slope',
                  "l.mean", "l.variance", "l.second_moment", "p.mean", "p.variance", "p.second_moment", 'f.mask')
      names(valuetable.i) <- names1
     # return (valuetable.i)  
    }
    
    if (buffer == 'no'){
      #extract pixel info
      pixel.value <- extract(all.covs, td@data, df=T) #buffer=50, fun=mean)
      class <- spdf$bioclass
      covs <- cbind(pixel.value, class) 
      valuetable <- covs
      
      #impute NAs
      print(colSums(is.na(valuetable)))
      valuetable$class <- factor(valuetable$class, levels = c(1:nclass))
      valuetable.i <- rfImpute(x=valuetable[ ,c(2:28)], y=valuetable$class)
      
      #name covariates properly
      names1 <- c('class', 'red',  "nir",   "swir1", "swir2", "ndvi",  "savi"  ,"evi",  
                  'rvi', "wdvi", 'svi','hh1', 'hv1', 'hh2', 'hv2', 'vv', 'vh','hhhv','svvh', 'dem', 'slope',
                  "l.mean", "l.variance", "l.second_moment", "p.mean", "p.variance", "p.second_moment", 'f.mask')
      names(valuetable.i) <- names1
      
    #  return (valuetable.i)  
    }}
  
  if (fmask_only == 'yes'){
    # Open forest mask and intersect spdf plots
    fmask <- readOGR(dsn =paste0(mydir,'/data/'), layer = 'ptd_ver1')
    crs(fmask) <- crs(spdf)
    spdfm <- raster::intersect(spdf, fmask)
    print(paste0('points inside forest mask are:', ' ', length(spdfm)))
    
    # Extract values
    td <- spdfm[,c(1:2)]
    
    if (buffer == 'yes'){
      #extract pixel info
      pixel.value <- extract(all.covs, td@data, df=T, buffer=100, fun=mean)
      class <- spdfm$bioclass
      covs <- cbind(pixel.value, class) 
      valuetable <- covs
      
      #impute NAs
      print(colSums(is.na(valuetable)))
      valuetable$class <- factor(valuetable$class, levels = c(1:nclass))
      valuetable.i <- rfImpute(x=valuetable[ ,c(2:28)], y=valuetable$class)
      
      #name covariates properly
      names1 <- c('class', 'red',  "nir",   "swir1", "swir2", "ndvi",  "savi"  ,"evi",  
                  'rvi', "wdvi", 'svi','hh1', 'hv1', 'hh2', 'hv2', 'vv', 'vh','hhhv','svvh', 'dem', 'slope',
                  "l.mean", "l.variance", "l.second_moment", "p.mean", "p.variance", "p.second_moment", 'f.mask')
      names(valuetable.i) <- names1
      #return (valuetable.i)  
    }
    
    if (buffer == 'no'){
      #extract pixel info
      pixel.value <- extract(all.covs, td@data, df=T) #buffer=50, fun=mean)
      class <- spdfm$bioclass
      covs <- cbind(pixel.value, class) 
      valuetable <- covs
      
      #impute NAs
      print(colSums(is.na(valuetable)))
      valuetable$class <- factor(valuetable$class, levels = c(1:nclass))
      valuetable.i <- rfImpute(x=valuetable[ ,c(2:28)], y=valuetable$class)
      
      #name covariates properly
      names1 <- c('class', 'red',  "nir",   "swir1", "swir2", "ndvi",  "savi"  ,"evi",  
                  'rvi', "wdvi", 'svi','hh1', 'hv1', 'hh2', 'hv2', 'vv', 'vh','hhhv','svvh', 'dem', 'slope',
                  "l.mean", "l.variance", "l.second_moment", "p.mean", "p.variance", "p.second_moment", 'f.mask')
      names(valuetable.i) <- names1
      
      #return (valuetable.i)  
    }}
  if (model == 'all'){
    valuetable1 <- valuetable.i
    print(names(valuetable1))
    return(valuetable1)}
  
  if (model == 'ps'){
    valuetable1 <-valuetable.i[c(1,14:19,25:27)]
    print(names(valuetable1))
    return(valuetable1)}
  
  if (model == 'lp'){
    valuetable1 <-valuetable.i[c(1:15,18,25:27)]
    print(names(valuetable1))
    return(valuetable1)}
  
  if (model == 'l'){
    valuetable1 <-valuetable.i[c(1:11,22:24)]
    print(names(valuetable1))
    return(valuetable1)}
  
  if (model == 'p'){
    valuetable1 <-valuetable.i[c(1,12:14,18,25:27)]
    print(names(valuetable1))
    return(valuetable1)}
  
  if (model == 's'){
    valuetable1 <-valuetable.i[c(1,16,17,19)]
    print(names(valuetable1))
    return(valuetable1)}
}
