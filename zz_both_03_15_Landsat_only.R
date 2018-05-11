## Function to train using both 2003 and 2015 points for Landsat only 
pacman::p_load(raster,randomForest,ranger)

l.files <- list.files(paste0(mydir,  '/mid-results/subprod'),pattern='.tif')
print(l.files)
setwd(paste0(mydir, '/mid-results/subprod'))

l.03 <- stack(l.files[[6]])
l.14 <- stack(l.files[[8]])
all.l <- stack(l.03,l.14)

#needs separate VT then rbind!
td.03 <- td.03.rec[,c(1:2)]
pixel.value <- extract(l.03, td.03@data, df=T) #, buffer=250, fun=mean)
class <- td.03.rec$bioclass
covs.03 <- cbind(pixel.value, class) 
  
td.14 <- td.14.rec[,c(1:2)]
pixel.value <- extract(l.14, td.14@data, df=T) #, buffer=250, fun=mean)
class <- td.14.rec$bioclass
covs.14 <- cbind(pixel.value, class) 
names1 <- c('id', 'red',  "nir",   "swir1", "swir2", "ndvi",  "savi"  ,"evi",  
            'rvi', "wdvi", 'svi', 'class')

names(covs.03) <- names1
names(covs.14) <- names1

l.covs <- rbind(covs.03,covs.14)
valuetable <- l.covs

#impute NAs
print(colSums(is.na(valuetable)))
valuetable$class <- factor(valuetable$class, levels = c(1:5))
valuetable.i <- rfImpute(x=valuetable[ ,c(2:11)], y=valuetable$class)
  
#name covariates properly
names1 <- c('class', 'red',  "nir",   "swir1", "swir2", "ndvi",  "savi"  ,"evi",  
              'rvi', "wdvi", 'svi')
names(valuetable.i) <- names1

test <- ranger (valuetable.i$class ~ ., data=valuetable.i)

test$confusion.matrix
