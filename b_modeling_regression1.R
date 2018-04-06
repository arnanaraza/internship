### Script to perform linear regression model for forest inventory plot and spatial covariates ###
if (!require("pacman")) install.packages("pacman")
pacman::p_load(stats, gstat, raster,rgdal, rgeos, maptools, ranger, neuralnet)
mydir <- setwd('/home/sarvision/Desktop/BiomassPhilippines')


# Open main rasters, all should be located at 'mid-results' folder
allfiles <- list.files('/home/sarvision/Desktop/BiomassPhilippines/mid-results',pattern='.tif' )
setwd('/home/sarvision/Desktop/BiomassPhilippines/mid-results')
landsat <- stack(allfiles[[3]])
palsar <- stack(allfiles[[1]])
names(palsar) <- c('hh1', 'hv1', 'hh2', 'hv2')
setwd(mydir)


# Load and prepare training-test data
 ## load nfi at SPDF

# Open all reclassified plots
td.s <- readOGR(dsn =paste0(mydir,'/mid-results/'), layer = "bio14a")
td.samp <- as.data.frame(td.s[,c(1:2)]) #change td.mask to td.14 for national-scale test 
td.samp1 <- td.samp[1:2]


# Load covariates
LoadCov <- function(cov){
  
}

# Extract values from td locations 
#samp reg
pixel.value <- as.data.frame(extract(landsat, td.samp1))
td <- cbind(pixel.value, td.s$bio_class)
names(td) <- c('red', 'nir', 'swir', 'swir1', 'biomass')
#samp rf
pixel.value <- as.data.frame(extract(palsar, td.samp1))
td <- cbind(pixel.value, td.s$bio_class)
names(td) <- c('hh1', 'hv1', 'hh2', 'hv2', 'biomass')
td <- na.omit(td)

# Perform models
#samp mlr
mlreg <- lm(td$biomass ~ td[[1]] + td[[2]] + td[[3]] + td[[4]], data=td)
mlreg.2 <- lm(td$biomass ~ td[[4]], data=td)
palsar <- na.omit(palsar)
palsar.reg <- predict(palsar, model=mlreg, na.rm=T)

fun <- function(y) { lm(td$biomass ~ td[[1]] + td[[2]] + td[[3]] + td[[4]])}
r <- calc(palsar, fun)


#newpred <- predict(result, newdata= data.frame(dep))
#samp rf
td$biomass <- factor(td$biomass, levels = c(1:12))
rf <- randomForest(y=td$biomass, x=td[,-5], mtry=3, num.trees = 1000)
palsar.rf <- predict(palsar,rf)
plot(palsar.rf, main='preliminary biomass prediction, ALOS-PALSAR dual-polarizations')

#samp nn
td.nn <- td
td.nn$biomass <- as.numeric(td.nn$biomass)
nn <- neuralnet(td.nn$biomass ~ td.nn[[1]] + td.nn[[2]] + td.nn[[3]] + td.nn[[4]], td.nn[[5]], 9, lifesign="full", rep=10, threshold=0.05)
pred.nn <- compute(nn, td.nn[,-5])
