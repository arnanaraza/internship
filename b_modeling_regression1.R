### Script to perform linear regression model for forest inventory plot and spatial covariates ###
if (!require("pacman")) install.packages("pacman")
pacman::p_load(stats, gstat, raster,rgdal, rgeos, maptools, ranger, neuralnet)
mydir <- setwd('/home/sarvision/Desktop/BiomassPhilippines')


# Load covariates
LoadCov <- function(cov){
  
  
}

# Extract values from td locations 
#samp reg
pixel.value <- as.data.frame(extract(all.covs, td))
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
