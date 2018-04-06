### Script to perform linear regression model for forest inventory plot and spatial covariates ###
if (!require("pacman")) install.packages("pacman")
pacman::p_load(stats, gstat, raster,rgdal, rgeos, maptools, ranger, neuralnet)
mydir <- setwd('/home/sarvision/Desktop/BiomassPhilippines')

# Load covariates
LoadCov <- function(cov){
  
}

# Open main rasters, all should be located at 'mid-results' folder
allfiles <- list.files('/home/sarvision/Desktop/BiomassPhilippines/mid-results',pattern='.tif' )
setwd('/home/sarvision/Desktop/BiomassPhilippines/mid-results')
landsat <- stack(allfiles[[3]])
palsar <- stack(allfiles[[9]])
s1 <- stack(allfiles[[5]])
names(s1) <- c('MinVH', 'MinVV', 'MeanVH', 'MeanVV', 'MaxVH', 'MaxVV', 'StdDevVH', 'StdDevVV', 
             'RangeVH', 'RangeVV', 'VariabilityVH', 'VariabilityVV')
s1.sel <- s1[[3:4]]
setwd(mydir)

setwd(paste0(mydir,'/scripts'))
source('preprocess_rasters.R')
setwd(mydir)
lp <- PP.Rasters(landsat,palsar)
lps <- PP.Rasters(lp, s1)

# Load and prepare training-test data
 ## load nfi at SPDF



# Extract values
#td.03$id <- c(1:length(td.03))
#pixels <- rasterize(td.03, field=td.03$id)
samp_ext <- readOGR(dsn = 'D:/Biomass/data/radar', layer = "sample_extent")
samp_ext <- spTransform(samp_ext, crs(proj4string(td.14)))
td.mask <- td.14[samp_ext,]


td.samp <- as.data.frame(td.14[,c(1:2)]) #change td.mask to td.14 for national-scale test 
td.samp1 <- td.samp[1:2]
#samp reg
pixel.value <- as.data.frame(extract(palsar, td.samp1))
td <- cbind(pixel.value, td.samp$biomass)
names(td) <- c('red', 'nir', 'swir', 'swir1', 'biomass')
#samp rf
pixel.value.rf <- as.data.frame(extract(palsar, td.samp1))
td <- cbind(pixel.value.rf, td.samp$biomass)
names(td) <- c('red', 'nir', 'swir', 'swir1', 'biomass')


f# Perform models
#samp mlr
mlreg <- lm(td$`td.samp$biomass` ~ td[[1]] + td[[2]] + td[[3]] + td[[4]], data=td)
mlreg.2 <- lm(td$biomass ~ td[[4]], data=td)

#newpred <- predict(result, newdata= data.frame(dep))
#samp rf
td <- na.omit(td)
rf <- ranger(td[[5]] ~ ., data=td[,-5], mtry=3, num.trees = 1000)

#samp nn
nn <- neuralnet(td$`td.samp$biomass`~ td[[1]] + td[[2]] + td[[3]] + td[[4]], td[[5]], 9, lifesign="full", rep=10, threshold=0.05)
pred.nn <- compute(nn, td[,-5])
