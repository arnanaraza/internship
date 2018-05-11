if (!require("pacman")) install.packages("pacman")
pacman::p_load(unixtools, raster,rgdal)
mydir <- ('/media/sarvision/InternshipFilesAraza/BiomassPhilippines') #just change if working with Windows
setwd(mydir)
dir.create(file.path('/media/sarvision/InternshipFilesAraza/tempfiles'), showWarnings = FALSE)
unixtools::set.tempdir('/media/sarvision/InternshipFilesAraza/tempfiles')


fin.files <- list.files(paste0(mydir, '/data/LandCover2014'),pattern='.tif')
setwd(paste0(mydir, '/data/LandCover2014'))
lcov <- raster(fin.files[[3]])
###

fin.files <- list.files(paste0(mydir, '/mid-results/stackable'),pattern='.tif')
setwd(paste0(mydir, '/mid-results/stackable'))
l.fin <- stack(fin.files[[3]])
p.fin <- stack(fin.files[[5]])
s.fin <- stack(fin.files[[7]])
d.fin <- stack(fin.files[[2]])
lt.fin <- stack(fin.files[[4]])
pt.fin <- stack(fin.files[[6]])
st.fin <- stack(fin.files[[9]])
all.covs <- stack(l.fin,p.fin,s.fin,d.fin,lt.fin,pt.fin,st.fin)
###


ye <- td.14.rec[,c(1:2)]
pixel.value <- extract(lcov, ye@data,df=T)#buffer=25, fun=max)
class <- td.14.rec$bioclass
covs <- cbind(pixel.value, class) 
covs <- subset(covs, covs$PhilippinesFinal_333.332_other_colors_ful == 1 | covs$PhilippinesFinal_333.332_other_colors_ful == 2 | covs$PhilippinesFinal_333.332_other_colors_ful == 3)
covs$PhilippinesFinal_333.332_other_colors_ful[covs$PhilippinesFinal_333.332_other_colors_ful == 1] <- 33
covs$PhilippinesFinal_333.332_other_colors_ful[covs$PhilippinesFinal_333.332_other_colors_ful == 2] <- 22
covs$PhilippinesFinal_333.332_other_colors_ful[covs$PhilippinesFinal_333.332_other_colors_ful == 3] <- 11
covs$PhilippinesFinal_333.332_other_colors_ful[covs$PhilippinesFinal_333.332_other_colors_ful == 33] <- 3
covs$PhilippinesFinal_333.332_other_colors_ful[covs$PhilippinesFinal_333.332_other_colors_ful == 22] <- 2
covs$PhilippinesFinal_333.332_other_colors_ful[covs$PhilippinesFinal_333.332_other_colors_ful == 11] <- 1

###
valuetable <- covs
valuetable$class <- factor(valuetable$class, levels = c(1:3))
covs <- covs[,c(2,3)]
modelRF <- randomForest(covs$class ~ ., covs,
                        importance = TRUE)
modelRF
predRF <- predict(lp, modelRF,na.action=na.omit)

###
hmm <- lm (covs$class ~ covs$PhilippinesFinal_333.332_other_colors_ful)
summary(hmm)
roh <- predict(p.fin, hmm, na.rm=T)
hist(a)

setwd('/media/sarvision/InternshipFilesAraza/BiomassPhilippines/mid-results/test_predictions')
writeRaster(oh, 'pred_pals_RF.tif')
setwd(mydir)
