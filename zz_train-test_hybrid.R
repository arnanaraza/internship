### Script to perform cross validation
pacman::p_load(caret,raster,ranger,randomForest,maptools, velox)


ExtractInfo <- function(spdf){
  
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
  
  # Extract values


}
### TESTS
lp <- all.covs
lp <- stack(s.fin)
lp<- s.fin

ye <- td.14.rec[,c(1:2)]
ye <- wtih345[,c(1:2)]
pixel.value <- extract(lp, ye@data,df=T, buffer=250, fun=mean)
class <- td.14.rec$bioclass
class <- wtih345$bioclass
covs <- cbind(pixel.value, class) 

valuetable <- covs
colSums(is.na(valuetable))
valuetable.i <- na.omit(valuetable)

valuetable$class <- factor(valuetable$class, levels = c(1:5))
valuetable.i <- rfImpute(x=valuetable[ ,c(2:28)], y=valuetable$class)
set.seed(1234)
modelRF <- randomForest(x=valuetable.i[ ,c(2:28)], y=valuetable.i$`valuetable$class`, importance = TRUE)
modelRF


valuetable.i$class <- factor(valuetable.i$class, levels = c(1:5))
modelRF <- randomForest(x=valuetable.i[ ,c(2:28)], y=valuetable.i$class, importance = TRUE)
modelRF

impt <- as.data.frame(modelRF$importance)                                                                                                                                
impt <- impt[order(impt$MeanDecreaseAccuracy),] 
shitty1 <- rownames(impt)
shitty1 <- shitty1[1:10]
predRF <- predict(lp, modelRF, na.rm=T)
plot(predRF)

shitty2 <- c(shitty, "s.tex.res.1", "s.tex.res.2", "s.tex.res.3", "s.tex.res.4", "s.tex.res.5", "s.tex.res.6",
             "s.tex.res.7", "s.tex.res.8")
duplicated(shitty2)

valuetable.ii <- valuetable.i[ , !(names(valuetable.i) %in% shitty)]
set.seed(1234)
modelRF <- randomForest(x=valuetable.ii[ ,c(2:45)], y=valuetable.ii$`valuetable$class`, importance = TRUE)
modelRF

### REG SAMPLE

class <- with56$biomass.ton
covs1 <- cbind(pixel.value, class)
valuetable.ii <- valuetable.i[ , !(names(valuetable.i) %in% shitty)]
set.seed(1234)
covs1 <- na.omit(covs1)
modelRF.reg <- randomForest(covs1$class ~ ., data=covs1, importance = TRUE)
modelRF.reg$














### TESTS BUFFERED
set.seed(1234)
valuetable$class <- factor(valuetable$class, levels = c(1:8))
modelRF <- randomForest(x=valuetable[ ,c(2:11)], y=valuetable$class,
                        importance = TRUE)
modelRF


predRF <- predict(lp, modelRF,na.action=na.omit)

#caret LOOCV
train_control <- trainControl(method="LOOCV")
train_control1 <- trainControl(method="cv", number=10)
train_control2 <- trainControl(method = "repeatedcv", number = 5, repeats = 5, returnResamp ="all")

modelNN <- train(valuetable.i[ ,c(3:17)], valuetable.i$class, trControl=train_control1, method="nnet")
modelSVM <- train(valuetable.i[ ,c(3:17)], valuetable.i$class, trControl=train_control1, method="svmRadial")
modelNN1 <- train(valuetable.i[ ,c(2:17)], valuetable.i$class, trControl=train_control2, method="nnet", preProcess = c('center','scale'))
modelRF1 <- train(valuetable.i[ ,c(2:25)], valuetable.i$class, trControl=train_control1, method="rf",preProcess = c('center','scale'))

summary(modelNN)
caret::confusionMatrix(modelRF1)
caret::confusionMatrix(modelNN1)

names(lp)
names(valuetable.i)

predSVM <-predict(lp, modelSVM, na.rm=T)
prefNN1
####





CreateFolds <- function(dataset, fold){
  set.seed <- 1234
  folds <-createFolds(dataset, k=fold)
  return(folds)
  
}
# The k results from the folds can then be averaged to produce a single estimation.


classes <- covs$`td.14.class$bioclass`
classes
asdf <- CreateFolds(td.14.rec$bioclass,10)
asdff <- CreateFolds(covs$`td.14.class$bioclass`,10)


covs$bioclass <- factor(covs$bioclass, levels = c(1:12))
names1 <- c("red",  "nir",   "swir1", "swir2", "ndvi",  "savi"  ,"evi",  
                   'rvi', "wdvi", 'svi','hh1', 'hv1', 'hh2', 'hv2', 'hv.hh1', 'hv.hh2',
                   'min.vh',  'mean.vh', 'min.vv', 'mean.vv', 'max.vh', 'max.vv', 'sd.vh', 
                   'sd.vv', 'range.vh', 'range.vv', 'variab.vh', 'variab.vv','dem', 'slope',
                    "l.mean", "l.variance", "l.homogeneity", "l.contrast", "l.dissimilarity", "l.entropy", 
                    "l.second_moment", "l.correlation","p.mean", "p.variance", "p.homogeneity", "p.contrast", 
                    "p.dissimilarity", "p.entropy", "p.second_moment", "p.correlation","s.mean", "s.variance", 
                    "s.homogeneity", "s.contrast", "s.dissimilarity", "s.entropy", 
                    "s.second_moment", "s.correlation",'bioclass')

names(covs) <- names1
covs2 <- na.omit(covs2)
write.csv(covs1, 'potek.csv')
covs2 <- covs[,c(2,4,5,6,11,15,29,30,31,33,39,47,49,55)]
colSums(is.na(all.covs))
covs3 <- na.omit(covs3)

covs3 <- covs[,c(5,6,7,8,9,10,55)]


all.covs2 <- all.covs[[c(2,4,5,6,11,15,29,30,31,33,39,47,49,55)]]

rf <- ranger(bioclass ~ ., data=covs2, mtry=6)
rf <- randomForest(bioclass ~ ., data=covs3)


naive.pred<- predict(all.covs,rf,na.action=na.omit)

TrainFolds <- function(covs, folds){
  covs$bioclass <- factor(covs$bioclass, levels = c(1:12))
  rf <- randomForest(y=td$biomass, x=td[,-5], mtry=3, num.trees = 1000)
  #palsar.rf <- predict(palsar,rf)
  #plot(palsar.rf, main='preliminary biomass prediction, ALOS-PALSAR dual-polarizations')
  return(rf)
}
  
  
}
                     
AccuracyStats <- function(predicted, observed){
  RMSE <- sqrt(mean(unlist(predicted - observed)^2))
  MAE <- mean(abs(unlist(predicted - observed)))
  ME <- mean(unlist(predicted - observed))
  return(data.frame(RMSE, MAE, ME))
}
