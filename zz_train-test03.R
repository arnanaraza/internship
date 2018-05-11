### Script to perform cross validation
pacman::p_load(caret,raster,ranger,randomForest)


ExtractInfo <- function(spdf){
  
  # Open preprocessed rasters
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
  
  # Extract values


}
### TESTS
lp <- l.intr.03
lp <- all.covs
lp <- stack(l.fin,p.fin)

ye <- td.03.rec[,c(1:2)]
pixel.value <- extract(lp, ye@data,df=T) #buffer=50, fun=mean)
class <- td.03.rec$bioclass
covs <- cbind(pixel.value, class) 

valuetable <- covs
colSums(is.na(valuetable))

set.seed(1234)
valuetable.i <- rfImpute(class ~ ., valuetable[2:11])
valuetable.i <- valuetable(na.action=na.omit)
set.seed(1234)
valuetable.i$class <- factor(valuetable.i$class, levels = c(1:6))
modelRF <- randomForest(x=valuetable.i[ ,c(2:11)], y=valuetable.i$class,
                        importance = TRUE)
modelRF
predRF <- predict(lp, modelRF,na.action=na.omit)
predRF03 <- predict(lp, modelRF,na.action=na.omit)


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

modelNN <- train(valuetable.i[ ,c(3:17)], valuetable.i$class, trControl=train_control, method="nnet")
modelNN1 <- train(valuetable.i[ ,c(3:17)], valuetable.i$class, trControl=train_control2, method="nnet", preProcess = c('center','scale'))
modelRF1 <- train(valuetable.i[ ,c(2:11)], valuetable.i$class, trControl=train_control, method="rf",preProcess = c('center','scale'))

summary(modelNN)
caret::confusionMatrix(modelRF1)
caret::confusionMatrix(modelNN1)

names(lp)
names(valuetable.i)

predRF1 <-predict(lp, modelRF1)
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
