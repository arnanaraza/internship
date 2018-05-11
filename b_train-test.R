### Script to create train-test valuetable data per ml technique ###
pacman::p_load(caret,ranger)

TrainCovs <- function (VT, technique, CV, parallel) {
  set.seed(1234)
  
  if (parallel == 'no'){
  #define cross-validation
    if (CV == 'LOOCV') {train_control <- trainControl(method=CV)}
    if (CV == 'cv') {train_control <- trainControl(method=CV, number=10)}
    
    if (technique == 'nnet'){model <- train(VT[ ,c(2:length(VT))], VT$class, 
                                            trControl=train_control, method=technique)}
    
    if (technique == 'rf'){model <- train(VT[ ,c(2:length(VT))], VT$class, 
                                            trControl=train_control, method=technique)}
  
    if (technique == 'svmRadial'){model <- train(VT[ ,c(2:length(VT))], VT$class, 
                                            trControl=train_control, method=technique)}
  
    summary(model)
  return(model)}
  
  if (parallel == 'yes'){
    
    print ('fuck yeah, this is parallelized! hahaha')
    # can just lapply per covariate set!
    
    
  }
}