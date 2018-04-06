### Script to divide plots into training and testing 
pacman::p_load(caret, ranger, randomForest, neuralnet)
## Open reclassified plots
td <- readOGR(dsn =paste0(mydir,'/mid-results/'), layer = "bio14a")
td <- as.data.frame(td[,c(1:2)]) #change td.mask to td.14 for national-scale test 
td <- td[1:2]


CreateFolds <- function(dataset, fold){
  set.seed <- 1234
  folds <-createFolds(dataset, k=fold)
  return(folds)
  
}








AccuracyStats <- function(predicted, observed){
  RMSE <- sqrt(mean(unlist(predicted - observed)^2))
  MAE <- mean(abs(unlist(predicted - observed)))
  ME <- mean(unlist(predicted - observed))
  return(data.frame(RMSE, MAE, ME))
}
