# Supprot vector machine

SVMFit <- function() {
  
  modelSVM = e1071:: svm(Spam ~ ., train, kernel='radial', 
                         cost = 1e2, scale = FALSE)  

  # corss-validation to choose best parameters
  # set.seed(123)
  
  # SVM.cv = e1071:: tune('svm' , Spam ~ ., data = train, kernel='radial',
  #                      ranges = list(gamma = c(0.001, 0.001, 0.1, 1, 2, 5, 10), 
  #                                    cost = c(1, 10, 100, 1000)) )
  
  # SVM.cv$best.parameters
  
  # With the best parameters
  modelSVM.cv = e1071:: svm(Spam ~ ., data = train, kernel='radial', 
                            cost = 10, 
                            gamma = 0.001, scale = FALSE) 
  
  return(modelSVM.cv)
}


SVMEval <- function(Model, Data) {
  
  predModel = predict(Model, newdata=Data, decision.values = TRUE)
  predSpam = predModel
  
  #Error Rate
  print('Error Rate: ')
  print(mean(Data$Spam != predSpam))
  
  # Compute AUC  
  predROCR = ROCR::prediction(attributes(predModel)$decision.values, Data$Spam)
  print("Area Under Curve (AUC): ")
  print(unlist(ROCR::performance(predROCR, "auc")@y.values))
}

SVMROC <- function(Model, Data) {
  
  predModel = predict(Model, newdata=Data, decision.values = TRUE)
  predROCR = ROCR::prediction(attributes(predModel)$decision.values, Data$Spam)
  
  perf = ROCR:: performance(predROCR, "tpr", "fpr")
  
  ROC = cbind.data.frame(FP=unlist(perf@x.values), 
                         TP=unlist(perf@y.values), 
                         Threshold = unlist(perf@alpha.values))
  
  return(ROC)
}
