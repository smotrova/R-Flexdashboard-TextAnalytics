# Random Forest

RandomForestFit<- function() {
  
  set.seed(123)
  modelRF = randomForest:: randomForest(Spam~., data = train, 
                                        ntree = 100, importance = TRUE) 
  return(modelRF)
}

RandomForestEval <- function(Model, Data) {
  
  predModel = predict(Model, newdata=Data, type='prob' )
  predSpam = ifelse(predModel[ ,2] > 0.5, 1, 0)
  
  #Error Rate
  print('Error Rate: ')
  print(mean(Data$Spam != predSpam))
  
  # Compute AUC  
  predROCR = ROCR::prediction(predModel[ ,2], Data$Spam)
  print("Area Under Curve (AUC): ")
  print(unlist(ROCR::performance(predROCR, "auc")@y.values))
}


RandomForestROC <- function(Model, Data) {
  
  predModel = predict(Model, newdata=Data, type='prob' )
  predROCR = ROCR::prediction(predModel[ ,2], Data$Spam)
  
  perf = ROCR:: performance(predROCR, "tpr", "fpr")
  
  ROC = cbind.data.frame(FP=unlist(perf@x.values), 
                         TP=unlist(perf@y.values), 
                         Threshold = unlist(perf@alpha.values))
  
  return(ROC)
}


#----------------------------------------------------------------------------------
# Check functions
# Random Forest

# emails = read.csv("./Data/emails.csv", stringsAsFactors = F)

# source('TermDocumentMatrix.R')
# EmailsDF = TextDataFrame(TermMatrix(emails$text))
# EmailsDF$Spam = as.factor(emails$spam)

# Split the data
# set.seed(123)
# Spl = caTools::sample.split(EmailsDF$Spam, SplitRatio = 0.7)

# source('TrainTestDataset.R')
# train = Train(EmailsDF, Spl)
# test = Test(EmailsDF, Spl)

# RF = RandomForestFit()

# RandomForestEval(RF, train)
# RandomForestEval(RF, test)

