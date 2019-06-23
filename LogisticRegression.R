# Logistic Regression

LogisticRegressionFit <- function(Data) {
  # build a logistic regression
  model = glm(Spam ~., data = Data, family = 'binomial')
  return(model)
}

LogisticRegressionEval <- function(Model, Data) {
  predModel = predict(Model, newdata=Data, type = 'response' )
  predSpam = ifelse(predModel > 0.5, 1, 0)
  
  #Error Rate
  print('Error Rate:')
  print(mean(Data$Spam != predSpam))
  
  # Compute AUC  
  predROCR = ROCR::prediction(predModel, Data$Spam)
  print("Area Under Curve (AUC): ")
  print(unlist(ROCR::performance(predROCR, "auc")@y.values))
}

LogisticRegressionROC <- function(Model, Data) {
  
  predModel = predict(Model, newdata=Data, type = 'response' )
  predROCR = ROCR::prediction(predModel, Data$Spam)
  
  perf = ROCR:: performance(predROCR, "tpr", "fpr")
  
  ROC = cbind.data.frame(FP=unlist(perf@x.values), 
                         TP=unlist(perf@y.values), 
                         Threshold = unlist(perf@alpha.values))
  
  return(ROC)
}



#-------------------------------------------------------------------
# Check function
#emails = read.csv("./Data/emails.csv", stringsAsFactors = F)

#source('TermDocumentMatrix.R')
#EmailsDF = TextDataFrame(TermMatrix(emails$text))
#EmailsDF$Spam = as.factor(emails$spam)

# Split the data
#Spl = caTools::sample.split(EmailsDF$Spam, SplitRatio = 0.7)

#source('TrainTestDataset.R')
#train = Train(EmailsDF, Spl)
#test = Test(EmailsDF, Spl)


#log = LogisticRegressionFit(train)
#summary(log)

#LogisticRegressionEval(log, train)
#LogisticRegressionEval(log, test)

