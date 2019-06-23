# Trees
# RPART Package
# caret package for performing cross-validation

TreeFit<- function() {
  
  set.seed(123)
  modelCART = rpart:: rpart(Spam~., method = "class", data = train)
  
  # a cross-validation has (usually) been done in the initial construction by rpart.
  
  # ~Choose a cp parameter~
  
  # A good choice of cp for pruning is often 
  # the leftmost value for which the mean lies below the horizontal line.
  # The horizontal line is drawn 1SE above the minimum of the curve
  
  # rpart::plotcp(modelCART, col = 2, lty=2)
  # modelCART$cptable
  
  # Define the line level
  LineLevel= min(modelCART$cptable[, 3]) + 
    1*sd(modelCART$cptable[, 3])/sqrt(nrow(modelCART$cptable))
  
  # Choose a cp value as a leftmost value 
  
  modelCV = rpart:: prune(modelCART, 
                  cp = max( modelCART$cptable[modelCART$cptable[, 3]< LineLevel, 1] ))
  
  #cat("cp = ", 
  #    max( modelCART$cptable[modelCART$cptable[, 3]< LineLevel, 1] ), "\n")
  
  return(modelCV)
}

TreeEval <- function(Model, Data) {
  predModel = predict(Model, newdata=Data )
  predSpam = ifelse(predModel[,2] > 0.5, 1, 0)
  
  #Error Rate
  print('Error Rate: ')
  print(mean(Data$Spam != predSpam))
  
  # Compute AUC  
  predROCR = ROCR::prediction(predModel[,2], Data$Spam)
  print("Area Under Curve (AUC): ")
  print(unlist(ROCR::performance(predROCR, "auc")@y.values))
}


TreeROC <- function(Model, Data) {
  
  predModel = predict(Model, newdata=Data)
  predROCR = ROCR::prediction(predModel[ ,2], Data$Spam)
  
  perf = ROCR:: performance(predROCR, "tpr", "fpr")
  
  ROC = cbind.data.frame(FP=unlist(perf@x.values), 
                         TP=unlist(perf@y.values), 
                         Threshold = unlist(perf@alpha.values))
  
  return(ROC)
}

#----------------------------------------------------------------------------------
# Check functions
# Trees

#emails = read.csv("./Data/emails.csv", stringsAsFactors = F)

#source('TermDocumentMatrix.R')
#EmailsDF = TextDataFrame(TermMatrix(emails$text))
#EmailsDF$Spam = as.factor(emails$spam)

# Split the data
# set.seed(123)
#Spl = caTools::sample.split(EmailsDF$Spam, SplitRatio = 0.7)

#source('TrainTestDataset.R')
#train = Train(EmailsDF, Spl)
#test = Test(EmailsDF, Spl)


#Tree = TreeFit()
#Tree
#rpart.plot:: prp(Tree)

#TreeEval(Tree, train)
#TreeEval(Tree, test)

