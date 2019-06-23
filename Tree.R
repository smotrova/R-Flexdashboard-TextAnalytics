# Trees
# Tree package

TreeFit <- function() {
  # build a Tree
  model = tree::tree(Spam~., data=train)
  
  # 10-fold cross-validation
  set.seed(123)
  cv.model = tree:: cv.tree(model, FUN = prune.misclass, K = 10)
  cv.model
  best = cv.model$size[which.min(cv.model$dev)]
  prune.model = tree:: prune.tree(model, best, method = 'misclass')
  
  return(prune.model)
}


TreeEval <- function(Model, Data) {
  predModel = predict(Model, newdata=Data, type = 'vector' )
  predSpam = ifelse(predModel[,2] > 0.5, 1, 0)
  
  #Error Rate
  print('Error Rate: ')
  print(mean(Data$Spam != predSpam))
  
  # Compute AUC  
  predROCR = ROCR::prediction(predModel[,2], Data$Spam)
  print("Area Under Curve (AUC): ")
  print(unlist(ROCR::performance(predROCR, "auc")@y.values))
}

#----------------------------------------------------------------------------------
# Check functions
# Trees

#emails = read.csv("./Data/emails.csv", stringsAsFactors = F)

#source('TermDocumentMatrix.R')
#EmailsDF = TextDataFrame(TermMatrix(emails$text))
#EmailsDF$Spam = as.factor(emails$spam)

# Split the data
#set.seed(123)
#Spl = caTools::sample.split(EmailsDF$Spam, SplitRatio = 0.7)

#source('TrainTestDataset.R')
#train = Train(EmailsDF, Spl)
#test = Test(EmailsDF, Spl)

#Tree = TreeFit()
#summary(Tree)

#plot(Tree)
#text(Tree)

#TreeEval(Tree, train)
#TreeEval(Tree, test)
