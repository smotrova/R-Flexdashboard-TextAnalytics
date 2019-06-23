emails = read.csv("./Data/emails.csv", stringsAsFactors = F)

source('TermDocumentMatrix.R')
EmailsDF = TextDataFrame(TermMatrix(emails$text))
EmailsDF$Spam = as.factor(emails$spam)

#Split the data
set.seed(123) 
Spl = caTools::sample.split(EmailsDF$Spam, SplitRatio = 0.7)

source('TrainTestDataset.R')
train = Train(EmailsDF, Spl)
test = Test(EmailsDF, Spl)

library(ggplot2)
library(ggthemes)
library(ggsci)

source('LogisticRegression.R')
log = LogisticRegressionFit(train)

source('TreeRPART.R')
Tree = TreeFit()

source('RandomForest.R')
RF = RandomForestFit()

source('SupportVectorMachine.R')
SVM = SVMFit()


roc_train = rbind.data.frame( cbind(LogisticRegressionROC(log, train), 
                 Model = rep("LR",nrow(LogisticRegressionROC(log, train))) ),
                 
                 cbind(TreeROC(Tree, train), 
                       Model = rep("Tree",nrow(TreeROC(Tree, train))) ),
                 
                 cbind(RandomForestROC(RF, train), 
                       Model = rep("RF",nrow(RandomForestROC(RF, train))) ),
                 
                 cbind(SVMROC(SVM, train), 
                       Model = rep("SVM",nrow(SVMROC(SVM, train))) )
                 )


ggplot(roc_train, aes(FP, TP, col = Model)) + 
  xlab("False positive rate") +
  ylab("True positive rate") +
  geom_line(data = roc_train[roc_train$Model == 'LR', ], size = 1.0) +
  geom_line(data = roc_train[roc_train$Model == 'Tree', ], size = 1.0) +
  geom_line(data = roc_train[roc_train$Model == 'RF', ], size = 1.0) +
  geom_line(data = roc_train[roc_train$Model == 'SVM', ], size = 1.0)+
  scale_color_d3()

  

roc_test = rbind.data.frame( cbind(LogisticRegressionROC(log, test), 
                                    Model = rep("LR",nrow(LogisticRegressionROC(log, test))) ),
                              
                              cbind(TreeROC(Tree, test), 
                                    Model = rep("Tree",nrow(TreeROC(Tree, test))) ),
                              
                              cbind(RandomForestROC(RF, test), 
                                    Model = rep("RF",nrow(RandomForestROC(RF, test))) ),
                              
                              cbind(SVMROC(SVM, test), 
                                    Model = rep("SVM",nrow(SVMROC(SVM, test))) )
                              )


ggplot(roc_test, aes(FP, TP, col = Model)) + 
  xlab("False positive rate") +
  ylab("True positive rate") +
  geom_line(data = roc_test[roc_test$Model == 'LR', ], size = 1.0) +
  geom_line(data = roc_test[roc_test$Model == 'Tree', ], size = 1.0) +
  geom_line(data = roc_test[roc_test$Model == 'RF', ], size = 1.0) +
  geom_line(data = roc_test[roc_test$Model == 'SVM', ], size = 1.0)+
  scale_color_d3()


