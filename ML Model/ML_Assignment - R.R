rm(list=ls(all=TRUE))
setwd("~/Desktop/Insofee/Module05_AI and Decision Science/Datasets")
library(dummies)
library(nnet)
library(randomForest)
library(caret)
library(ggplot2)
library(reshape2)
library(pastecs)
library(vegan)
library(DMwR)

attr = c('id', 'age', 'exp', 'inc', 'zip', 'family', 
         'ccavg', 'edu', 'mortgage', 'loan', 
         'securities', 'cd', 'online', 'cc')

# Read the data using csv file
data = read.csv(file = "UniversalBank.csv", 
                header = TRUE, col.names = attr)

# Removing the id, zip and experience. 
drop_Attr = c("id", "zip","age")
attr = setdiff(attr, drop_Attr)
data = data[, attr]
str(data)

options(scipen=100)
options(digits=2)
stat.desc(data)

#converting to approproate data types - 
cat_Attr = c("family","edu","securities","cd","online","loan")
num_Attr = setdiff(attr, cat_Attr)
cat_Data <- data.frame(sapply(data[,cat_Attr], as.factor))
num_Data <- data.frame(sapply(data[,num_Attr], as.numeric))
num_Data <- decostand(num_Data, "range") #normalising numeric data
data <- cbind(num_Data, cat_Data)
str(data)
rm(attr, cat_Attr, num_Attr, drop_Attr,cat_Data, num_Data, reg_Data)

# Split dataset into train and test
set.seed(12)

train_RowIDs = sample(1:nrow(data), nrow(data)*0.7)
train_Data = data[train_RowIDs,]
test_Data = data[-train_RowIDs,]
rm(train_RowIDs)

#check the split with respect to target attribute - 

table(train_Data$loan)
table(test_Data$loan)


# Logistic Regression Model
Unibank_glm = glm(loan~., data = train_Data, family=binomial)
summary(Unibank_glm)
car::vif(Unibank_glm)

#Accuracy on the training set
predictTrain = predict(Unibank_glm, type="response", newdata = train_Data)
pred_class <- ifelse(predictTrain> 0.5, 1, 0)

head(predictTrain)
head(pred_class)

# Confusion matrix with threshold of 0.5
conf_matrix_train = table(train_Data$loan, predictTrain > 0.5)

# Recall on Train Set
Recall_Train= conf_matrix_train[2,2]/sum(conf_matrix_train[2,2]+conf_matrix_train[2,1])
Recall_Train

# Confusion matrix with threshold of 0.9
table(train_Data$loan, predictTrain > 0.9)
# Confusion matrix with threshold of 0.7
table(train_Data$loan, predictTrain > 0.7)
# Confusion matrix with threshold of 0.5
table(train_Data$loan, predictTrain > 0.5)
# Confusion matrix with threshold of 0.3
table(train_Data$loan, predictTrain > 0.3)
# Confusion matrix with threshold of 0.1
table(train_Data$loan, predictTrain > 0.1)

# Test set AUC 
library(ROCR)
ROCRpred = prediction(predictTrain, train_Data$loan)
as.numeric(performance(ROCRpred, "auc")@y.values)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
par(mfrow=c(1,1))
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

#Conclusion - Based on the Train data, threshold for classification can be set to 0.3

# Predictions on the test set
predictTest = predict(Unibank_glm, type="response", newdata=test_Data)

# Confusion matrix with threshold of 0.3
conf_matrix_test = table(test_Data$loan, predictTest > 0.3)

# Recall on Test Set
Recall_test= conf_matrix_train[2,2]/sum(conf_matrix_train[2,2]+conf_matrix_train[2,1])
Recall_test

#Recall for the test data is 71.50%
