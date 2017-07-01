rm(list=ls(all=TRUE))
setwd("~/Desktop/Insofee/Module05_AI and Decision Science/Datasets")
library(dummies)
library(nnet)
library(randomForest)
library(caret)
library(ggplot2)
library(reshape2)

attr = c('id', 'age', 'exp', 'inc', 'zip', 'family', 
         'ccavg', 'edu', 'mortgage', 'loan', 
         'securities', 'cd', 'online', 'cc')

# Read the data using csv file
data = read.csv(file = "UniversalBank.csv", 
                header = TRUE, col.names = attr)

# Removing the id, zip and experience. 
drop_Attr = c("id", "zip")
attr = setdiff(attr, drop_Attr)
data = data[, attr]
str(data)

data$loan = as.factor(data$loan)
str(data)


#Visualisation of data and distribution - 
par(mfrow = c(3,3))
ggplot(data, aes(x = age, y = factor(loan))) + geom_boxplot()
ggplot(data, aes(x = factor(loan), y = exp)) + geom_boxplot()
ggplot(data, aes(x = factor(loan), y = inc)) + geom_boxplot()
ggplot(data, aes(x = factor(loan), y = ccavg)) + geom_boxplot()
ggplot(data, aes(x = factor(loan), y = mortgage)) + geom_boxplot()
ggplot(data, aes(x = factor(loan), y = factor(edu))) + geom_boxplot()

