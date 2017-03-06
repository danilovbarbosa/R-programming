#Plots
setwd("/home/danilo/R/workspace/R-programming/ML/")


if(!exists("foo", mode="function")) source("knn_final.R")
if(!exists("foo", mode="function")) source("bayes_final.R")
if(!exists("foo", mode="function")) source("c5_0.R")
if(!exists("foo", mode="function")) source("svn_final.R")
if(!exists("foo", mode="function")) source("randomForest_final.R")

listValuesKnn <- knnFunction(getwd())
listValuesBayes <- bayesFunction(getwd())
listValuesC5_0 <- c5_0Function(getwd())
listValuesSvm <- svmFunction(getwd())
listValuesRandomForest <- randomForestFunction(getwd())


listValuesKnn
listValuesBayes
listValuesC5_0
listValuesSvm
listValuesRandomForest

listPrecision <- c(listValuesKnn[1], listValuesBayes[1], listValuesC5_0[1], listValuesSvm[1], listValuesRandomForest[1])
listRecall <- c(listValuesKnn[2], listValuesBayes[2],listValuesC5_0[2], listValuesSvm[2],  listValuesRandomForest[2])
listFMeasure <- c(listValuesKnn[3], listValuesBayes[3], listValuesC5_0[3], listValuesSvm[3],  listValuesRandomForest[3])

namesAlg <- c("Knn", "Naive Bayes", "C5.0", "SVM", "Random Forest")
xlab = "Algorithms Machine Learning"
ylab = "Values"
barplot(listPrecision, main="Precision", names.arg = namesAlg, xlab = xlab, ylab = ylab)
barplot(listRecall, main="Recall",  names.arg = namesAlg, xlab = xlab, ylab = ylab)
barplot(listFMeasure, main="F-Measure",  names.arg = namesAlg, xlab = xlab, ylab = ylab)

