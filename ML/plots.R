#Plots
setwd("/home/danilo/R/workspace/R-programmming/ML/")


if(!exists("foo", mode="function")) source("knn_final.R")
if(!exists("foo", mode="function")) source("bayes_final.R")
if(!exists("foo", mode="function")) source("c5_0.R")


listValuesKnn <- knnFunction(getwd())
listValuesBayes <- bayesFunction(getwd())
listValuesC5_0 <- c5_0Function(getwd())


listValuesKnn
listValuesBayes
listValuesC5_0


listPrecision <- c(listValuesKnn[1], listValuesBayes[1], listValuesC5_0[1])
listRecall <- c(listValuesKnn[2], listValuesBayes[2],listValuesC5_0[2])
listFMeasure <- c(listValuesKnn[3], listValuesBayes[3], listValuesC5_0[3])

barplot(listPrecision)
barplot(listRecall)
barplot(listFMeasure)

