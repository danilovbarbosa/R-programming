#Plots
setwd("/home/danilo/R/workspace/R-programming/ML/")


if(!exists("foo", mode="function")) source("knn_final.R")
if(!exists("foo", mode="function")) source("bayes_final.R")
if(!exists("foo", mode="function")) source("c5_0.R")
if(!exists("foo", mode="function")) source("j48_final.R")
if(!exists("foo", mode="function")) source("jRip_final.R")
if(!exists("foo", mode="function")) source("randomForest_final.R")
if(!exists("foo", mode="function")) source("smo_final.R")
if(!exists("foo", mode="function")) source("svm_final.R")

listValuesKnn <- knnFunction(getwd())
listValuesBayes <- bayesFunction(getwd())
listValuesC5_0 <- c5_0Function(getwd())
listValuesJ48 <- j48Function(getwd())
listValuesjRip <- jRipFunction(getwd())
listValuesRandomForest <- randomForestFunction(getwd())
listValuesSmo <- smoFunction(getwd())
listValuesSvm <- svmFunction(getwd())


listValuesKnn
listValuesBayes
listValuesC5_0
listValuesJ48
listValuesjRip
listValuesRandomForest
listValuesSmo
listValuesSvm

listValuesAll <- list(listValuesKnn, listValuesBayes, listValuesC5_0, listValuesJ48, listValuesjRip, listValuesRandomForest, listValuesSmo, listValuesSvm)
listPrecision <- c()
listRecall <- c()
listFMeasure <- c()

for(listValues in listValuesAll){
  print(listValues)
  listPrecision <- c(listPrecision, listValues[1])
  listRecall <- c(listRecall, listValues[2])
  listFMeasure <- c(listFMeasure, listValues[3])
}

#listPrecision
#listRecall
#listFMeasure


namesAlg <- c("Knn", "Naive Bayes", "C5.0", "J48", "JRip", "Random Forest", "SMO","SVM")
xlab = "Algorithms Machine Learning"
ylab = "Values"
barplot(listPrecision, main="Precision", names.arg = namesAlg, xlab = xlab, ylab = ylab)
barplot(listRecall, main="Recall",  names.arg = namesAlg, xlab = xlab, ylab = ylab)
barplot(listFMeasure, main="F-Measure",  names.arg = namesAlg, xlab = xlab, ylab = ylab)

