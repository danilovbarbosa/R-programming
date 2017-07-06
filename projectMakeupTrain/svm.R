#library(e1071)
library(kernlab)
library(caTools)
library(randomForest)


########################### functions ############
splitDatesInTrainAndTest <- function(dataMedidasRosto){
  #Separando base de trainamento e teste
  indexMedidasRosto <- sample.split(dataMedidasRosto,SplitRatio=.7)
  trainsetMedidasRosto <- dataMedidasRosto[indexMedidasRosto==TRUE,]
  testsetMedidasRosto <- dataMedidasRosto[indexMedidasRosto==FALSE,]
  
  return(list(trainsetMedidasRosto, testsetMedidasRosto))
}

unionDates <- function(listMedidasRosto){
  setMedidasRosto <- data.frame()
  for (medidaRosto in listMedidasRosto){
    auxDataFrame <- as.data.frame(medidaRosto)
    setMedidasRosto <- rbind(setMedidasRosto, auxDataFrame)
  }
  return(setMedidasRosto)
}

svmWithPrediction <- function(tipo, setTrainSVM, setTestSVM){
  #Definindo o primeiro tipo como 1 e o resto como 0 
  setTrainSVM$tipo<-as.character(setTrainSVM$tipo)
  setTrainSVM$tipo[setTrainSVM$tipo!=tipo]<-'0'
  setTrainSVM$tipo[setTrainSVM$tipo==tipo]<-'1'
  print(setTrainSVM)
  
  #Aplicando SVM
  predMedidasRosto <- ksvm(tipo ~ ., data = setTrainSVM, kernel = "rbfdot")
  print(predMedidasRosto)
  
  #Predições
  #print(setTestSVM[,1])
  predictionsMedidasRosto <- predict(predMedidasRosto, setTestSVM)
  print(predictionsMedidasRosto)
  #print(table(predictionsMedidasRosto, setTestSVM$tipo))
  
  return(predictionsMedidasRosto)
}
randomForestWithPrediction <- function(tipo, setTrainSVM, setTestSVM){
  predMedidasRosto <- randomForest(tipo ~ ., setTrainSVM, ntree=50, norm.votes=FALSE)
  return(predMedidasRosto)
}
###########################

workSpace <- setwd("/home/danilo/R/workspace/projetoMaquiagem/")

dataMedidasRosto <- read.csv("medidasRosto.csv", stringsAsFactors = TRUE)

#Split por factor
dataMedidasRostoSplitPorFactor <- split(dataMedidasRosto, dataMedidasRosto$tipo)

datesInTrainAndTest.alongado <- splitDatesInTrainAndTest(dataMedidasRostoSplitPorFactor[1]$alongado)
#datesInTrainAndTest.coracao <- splitDatesInTrainAndTest(dataMedidasRostoSplitPorFactor[2]$coracao)
#datesInTrainAndTest.diamante <- splitDatesInTrainAndTest(dataMedidasRostoSplitPorFactor[3]$diamante)
datesInTrainAndTest.oval <- splitDatesInTrainAndTest(dataMedidasRostoSplitPorFactor[4]$oval)
datesInTrainAndTest.quadrado <- splitDatesInTrainAndTest(dataMedidasRostoSplitPorFactor[5]$quadrado)
datesInTrainAndTest.redondo <- splitDatesInTrainAndTest(dataMedidasRostoSplitPorFactor[6]$redondo)
datesInTrainAndTest.triangular <- splitDatesInTrainAndTest(dataMedidasRostoSplitPorFactor[7]$triangular)

#Unindo os dados de treinamento e de teste
auxTrainsetMedidasRosto <- c(datesInTrainAndTest.alongado[1], datesInTrainAndTest.oval[1], datesInTrainAndTest.quadrado[1], datesInTrainAndTest.redondo[1], datesInTrainAndTest.triangular[1])
auxTestsetMedidasRosto <- c(datesInTrainAndTest.alongado[2], datesInTrainAndTest.oval[2], datesInTrainAndTest.quadrado[2], datesInTrainAndTest.redondo[2], datesInTrainAndTest.triangular[2])

trainsetMedidasRosto <- unionDates(auxTrainsetMedidasRosto)
testsetMedidasRosto <- unionDates(auxTestsetMedidasRosto)
length(testsetMedidasRosto)

##Ajeitar o gráfico 3D.
teste <- split(trainsetMedidasRosto, trainsetMedidasRosto$tipo)
plot(as.table(teste[1]))


ls.str(trainsetMedidasRosto)
#testsetMedidasRosto

#Aplicando o svm e pegando o prediction
predictionRostoAlongado <- svmWithPrediction("alongado", trainsetMedidasRosto, testsetMedidasRosto)
#predictionRostoCoracao <-svmWithPrediction("coracao", trainsetMedidasRosto, testsetMedidasRosto)
#predictionRostoDiamante <-svmWithPrediction("diamante", trainsetMedidasRosto, testsetMedidasRosto)
predictionRostoOval <-svmWithPrediction("oval", trainsetMedidasRosto, testsetMedidasRosto)
predictionRostoQuadrado <-svmWithPrediction("quadrado", trainsetMedidasRosto, testsetMedidasRosto)
predictionRostoRedondo <-svmWithPrediction("redondo", trainsetMedidasRosto, testsetMedidasRosto)
predictionRostoTriangular <-svmWithPrediction("triangular", trainsetMedidasRosto, testsetMedidasRosto)

predictionrandomForest <- randomForestWithPrediction(trainsetMedidasRosto$tipo, trainsetMedidasRosto, testsetMedidasRosto)

