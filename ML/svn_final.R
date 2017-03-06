svmFunction <- function(workSpace){
setwd(workSpace)
#setwd("/home/danilo/R/workspace/R-programming/ML/")

wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
wbcd <- wbcd[-1]#Apaga a primeira coluna, pois como contém apenas os valores de ID, eles não nos interessam

table(wbcd$diagnosis)#Cria uma tabela com os valores da coluna 'diagnosis'

#Muitos classificadores de aprendizagem de máquina R exigem que o recurso de destino seja codificado como um fator, 
#então precisamos recodificar a variável de diagnóstico. 
#Também aproveitamos esta oportunidade para dar aos valores "B" e "M" valores mais informativos usando o parâmetro labels
wbcd$diagnosis<- factor(wbcd$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))
#wbcd$diagnosis
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)#round aredonda o valor e prop.table pega as colunas da tabela e mostra a divisão em %

summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])
#Função para normalizar as features das colunas
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Aplicando a normalize na tabela wbcd a partir da coluna 2, criando um novo frame e salvando wm wbcd_n
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
wbcd_n <- merge(wbcd[1], wbcd_n)
summary(wbcd_n$area_mean)

#Dividindo a base em treinamento e teste
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

#Valores do diagnosis, é o que queremos predizer, são os fatores
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

#Carregando lib do svm
library(kernlab)
#Aplicando SVM
wbcd_test_pred <- ksvm(diagnosis ~ ., data = wbcd_train, kernel = "vanilladot")
wbcd_test_pred
#Predições
wbcd_predictions <- predict(wbcd_test_pred, wbcd_test)
table(wbcd_predictions, wbcd_test$diagnosis)

library(gmodels)
CrossTable(x = wbcd_test_labels, y = wbcd_predictions, prop.chisq=FALSE)

#Valores da CroosTable
trueNegative <- 77
falsePositive <-  23
falseNegative <-0
truePositive <- 0

#Aplicando Precision
precision_sms_results1 <- truePositive / (truePositive + falsePositive)
precision_sms_results1

#Aplicando Recall
recall_sms_results1 <-  truePositive / (truePositive + falseNegative)
recall_sms_results1

#Aplicando F-measure
f_measure_sms_results1 <- (2 * precision_sms_results1 * recall_sms_results1) / (precision_sms_results1 + recall_sms_results1)
f_measure_sms_results1

value_return <- c(precision_sms_results1, recall_sms_results1, f_measure_sms_results1)
value_return
}

#lsvm <- svmFunction("/home/danilo/R/workspace/R-programming/ML/")
#lsvm
