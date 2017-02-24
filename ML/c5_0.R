c5_0Function <- function(workSpace){
  
setwd(workSpace)

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
summary(wbcd_n$area_mean)

#Dividindo a base em treinamento e teste
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

#Valores do diagnosis, é o que queremos predizer, são os fatores
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

library(C50)

#Aplicando o treimanto na arvore, criando o modelo preditivo
wbcd_classifier <- C5.0(wbcd_train, wbcd_train_labels)
wbcd_classifier
summary(wbcd_classifier)

#Avaliando o modelo
wbcd_pred <- predict(wbcd_classifier, wbcd_test)

library(gmodels)

CrossTable(wbcd_pred, wbcd_test_labels, prop.chisq = FALSE, prop.t = FALSE)

###Novos testes###

#•	 True Positive (TP): Correctly classified as the class of interest
#•	 True Negative (TN): Correctly classified as not the class of interest
#•	 False Positive (FP): Incorrectly classified as the class of interest
#•	 False Negative (FN): Incorrectly classified as not the class of interest

#Valores da CroosTable
trueNegative <- 71
falsePositive <-  2
falseNegative <- 6
truePositive <- 21


library(caret)
#Aplicando Precision
precision_sms_results1 <- truePositive / (truePositive + falsePositive)
precision_sms_results1

#Aplicando Recall
recall_sms_results1 <-  truePositive / (truePositive + falseNegative)
recall_sms_results1

#Aplicando F-measure
f_measure_sms_results1 <- (2 * precision_sms_results1 * recall_sms_results1) / (precision_sms_results1 + recall_sms_results1)
f_measure_sms_results1

#boxplot(precision_sms_results1, recall_sms_results1, f_measure_sms_results1)

value_return <- c(precision_sms_results1, recall_sms_results1, f_measure_sms_results1)
value_return
}