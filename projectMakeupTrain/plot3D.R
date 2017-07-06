
workSpace <- setwd("/home/danilo/R/workspace/projetoMaquiagem/")

dataMedidasRosto <- read.csv("medidasRosto.csv", stringsAsFactors = TRUE)


# 3D Scatterplot
library(scatterplot3d)
larguraTesta <- dataMedidasRosto$larguraTesta
larguraMacaDoRosto <- dataMedidasRosto$larguraMacaDoRosto
larguraMandibula <- dataMedidasRosto$larguraMandibula

dataMedidasRosto$Colour="black"
# Set new column values to appropriate colours
dataMedidasRosto$Colour[dataMedidasRosto$tipo=="alongado"]="red"
dataMedidasRosto$Colour[dataMedidasRosto$tipo=="oval"]="blue"
dataMedidasRosto$Colour[dataMedidasRosto$tipo=="quadrado"]="green"
dataMedidasRosto$Colour[dataMedidasRosto$tipo=="redondo"]="pink"
dataMedidasRosto$Colour[dataMedidasRosto$tipo=="triangular"]="orange"


dataMedidasRosto
#scatterplot3d(larguraTesta,larguraMacaDoRosto,larguraMandibula, main="Medidas do rosto", pch=16, col.lab = "red")
scatterplot3d(larguraTesta,larguraMacaDoRosto,larguraMandibula, main="Medidas do rosto", pch=16, color = dataMedidasRosto$Colour)

# Spinning 3d Scatterplot
library(rgl)
plot3d(larguraTesta,larguraMacaDoRosto,larguraMandibula, main="", size=3, col = dataMedidasRosto$Colour)
#plot3d(larguraTesta, larguraMacaDoRosto, larguraMandibula, col="red", size=3)

