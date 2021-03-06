# Instalo las librerias
install.packages("FNN")
install.packages("dummies")
install.packages("scales")
# install.packages("Scale")
install.packages("caret")
# install.packages("psych")
install.packages("Hmisc")
install.packages("survival")
install.packages("caret")
install.packages("lattice")
install.packages("ggplot2")
install.packages("Formula")

library(FNN)
library(dummies)
library(scales)
library(Formula)
library(lattice)
library(ggplot2)
library(Hmisc)
library(survival)
library(caret)
# library(psych)
library(Hmisc)
library(caret)
#library(Scale)


install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

# cargamos la base de Boston Housing (bh)
# vamos a predecir la variable MVED

set.seed(2018)

bh <- read.csv("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo/Nuevo udemy II/Datos Inteligencia-Artificial/r-course/data/tema4/BostonHousing.csv")

#armamos el conjunto de partici�n 
t.id <- createDataPartition(bh$MEDV, p= .7, list=F)

#usamos el arbol de regresion con rpart
bfit <- rpart(MEDV ~.,data= bh[t.id,])
bfit

# para q el grafico me ocupe toda la pantalla
par(mflow=c(1,1))

# vamos a ver una representaci�n grafica
prp(bfit, type=2, nn=T,
    fallen.leaves = T, faclen = 4,
    varlen = 8, shadow.col = "gray")

# Este arbol me muestra que varaibles son relevantes y cuales no aparecen en el arbon y no
# no son relevantes

# puedo analizar Qu� tipo de arbol me conviene elegir, seg�n la cantidad 
# de ramificaciones (nsplit), nos indica xerror (error relativo) y xstd (error standar)
# debo buscar el arbon donde sea xerror+ xstd lo menor posible

bfit$cptable
# armamo un plot donde vemos la cantidad de ramas del arbol (size tree) y el nivel de error
# tambien puedo ver el CP (factor de complejidad)
plotcp(bfit)

# Elijo el 5to no tiene tanta complejoidad, no tiene tantas ramas y un error relativamente bajo
# Entonces corro utilizando el arbol con 5 ramas q es el 6to

bfitprune <- prune(bfit, cp= 0.02527075)

prp(bfitprune, type=2, nn= T,
    fallen.leaves = T, faclen = 4,
    varlen = 8, shadow.col = "gray")

# Usamos este �ltimo arbol (bfitprune) para predecir los valores que estan fuera del consunjunto (-t.id) de validaci�n
# Es decir vamos a hacer la estimaci�n de las casas de boston las que estan afuera del conjunto de predici�n

preds <- predict(bfitprune, bh[-t.id,])
# Calculamos el error cuadr�tico de los valores predecidos menos los valores originales de la columna MDEV (bh[-t.id,]$MEDV))
sqrt(mean((preds - bh[-t.id,]$MEDV)^2))

# Si lo queremos hacer con el conjunto de entrenamiento es =
preds <- predict(bfitprune, bh[t.id,])
sqrt(mean((preds - bh[t.id,]$MEDV)^2))

# �Qu� sucede si elijo el �rbol original (bfit)
preds <- predict(bfit, bh[t.id,])
sqrt(mean((preds - bh[t.id,]$MEDV)^2))

# Conclusi�n, el error es efectivamente mas chico x q es un arbol mas grande, pero el modelo es mas largo


