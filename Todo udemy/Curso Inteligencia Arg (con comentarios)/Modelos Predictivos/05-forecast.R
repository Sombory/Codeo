
bh <- read.csv("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo/Nuevo udemy II/Datos Inteligencia-Artificial/r-course/data/tema4/BostonHousing.csv")

install.packages("randomForest")
install.packages("ggplot2")

library(lattice)
library(ggplot2)
library(randomForest)
library(caret)

set.seed(2018)

# gbmfit <- gbm(MEDV~., data = bh, distribution = "gaussian")
# prediction.t <- predict(gbmfit, bh)

t.id <- createDataPartition(bh$MEDV, p= 0.7, list = F)

# contruimos varios arboles de reg y luego los procesamos


mod <-  randomForest(x= bh[t.id,1:13],y=bh[t.id,14],
                     ntree = 1000, 
                     xtest = bh[-t.id, 1:13], ytest = bh[-t.id, 14],
                     importance = T,
                     keep.forest = T)
#  veo un resumen del modelo con los 1000 arboles generados,
# veo q explica/predice un 86% del modelo y que tienen un promedio 4 divisiones (split)
mod

mod$importance
# mientras mas alto es el número de IncNodePure, + explica a la variable


# hacemo un plot para comparar los valores de la base vs. los predecidos del modelo
plot(bh[t.id,]$MEDV, predict(mod, newdata = bh[t.id,]),
     xlab = "Actuales", ylab = "Predichos")

abline(0,1)

#mtry
