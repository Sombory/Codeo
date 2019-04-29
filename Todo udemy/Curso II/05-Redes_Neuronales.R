
# usamos la base de boston housing
bh <- read.csv("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo/Nuevo udemy II/Datos Inteligencia-Artificial/r-course/data/tema4/BostonHousing.csv")
install.packages("nnet")
install.packages("caret")
install.packages("devtools")
library(nnet)
library(caret)
library(devtools)
library(lattice)
library(ggplot2)
library(randomForest)

set.seed(2018)
t.id <- createDataPartition(bh$MEDV, p= 0.7, list = F)

# quiero ver el rango de variabilidad de MEDV, como el maximo es 50, voy a decirle q me
#prediga pero llevada a su maximo
summary(bh$MEDV)

# Maxit es el parametro que se utiliza para "parar" (numero de iterasiones) el proceso si la corrida no converge
# linaut .. es una respuesta "lineal"
fit <- nnet(MEDV/50 ~., data=bh[t.id, ],
            size = 6, decay = 0.1,
            maxit= 1000, linout=T)

# para profundizar mas https://beckmw.wordpress.com/2013/11/14/visualizing-neural-networks-in-r-update/


install.packages("NeuralNetTools")
source_url("https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r")
# Otra opsión pero que no me funciona
library(NeuralNetTools)

# Me sale el graf de la red neuronal con los 6 nodos y todas las variables del modelo
plot(fit, max.sp=T)

# Para saver el error de los valores ajustados sobre el conjunto de entrenamiento
sqrt(mean((fit$fitted.values*50- bh[t.id,"MEDV"])^2))
# Vemos que los valores los ajusta terriblemente bien por eso el error cuadratico es muy bajo

pred <- predict(fit, bh[-t.id,])
sqrt(mean((pred*50 -  bh[-t.id,"MEDV"])^2))

  