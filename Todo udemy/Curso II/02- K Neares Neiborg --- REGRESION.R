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

edu <- read.csv("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo/Nuevo udemy II/Datos Inteligencia-Artificial/r-course/data/tema4/education.csv")

#creo variables dummies para c/region (son 4)
dms <- dummy(edu$region, sep = "_")
edu <- cbind(edu,dms)
View(edu)

# NORMALIZO las variables dentro del rango 0 a 1
# le asigono el nombre "urban.s" para rescalarlo.. GENERO ESTAS 3 NUEVAS VARIABLES
edu$urban.s <- rescale(edu$urban)
edu$income.s <- rescale(edu$income)
edu$under18.s <- rescale(edu$under18)

# HACEMOS UNA PARTICIÓN DE LOS DATOS
set.seed(2018)

# Trabajo con la partición del 60% de los datos de edu&expense
t.id <- createDataPartition(edu$expense, p=0.6, list = F)
tr <- edu[t.id, ]
temp <- edu[-t.id, ]
# Ahora pariticiono el grupo en 2 iguales por eso pongo p=0.5 (50% de los datos)
v.id <- createDataPartition(temp$expense, p=0.5, list=F)
val <- temp[v.id, ]
test <- temp[-v.id, ]

# vemos que modelo es mejor mediante una regresión
# USAMOS EL METODO DE K "VECINOS" (knn.reg), KNN, es Nearest, Neighbour (vecinos proximos)
# Opera con la operacion euclidea
# Mas info en https://www.youtube.com/watch?v=zBCcAtg3P4k

reg1 <- knn.reg(tr[,7:12], val[,7:12], tr$expense, k=1,
                algorithm = "brute" )
reg1

# Si queremos calcular nuestro error cuadratico medio (rmse). Uso el algoritmo de Fuerza bruta

rmse1 <- sqrt(mean(reg1$pred- val$expense)^2)
rmse1

# hacemos la otra regresión 
reg2 <- knn.reg(tr[,7:12],val[,7:12], tr$expense, k=2,
                 algorithm = "brute")

# calculamos el error cuadratico de forma mas fesil
rmse2 <- rmse(val$expense, reg2$pred)
rmse2 <- sqrt(mean(reg2$pred- val$expense)^2)
rmse2

# hacemos la regresión N°3 (k=3), esta regresion da el error + bajo
reg3 <- knn.reg(tr[,7:12],val[,7:12], tr$expense, k=3,
                algorithm = "brute")

rmse3 <- sqrt(mean(reg3$pred- val$expense)^2)
rmse3

# Conclusión: a medida que K crece, el error cuadrático va bajando

 # pero llega un punto donde seguir bajando los K no es OPTIMO
# YA 4 empieza a subir con este ejemplo
reg4 <- knn.reg(tr[,7:12],val[,7:12], tr$expense, k=4,
                algorithm = "brute")
rmse4 <- sqrt(mean(reg4$pred- val$expense)^2)
rmse4

# lo vemos en un plot 
error <- c(rmse1,rmse2, rmse3, rmse4)
plot(error, type = "o", xlab = "K", ylab = RMSE)

# hacer un test global
reg.test <- knn.reg(tr[,7:12],test[,7:12], tr$expense, k=3,
                algorithm = "brute")

rmse.test <- sqrt(mean(reg.test$pred- test$expense)^2)
rmse.test

# Genero un data frame para ver las diferencias entre los datos actuales vs. la predicción
df = data.frame(actual= test$expense, pred=reg.test$pred)
View(df)
plot(df)
abline(0,1)

#############################################
# vamos a hacer una validación cruzada, con test = NULL

t.id <- createDataPartition(edu$expense, p= 0.7, list = F)
tr <- edu[t.id, ] # indicador de entrenamiento // tr=training
val <- edu[-t.id,] # indicador de valuación  // val=valuation

# Hago la regresión, con Y (variable predictora "expense")
reg <- knn.reg(tr[,7:12], test = NULL, y = tr$expense,
               k=2, algorithm = "brute")

rmse.reg <- sqrt(mean(reg$residuals^2))
rmse.reg

#
rdacb.knn.reg <- function(tr_predictor, val_predictors,
                          tr_target, val_target, k){
  library(FNN)
  res <- knn.reg(tr_predictor, val_predictors,
                 tr_target, k, algorithm = "brute")
  rmserror <- sqrt(mean((val_target - res$pred)^2))
  cat(paste("RMSE para K", toString(k),": ", rmserror,"\n", sep=""))
  rmserror
}
                          
# lo hacemos con k=1
rdacb.knn.reg(tr[,7:12,], val[,7:12], 
              tr$expense, val$expense, 1)
# lo hacemos con k=2
rdacb.knn.reg(tr[,7:12,], val[,7:12], 
              tr$expense, val$expense, 2)
rdacb.knn.reg(tr[,7:12,], val[,7:12], 
              tr$expense, val$expense, 3)
rdacb.knn.reg(tr[,7:12,], val[,7:12], 
              tr$expense, val$expense, 4)

# Hacemos una función donde automatizamos lo anterior

rdacb.knn.reg.multi <- function(tr_prediction, val_predictors,
                                tr_target,val_target, start_k, end_k) {
  rms_errors=vector()
  for(k in start_k:end_k){
    rms_error <- rdacb.knn.reg(tr_prediction, val_predictors,
                               tr_target,val_target,k)
    rms_errors <- c(rms_errors, rms_error)
  }
  plot(rms_errors, type = "o", xlab = "K", ylab = "RMSE")
}


rdacb.knn.reg.multi(tr[,7:12,], val[,7:12], tr$expense, val$expense,1,10)


