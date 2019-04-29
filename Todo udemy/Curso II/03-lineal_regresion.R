
library(caret)

auto <- read.csv("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo/Nuevo udemy II/Datos Inteligencia-Artificial/r-course/data/tema4/auto-mpg.csv")

# Modifico el contenido de la variable cylindros a una variable de "factores" (ej. 3 a 3c)
auto$cylinders <- factor(auto$cylinders,
                         levels = c(3,4,5,6,8),
                         labels = c("3c", "4c","5c","6c","8c"))

set.seed(2018)
# mpg son la cantidad de millas q se pueden hacer x galeón
t.id <- createDataPartition(auto$mpg, p=0.7, list=F)
names(auto)

# creo 1 modelo linea, pero exluyendo siento tipo de variables como nombre del vehículo
# nótese q excluyo las variables 1,8 y 9
# corro la variable mpg en función (~) del resto de varaibles incluídas en el modelo
mod <- lm(mpg ~ .,data = auto[t.id, -c(1,8,9)])
plot(mod)


# con los resultados del modelo puedo armar
#  mpg = 38.607312 + 7.212652* 4c + 5.610350*5c + 3.307172*6c + ....
# 0.006878* displacement  -0.072209* horsepower 

# entonces con esto si me dicen q tiene un auto con determinada cilindrada
# yo te puedo decir cuanto consumo

summary(mod)
# para ver los residuos
boxplot(mod$residuals)
# Saco el error mínimo cuadrático movil
sqrt(mean((mod$fitted.values - auto[t.id,]$mpg)^2))

# Corro el modelo con a predicción 
pred <- predict(mod, auto[-t.id, -c(1,8,9)])
sqrt(mean((pred - auto[-t.id,]$mpg)^2))

# haremos una grafico de 2 filas y 2 col
par(mfrow=c(2,2))
plot(mod)

# si queremos ordenar el dataframe priorizando los autos de cilindrada 4c
# va a ser modificar la categoria de referencia 
auto <- within(auto,
               cylinders <- relevel(cylinders, ref = "4c"))

# vemos si modifica algo en el modelo 
mod <- lm(mpg ~., data = auto[t.id, -c(1,8,9)])
mod
# se observa que "4c" no esta mas porq concidera q es lo estander¡¡
# recomendación: se debe utilizar la referencia mas "frecuente" (en el ej. "4c")
pred <- predict(mod, auto[-t.id, -c(1,8,9)])
pred
sqrt(mean((pred - auto[-t.id,]$mpg)^2))
plot(mod)

### AGREGAMOS LA LIBRERIA MASS
# Este modelo va interando paso a paso y sacando las variables que son poco relevantes
library(MASS)
mod 
summary(mod)

# Mediante la formula Step AIC (se puede poner "foward" o "backward")
#acaike informacion complement AIC ---  si sindicamos fowar va agregando variables a partir de un modelo nulo 
# y si le ponemos backward va quitando varaibles a partir de un modelo completo
step.model <- stepAIC(mod, direction="forward")

# con Backward va eliminando variables hasta que se queda con 
# mpg ~ cylinders + horsepower + weight (y dice que displacement y acceleration no son relevantes)
step.model <- stepAIC(mod, direction="backward")
summary(step.model)
step.model





