
ed <- read.csv("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo/Nuevo udemy II/Datos Inteligencia-Artificial/r-course/data/tema4/education.csv")
bh <- read.csv("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo/Nuevo udemy II/Datos Inteligencia-Artificial/r-course/data/tema4/BostonHousing.csv")


ed$region <- factor(ed$region)
t.id <- createDataPartition(ed$expense, p=0.7, list = F)
fit <- rpart(expense ~ region+urban+income+under18, data = ed[t.id,])

prp(fit, type = 2, nn=T, fallen.leaves = T,
    faclen = 4, varlen = 8, shadow.col = "gray")

# Hay dos metodos para ensamblar variables o dataframe
##  Empezamos con el metodo del ""Bagging""¡¡

install.packages("ipred")
library(ipred)

bagging.fit <- bagging(MEDV~., data = bh[t.id,])
prediction.t <- predict(bagging.fit, bh[t.id,])
sqrt(mean((prediction.t- bh[t.id,]$MEDV)^2))

prediction.v <- predict(bagging.fit, bh[-t.id,])
sqrt(mean((prediction.v- bh[-t.id,]$MEDV)^2))

# Boosting. Modelo de aprendizaje .. modelo de gradientes mejorarados

install.packages("gbm")
library(gbm)









