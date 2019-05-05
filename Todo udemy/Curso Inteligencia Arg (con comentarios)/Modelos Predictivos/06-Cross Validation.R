
# Para evitar el sesgo de los resultados "en el grrupo de datos testiados" existe la validacion cruzada
# K-fold cross validation
bh <- read.csv("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo/Nuevo udemy II/Datos Inteligencia-Artificial/r-course/data/tema4/BostonHousing.csv")
# df: es el dataframe original
# 1: nrow(df) significa que va de la fila 1 a la "final de data set"

kfold.crossval.reg <- function(df, nfolds){
  fold <- sample(1:nfolds, nrow(df), replace = T)
  mean.sqr.errs <- sapply(1:nfolds,
                          kfold.cval.reg.iter,
                          df, fold)
  list("MSE " = mean.sqr.errs,
       "Overall_Mean_Sqr_Erro"= mean(mean.sqr.errs),
       "Std_Mean_Sqr_Error"= sd(mean.sqr.errs))
}

kfold.cval.reg.iter <- function(k, df, fold){
  
  tr.ids <- !fold %in% c(k)
  test.ids <- fold %in% c(k)
  mod <- lm(MEDV ~., data = df[tr.ids,])
  pred <- predict(mod, df[test.ids,])
  sqr.errs <- (pred - df[test.ids,"MEDV"])^2
  mean(sqr.errs)
}

res <- kfold.crossval.reg(bh, 5)
res

# Hacemos mediante el modelo: "leave one out cross validation" (loocv)
loocv.reg <- function(df){
  mean.sqr.errors <- sapply(1:nrow(df), 
                            loocv.reg.iter, df)
  list("MSE"=mean.sqr.errors,
       "overall_mean_sqr_errors" = mean(mean.sqr.errors),
       "sd_mean_sqr_errors" = sd(mean.sqr.errors))
}

loocv.reg.iter <- function(k, df){
  mod <- lm(MEDV~., data = df[-k,])
  pred <- predict(mod, df[k,])
  sqr.error <- (pred - df[k,"MEDV"])^2
  sqr.error
}

res <- loocv.reg(bh)
# Vemos cómo quedo el modelo 
res

# se observa que hay 506 iterasiones, y al fina esta el promedio de los errores cuadraticos








