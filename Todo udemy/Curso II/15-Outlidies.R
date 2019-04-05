

ozone <- read.csv("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo/Nuevo udemy II/Datos Inteligencia-Artificial/r-course/data/tema1/ozone.csv", stringsAsFactors=FALSE)

ozone.data <- ozone

# Vamos a tratar de detectar valores que se encuentres fuera de ciertos rangos
# genera un diagrama de caja y se ve facilmente q valores estan fuera 
outlier_value <- boxplot(ozone.data$pressure_height,
                         main= "Presure Height",
                         boxwex = 0.5)$out
# para ver la dispersión de los datos a lo largo de los meses
# pongo al final $out para ver los valores
boxplot(pressure_height ~ Month, 
        data = ozone.data,
        main= "Presure Height per Month"
        )$out
# puedo poner los outlider encima de la cada
mtext("480 5410 5350 5320 5570 5640 5640 5500")

# replazo los cuantiles q estan x debajo del 0,05 x la mean y los que estan x encima x 0,95 
impute_outliers <- function(x, removeNA = T){
  quantiles <- quantile(x, c(0.05, 0.95), na.rm = removeNA)
  x[x<quantiles[1]] <- mean(x, na.rm = removeNA)
  x[x>quantiles[2]] <- median(x, na.rm = removeNA)
  x
}   

imputed_data <- impute_outliers(ozone.data$pressure_height)


# con la función "par", que sirve para estructurar los graficos uno alado del otro
par(mfrow= c(1,2))
boxplot(ozone.data$pressure_height, main="Presion con Outlier")
boxplot(imputed_data, main= "Presion sin Outlier")

---------------

remplace_outliers <- function(x, removeNA = TRUE){
    qrts <- quantile(x, probs = c(0.25, 0.75), na.rm = removeNA)
    caps <- quantile(x, probs = c(.05, .95), na.rm = removeNA)
    iqr <-  qrts[2]-qrts[1]
    h <- 1.5* iqr
    x[x<qrts[1]-h] <- caps[1]
    x[x>qrts[2]+h] <- caps[2]
    x
}

cappet_pressure_height <- remplace_outliers(ozone.data$pressure_height)

par(mfrow= c(1,2))
boxplot(ozone.data$pressure_height, main="Presion con Outlier")
boxplot(cappet_pressure_height, main= "Presion sin Outlier")


