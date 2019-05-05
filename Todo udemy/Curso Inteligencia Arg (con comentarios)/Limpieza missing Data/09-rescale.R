
install.packages("Scale")
library(scales)

### Modelo de Normalización
## Esta Normalización por defecto 0 a 1 
# Agrego una columna normalizando el Ingero meidante la formula rescale¡

students$Income.rescale <- rescale(students$Income)

# Si quiero NORMALIZAR dentro de un rango; poe ejemplo 0 a 100

rescale(students$Income, to= c(0,100))

# voy a hacer un bucle

#primero digo donde applicare mi funcion ... en todo el "dataframe"
# LUEGO digo que quiero estraer los nombres del dataframe
# luego le digo como corro el bucle en en dada columna
# al nombre generado le agredo la palabra "rescale y la separo por un punto
# finalmente rescalo o NORMALIZO en todas las columnas
# indicamos 
rescale.many <- function(dataframe, cols){
  names <- names(dataframe)
  for(col in cols){
    name <- paste(names[col],"rescaled", sep=".")
    dataframe[name] <- rescale(dataframe[,col])
  }
  cat(paste("Hemos Rescalado", length(cols), "variable(s)"))
  dataframe
}

students <- rescale.many(students, c(1,4))


dataframe



