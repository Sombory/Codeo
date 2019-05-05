

data <- read.csv("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo/Nuevo udemy II/Datos Inteligencia-Artificial/r-course/data/tema1/missing-data.csv", na.strings = "")


# voy a crear un row de la siguiente manera

data$Income.mean <- ifelse(is.na(data$Income), 
                           mean(data$Income, na.rm = T),
                           data$Income
                           )

################################
# QUIERO REMPLAZAR LOS NA POR VALORES ALEATORIOS EN VEZ DE MEDIAS
################################

# vuelvo a carcar el dataframe

# Armo un vector X q contiene NA
rand.impute <- function(X){
  # missin contiene un vector con T/F dep del NA de X
  missing <- is.na(X)
  # contamos los vectores con NA
  n.missing <- sum(missing)
  # x.obs son los valores "CONOCIDOS" que tiene data diferente de NA de X
  x.obs <- x[!missing]
  # por defecto, devolverá lo mismo que habia entrado por parámetro
  imputed <- x
  # en los valores que faltaban, los remplazo x 1 muestra de los val mas
  imputed[missing] <- sample(x.obs, n.missing, replace=TRUE)
  return(imputed)
}

random.impute.data.frame <- function(dataframe,cols){
  names <- names(dataframe)
  for(col in cols){
    name <- paste(names[col],"imputed", sep=".")
    dataframe[name] = rand.impute(dataframe[,col])
  }
  dataframe  
}

data <- read.csv("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo/Nuevo udemy II/Datos Inteligencia-Artificial/r-course/data/tema1/missing-data.csv", na.strings = "")
data <- random.impute.data.frame(data,c(1,2))
View(data)

