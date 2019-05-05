
install.packages("tidyr")
library(tidyr)

# importo el csv de arrestos en USA
crime.data <- read.csv("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo/Nuevo udemy II/Datos Inteligencia-Artificial/r-course/data/tema1/USArrests.csv")
# o mas coroto
crime.data <- read.csv("../Codeo R/Codeo/Nuevo udemy II/Datos Inteligencia-Artificial/r-course/data/tema1/USArrests.csv")

# agrego una columna adicional "state" igual a la de rownames
crime.data <- cbind(state= rownames(crime.data),crime.data)

# quiero comprimir el dataframe y poner en una misma columna 3 variables de Muerder a Urban pop
# estas 3 varaibles van en key ="crime type" y los datos q habia en cada variable van en value="errest_estimate"
crime.data1 <- gather(crime.data, 
                      key = "crime type", 
                      value = "arrest_estimate", 
                      Murder: UrbanPop)
View(crime.data1)

# quiero juntar todas las variables menos "state"
crime.data2 <- gather(crime.data,
                      key = "crime type",
                      value = "arrest_estimate",
                      -state)
View(crime.data2)
crime.data2

# quiero poner en una misma columna "determinadas" columnas
crime.data3 <- gather(crime.data,
                      key = "crime type",
                      value= "arrest",
                      Murder, Assault)

# tambien poder volver a expandir 
crime.data4 <- spread(crime.data2,
                      key = "crime type",
                      value = "arrest_estimate")

# te puede fusionar 2 columnas (sirva mas para nombres)
crime.data5 <- unite(crime.data,
                     col = "Murder_Assault",
                     Murder, Assault,
                     sep = "_")

# tambien se puede separar : le indicamos que columna separar y con que criterio (separar despues de "_" en dos columnas Muerdey y Assault)
crime.data6 <- separate(crime.data5,
                        col =Murder_Assault,
                        into = c("Murder","Assailt"),
                        sep = "_")

