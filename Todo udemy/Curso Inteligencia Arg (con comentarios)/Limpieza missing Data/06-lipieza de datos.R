
data <- missing.data <- read.csv("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo/Nuevo udemy II/Datos Inteligencia-Artificial/r-course/data/tema1/missing-data.csv")

# Limpiar NA solamente en income
data.income.cleaned <- data[!is.na(data$Income)]

# Filas completas para data frame

complete.cases(data)
data.clean.2 <- data[complete.cases(data),]

# Convertir los ceros en NA en la col Income

data$Income[data$Income==0]<- NA

#medias sin NA


mean(data, na.rm=TRUE)


