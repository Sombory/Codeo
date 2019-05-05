
library(mice)

housing.data <- read.csv("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo/Nuevo udemy II/Datos Inteligencia-Artificial/r-course/data/tema1/housing-with-missing-value.csv",
header=T, stringsAsFactors=T)

columns <- c("ptratio","rad")

# sirve para gestionar  los datos que "faltan", haciendolo con tecnica multicariante
# m = cantidad de numero, maxit = cantidad de iteraciones
# method = pmm, q es el prediting, min machine: 
imputed_data <- mice(housing.data[,names(housing.data) %in% columns],
                     m=5,
                     maxit = 50,
                     method = "pmm",
                     seed = 2018)
View(imputed_data)
# OTROS "method"
# pmm - Comparacion predictiva de medias
# logreg - regresión logística
# polyred - regresión logística politómica
# polr - modelo de probabilidad proporcionales
# IMPORtANTE pude haber funciones de ej. "complete"
# q son de librerias distintas, para ello se puede poner de q pakete pertenece ""mice:FUNCION"
# esta funcion remplaza donde hay NA por datos iterados antes
complete.data <- mice::complete(imputed_data)
View(complete.data)

housing.data$ptratio <- complete.data$ptratio
housing.data$rad <- complete.data$rad

# quiero ver si hay mising data. Si me da false es porque no hay NA
anyNA(housing.data)


## hacemos una iteración automatica
housing.data <- read.csv("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo/Nuevo udemy II/Datos Inteligencia-Artificial/r-course/data/tema1/housing-with-missing-value.csv",
                         header=T, stringsAsFactors=T)

---------------------------------------------------------------------------------------
library(Hmisc)
# CALCULA MEDIANTE ITERACIONES LOS DATOS FALTANTES ()

# Multiple Imputation using Additive Regression, Bootstrapping, and Predictive Mean Matching

impute_arg <- aregImpute(~ptratio + rad, data=housing.data, n.impute = 5)
# QUEREMOS VER EL NIVEL DE CONFIABILIDAD (EL R2 DEL VALOR, MIENTRAS MAS ALTA MEJOR¡)
impute_arg
# COMO PODEMOS VER: vemos en ptratio una matriz con las 5 interaciiones realizadas en cada fila de NA
impute_arg$imputed$ptratio


