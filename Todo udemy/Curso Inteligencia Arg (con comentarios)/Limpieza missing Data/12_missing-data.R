
housing <- read.csv("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo/Nuevo udemy II/Datos Inteligencia-Artificial/r-course/data/tema1/housing-with-missing-value.csv")

summary(housing)

# vemos en en rad y ptratio tenemos 40 NA
# entonces creamos una dataframe que las omita
housing.data1 <- na.omit(housing)
# ahora si, si vemos ya no hay mas NA
summary(housing.data1)

# pero al eliminar filas completas donde sólo hay un NA, te cambia el resto de las variables
# pero si sólo kiero elimiar las NA de ciertas Columnas
## ENTONCES si quiero sacar todos los NA de las filas, menos los de la variable rad por ejemplo deberia....
drop_na <- c("rad") #  de todas las q tiene NA , digo cuales kiero conservar
housing.data2 <- housing[
  complete.cases(housing[,!(names(housing))%in% drop_na]),]
# las filas donde puedo obtener lo contrario de complete.cases ..
# que no se encuentran en !(names(housing))%in% drop_na]
--------------------------------------------------------------------------------------------

### Eliminar toda la columna
housing$rad <- NULL
summary(housing)  


# esta libería permite remplazar valores NA con cualquier otro valor

install.packages("Hmisc")
library(Hmisc)

housing.data.copy <- housing
View(housing.data.copy)


# utilizamos una formula para rellenar los NA
# mediante impute podemos agregar algun calculo a alguna variable
housing.data.copy$ptratio <- impute(housing.data.copy$ptratio, mean)
# chequeamos que hayan kedado NA
summary(housing.data.copy)

# tambien puedo remplaza x otras formulan "median" o numeros q kiera asignar
housing.data.copy2 <- housing
housing.data.copy2$ptratio <- impute(housing.data.copy2$ptratio, 18)

# Existe otra libreria

install.packages("mice")
library(mice)

# puedo ver el patron de la información que falta
md.pattern(housing)

# dice q housing tiene 466 variables con filas donde todo es conocido, sin NA
# Consta de 40 valores donde se conocer todos los valores menos en la variable ptratio y vise versa

X crim zn indus chas nox rm age dis tax b lstat medv ptratio   
466 1    1  1     1    1   1  1   1   1   1 1     1    1       1  0
40  1    1  1     1    1   1  1   1   1   1 1     1    1       0  1
    0    0  0     0    0   0  0   0   0   0 0     0    0      40 40

# otra libreria muy util VIM, para visualizar los missing data
    
install.packages("VIM")
library(VIM)

aggr(housing)
# le agrego colores específicos
aggr(housing, col= c("green","red"))
# agregado una función para que nos ordene las variables faltantes de mayor a menor
# cex.axis tamaño de las letras (1 es standar > mas grande)
# espacio entre graficos
aggr(housing, 
     col= c("green","red"),
     numbers=TRUE,
     sortVars=T,
     cex.axis=1.0,
     gap= 0,8,
     ylab= c("Histograma de NAs", "Patrón"),
)



