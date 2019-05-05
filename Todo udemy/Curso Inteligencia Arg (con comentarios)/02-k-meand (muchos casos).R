
protein <- read.csv("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo/Nuevo udemy II/Datos Inteligencia-Artificial/r-course/data/tema5/protein.csv")
rownames(protein) = protein$Country
# asi solo tengo valores numericos
protein$Country = NULL
# normalizo
protein.scaled = as.data.frame(scale(protein))

# descargo esta librería para graficos complejos
library(devtools)
devtools::install_github("kassambara/factoextra")


km <- kmeans(protein.scaled,4)
km

# puedo aplicar la media de cada varible "mean" al km
# es decir obtengo el promedio de cada variable escalada
aggregate(protein.scaled, by = list(cluster = km$cluster), mean)

install.packages("factoextra")
install.packages("ggplot2")
library(ggplot2)
library(factoextra)
fviz_cluster(km, data = protein.scaled)
