
# base de datos de consumo de proteinas de paises de Europa
protein <- read.csv("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo/Nuevo udemy II/Datos Inteligencia-Artificial/r-course/data/tema5/protein.csv")

# Normalizo  ( z= (xi - Xmedia)/desvio) las variables (menos la primera col [,-1])
data <- as.data.frame(scale(protein[,-1]))
View(data)

# luego le añado la columna de paises
data$Counthy <- protein$Country
# Le agrego los nombres al dataframe p/ q se vean el grafico
rownames(data) = data$Counthy

#hacemos un clustering AGLOMERATIVO .. 
# armo el cluster y utilizo la funsión dist q mide la distancia entre variables
# Utilizamos el método ward.D2 que es el de minima varianza
hc <- hclust(dist(data, method = "euclidean"), method = "ward.D2")
plot(hc, names= data$Counthy, hang= -0.01, cex=0.7)

hc2 <- hclust(dist(data, method = "euclidean"),
                   method = "single")

plot(hc2, hang= -0.01, cex=0.7)
plot(hc2, cex=0.7)

# Ahora analizamos la matri "dist"
d <- dist(data, method = "euclidean")
# Esta matriz te da la distancia Euclidea entre los paises de la matriz
d

# ejemplo entre Albania y Australia la distancia es 6

#Esto quiere decir que si hacemos
alb <- data["Albania",-10]
aus <- data["Austria",-10]
sqrt(sum(alb-aus)^2)

# Esto me daria lo mismo que la matriz

# luego existe el metodo manhattan q es el mismo calculo, pero si sacar la raiz
dman <- dist(data, method = "manhattan")

#### COTINUA CON OTRAS METODOLOGIAS

fit <- cutree(hc, k=4)
table(fit)
rect.hclust(hc, k=4, border="red")

hc2 <- hclust(dist(data, method = "euclidean"),
              method = "single")
plot(hc2, hang=-0.01, cex = 0.7)

hc3 <- hclust(dist(data, method = "euclidean"),
              method = "complete")
plot(hc3, hang=-0.01, cex = 0.7)
hc3$merge

hc4 <- hclust(dist(data, method = "euclidean"),
              method = "average")
plot(hc4, hang=-0.01, cex = 0.7)
hc4$merge

d <- dist(data, method = "euclidean")

d 

alb<-data["Albania",-10]
aus<-data["Austria",-10]
sqrt(sum((alb-aus)^2))
sum(abs(alb-aus))


install.packages("cluster")
library(cluster)
dv <- diana(data, metric = "euclidean")
par(mfrow=c(1,2))
plot(dv)

#keremos dividir el dendo grama


fit <- cutree(hc, k=4)
# me ordena en una tabla la cantidad de paises que hay en cada grupo
table(fit)

# los representamos graficamente
rect.hclust(hc, k=4, border = "green")


 



