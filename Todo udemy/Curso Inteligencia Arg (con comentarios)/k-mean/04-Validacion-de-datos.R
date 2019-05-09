install.packages("DeoptimR")
install.packages("DEoptimR")
install.packages("fpc") # no me carga
install.packages("NbClust")

library(DEoptimR)
library(factoextra)
library(cluster)
library(fpc)
library(NbClust)

protein <- read.csv("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo/Nuevo udemy II/Datos Inteligencia-Artificial/r-course/data/tema5/protein.csv")
rownames(protein) = protein$Country
protein$Country = NULL
protein.scaled <- as.data.frame(scale(protein))

# genero un grafico para ver cual es el numero optimo de clúster
# creo XXX , donde puedo indicar el max y min numero de cluster
nb <- NbClust(protein.scaled, distance = "euclidean",
              min.nc = 2, max.nc = 12, 
              method = "ward.D2", index = "all")
# nos dice cual es el número óptimo de cluster

#* Among all indices:                                                
#* 6 proposed 2 as the best number of clusters 
#* 7 proposed 3 as the best number of clusters ### es el cluster optomo
#* 3 proposed 6 as the best number of clusters 
#* 1 proposed 7 as the best number of clusters 
#* 1 proposed 10 as the best number of clusters 
#* 5 proposed 12 as the best number of clusters

# grafico esto 
fviz_nbclust(nb) + theme_minimal()
 
km.res <- kmeans(protein.scaled, 3)
sil.km <- silhouette(km.res$cluster, 
                     dist(protein.scaled))

sil.sum <- summary(sil.km)
sil.sum

# Vemos al silueta del Clusters
# Esto nor sirve para analizar el analisis de la silueta de un k-mean
# Si la var. Width¡ esta mar cerca de uno mejor explica el clúster el modelo
fviz_silhouette(sil.km)

fviz_cluster(km.res, data = protein.scaled)

dd <- dist(protein.scaled, method = "euclidean")

km_stats <- cluster.stats(dd, km.res$cluster)
km_stats$within.cluster.ss
km_stats$clus.avg.silwidths
km_stats$dunn

km_stats$corrected.rand
km_stats$vi

kmed <- pam(protein.scaled, 3)
sil.kmed <- silhouette(kmed$clustering, 
                       dist(protein.scaled))
fviz_cluster(kmed, data = protein.scaled)

fviz_silhouette(sil.kmed)

kmed_stats <- cluster.stats(dd, kmed$clustering)
kmed_stats$within.cluster.ss
kmed_stats$clus.avg.silwidths
#es el cociente entre la distancia mas pequeñas de observasiones q no estan el mismo cluster y  la mayor distancia q hay de elementros dentro de 1 cluster 
# a este indicador se busca maximizarlo ... significa o q los cluster estan bien separados o que las distancia entre los
# elementos dentro de un cluster e muy chico... lo cual habla de 1 buen modelo.
kmed_stats$dunn

kmed_stats$corrected.rand

kmed_stats$vi

# puedo comparar dos tipo de indices
res.com <- cluster.stats(dd, km.res$cluster, 
                         kmed$clustering)
res.com$corrected.rand
res.com$vi



