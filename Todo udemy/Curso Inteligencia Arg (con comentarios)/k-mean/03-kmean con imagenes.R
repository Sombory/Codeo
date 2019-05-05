
install.packages(c("OpenImageR", "ClusterR"))
install.packages("httpuv")
install.packages("xtable")
library(xtable)
library(httpuv)
library(OpenImageR)
library(ClusterR)

# cargamos la imagen de mi disco
imagename <- "C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo/Nuevo udemy II/Datos Inteligencia-Artificial/r-course/data/tema5/bird.jpg"
img <- readImage(imagename)

#puedo redimensionar la imagen par que pese menos
img.resize <- resizeImage(img, 350, 350, 
                          method = "bilinear")
imageShow(img)

# armo una imagen vectorizada
img.vector <- apply(img, 3, as.vector)
dim(img.vector)

# Hacemos el K-mean, para agrupar el vector. Armo una matriz con 10 cluster
kmmb<-MiniBatchKmeans(img.vector, clusters = 10,
                      batch_size = 20, num_init = 5,
                      max_iters = 100, init_fraction = 0.2,
                      initializer = "kmeans++",
                      early_stop_iter = 10, verbose = F)
kmmb

# hacemos la predición pasando los colores en kmb$centroids
prmb <- predict_MBatchKMeans(img.vector, kmmb$centroids)

get.cent.mb <- kmmb$centroids
new.img <- get.cent.mb[prmb,]
dim(new.img) <- c(nrow(img), ncol(img),3)
imageShow(new.img)
