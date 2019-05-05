# creando ficheros

clientes <- c("Juan Gabriel","Ricardo","Pedro")
fechas <- c("2017-12-27", "2017-11-1", "2018-1-1")

fechas <- as.Date(c("2017-12-27", "2017-11-1", "2018-1-1"))
pago <- c("315","195.55","40.15")
pedidos <- data.frame(clientes,fechas,pago)


# indicamos q cuadamos un objeto de R en 1 lugar determinado
### IMPORTNATE ¡¡ CÓMO GUARDAR UN ARCHIVO EN FORMATO R

save(pedidos,file = "C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo-R/Inteligencia-Artificial/r-course/data/tema1/pedidos.Rdata")

# Otra opción RDS

saveRDS(pedidos, file= "C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo-R/Inteligencia-Artificial/r-course/data/tema1/pedidos.rds")


## esta función permite guardar una infinida de Dataframe "pedidos" ... "" y otras.

# tambien podemos eliminar ciertos data Frame.
remove(pedidos)


# ahora que pasa si tengo mi fichero vación y kiero cargar los archivos q acabo de crear

load("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo-R/Inteligencia-Artificial/r-course/data/tema1/pedidos.Rdata")

# para levantar RDS
orders <- readRDS("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo-R/Inteligencia-Artificial/r-course/data/tema1/pedidos.rds")

data("iris")
iris
data("cars")
cars


# puedo guardar todos los ojetos y dataframe --> save.image --

setwd("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo-R/Inteligencia-Artificial/r-course")
save.image(file= "C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo-R/Inteligencia-Artificial/r-course/data/tema1/alldata.Rdata")



