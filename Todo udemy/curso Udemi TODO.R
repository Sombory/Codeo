
# cUROS DUDEMY

# REMPLAZAR DATOS
fin$Expenses <- gsub("Dollars", "", fin$Expenses)
fin$Expenses <- gsub(",", "", fin$Expenses)

# MISSING DATA
head(fin,24)
fin[!complete.cases(fin),]
?complete.cases
str(fin)

# FILTRAR DATOS na

fin[fin$Revenue == 9746272,]

# pero esto trae valores NA
#entonces agrego la finción  wich


which(fin$Revenue==9746272)


# tambien se puede filtrar eligiando que fila exacta

# fila
fin[c(3,4,5),]

#columna
fin[c(3,4,5)]


# si quiero elegir las empresas que tiene 45 empleados

fin$Employees==45
fin[fin$Employees==45,]

#saco los NA
fin[which(fin$Employees==45),]


# is.na

head(fin,24)

# formas poco eficientes


fin$Employees == NA
fin[(fin$Employees==NA),]



is.na(fin$Employees)

# Se busca específicamente las "filas", donde hay ""NA"" en Expenses

fin[is.na(fin$Expenses),]


# Removemos todos los NA

fin_backup <- fin


# primer buscamos todas las filas que tiene al menos 1 NA


fin[!complete.cases(fin),]
fin[is.na(fin$Industry),]

# si le agregro el signo ""!"" es lo OPUESTO

fin[!is.na(fin$Industry),]


# Entonces puedo eliminar esas filas con na


fin <- fin[!is.na(fin$Industry),]
fin


fin[is.na(fin$State),]
fin[is.na(fin$State) & fin$City=="New York",]

fin[is.na(fin$State) & fin$City == "New York", "State"] <- "NY"


head(fin, 12)


fin[is.na(fin$State),]
fin[is.na(fin$State) & fin$City == "San Francisco", "State"] <- "CA"

#chek
fin[c(84,267),]


# seguimos cargando las filas sin datos ... agregamos medias 

fin[!complete.cases(fin),]

#saco la media
median(fin[,"Employees"])

# como me aparece NA, porque no la calcula si hay un NA ... hago ...
median(fin[,"Employees"],na.rm=TRUE)

median(fin[fin$Industry=="Retail","Employees"],na.rm=TRUE)

#saco la media de Retail de Employees sin tener en cuenta la NA
med_empl_retail <-  median(fin[fin$Industry=="Retail","Employees"], na.rm = TRUE)
med_empl_retail

# busco lo NA de ratail
fin[is.na(fin$Employees) & fin$Industry=="Retail",]

#finalmente en los NA de Emplyees de retail agrego la media del sector (28) 
fin[is.na(fin$Employees) & fin$Industry=="Retail", "Employees"] <- med_empl_retail

# lo puedo ver en la fila 3
fin[3,]

#lo mismo sector financiero
med_empl_financial <-  median(fin[fin$Industry=="Financial Services","Employees"], na.rm = TRUE)
fin[is.na(fin$Employees) & fin$Industry=="Financial Services", "Employees"] <- med_empl_financial
mfin[complete.cases(fin$Industry=="Financial Services"),]

fin[fin$Industry=="Financial Services",]

# VAMOS A CONSTRUCCIÓN

fin[!complete.cases(fin$Growth),]

median(fin[fin$Industry=="Construction","Growth"], na.rm=TRUE)
media_growth_constr <- median(fin[fin$Industry=="Construction","Growth"], na.rm=TRUE)
fin[is.na(fin$Growth) & fin$Industry=="Construction", "Growth"] <- media_growth_constr

fin[complete.cases(fin$Growth),]
fin[fin$Industry=="Construction",]

#con revenue
med_rev_constr <- median(fin[fin$Industry=="Construction", "Revenue"],na.rm=TRUE)
med_rev_constr
fin[is.na(fin$Revenue) & fin$Industry=="Construction","Revenue"] <- med_rev_constr

# con expense
fin$Expenses <- as.numeric(fin$Expenses)
med_exp_constr <- median(fin[fin$Industry=="Construction", "Expenses"],na.rm=TRUE)
med_exp_constr

fin[is.na(fin$Expenses) & fin$Industry=="Construction"& is.na(fin$Profit),]

fin[is.na(fin$Expenses) & fin$Industry=="Construction" & is.na(fin$Profit), "Expenses"] <- med_exp_constr

fin[!complete.cases(fin),]

fin[is.na(fin$Expenses),"Expenses"]  <- fin[is.na(fin$Expenses), "Revenue"] - fin[is.na(fin$Expenses), "Profit"]

fin[!complete.cases(fin),]

fin[is.na(fin$Profit),"Profit"]  <- fin[is.na(fin$Profit), "Revenue"] - fin[is.na(fin$Profit), "Expenses"]

#Eliminto estas dos filas 
fin <- fin[-c(14,15),]


# libreria ggplot


install.packages("ggplot2")
library(ggplot2)

gglplot2


library(gglo)

p <- ggplot(data=fin)
p

p + geom_point(aes(x=Revenue, y=Expenses,colour=Industry,size=Profit))

p1 <- p + geom_smooth(aes(x=Revenue, y=Expenses,colour=Industry,size=Profit))
p1

#Scarter PLot

d <- ggplot(data=fin, aes(x=Revenue, y=Expenses,colour=Industry))
d+ geom_smooth(fill=NA, size=1.2)

d + geom_point() + geom_smooth(fill=NA, size=1.2)

# Boxplot

f <- ggplot(data=fin, aes(x=Revenue, y=Expenses,colour=Industry))

f + geom_boxplot(size=1.5)

# extra

f + geom_jitter() + geom_boxplot(size=1, alpha=0.5, outlier.color = 5)


View(fin_backup)

fin2 <- fin

View(fin2)

## repaso 
#You have a dataframe df and you have just removed some 
#rows from it. Which of the following lines will help you resent
#the dataframe index (the row names)?
rownames(fin2) <- RESET


#####################################################################
## SEGUNDA SECIÓN #########################################
#########################################################

util <- read.csv("C:/Users/ezequiel.eliano/Desktop/Codeo R/Machine-Utilization.csv")

head(util,12)
str(util)
summary(util)

#  agregamos una columna para ver la utilización

util$Utilization =  1- util$Percent.Idle


# Handing data-time in R
# poner el ultimo día del mes de cada serie 
tail(util)
?POSIXct

# agregamos esta columna con este formato
util$PosixTime <- as.POSIXct(util$Timestamp, format="%d/%m/%Y %H:%M")

head(util,14)
summary(util)

# TIP COMO ARRLEAR EN EL DATAFRAME COLUMNAS DE df:

util$Timestamp <- NULL

util

# Ahora reacomodamos el dataframe, tonemos la columna 4 al principio
util <- util[,c(4,1,2,3)]
head(util,12)

# que es una lista

summary(util)
#filtro la makina RL1
RL1 <- util[util$Machine=="RL1",]

RL1$Machine <- factor(RL1$Machine)
summary(RL1)

# Construir una lista

# Character: Macrine name
# Vector: (min, mean, max) utilización for the moth 
# Logical tener que utilizar inclusive cuando falla en un 90% TRUE/FALSE

# aRMO EL MINIMO DE utilizacion de la maquina RL1 ¡ excluyo los NA
util_stats_rl1 <- c(min(RL1$Utilization, na.rm =TRUE),
                    mean(RL1$Utilization, na.rm =TRUE),
                    max(RL1$Utilization, na.rm =TRUE))
util_stats_rl1

# hago el listado de todo los que cumple o no
RL1$Utilization < 0.9

#  si quiero ver solo los que cumplen con la condición
which(RL1$Utilization < 0.9)

# quiero saber cuantos espisodios hay
length(which(RL1$Utilization < 0.9))

# armamos el rango 
until_under_90_flag <- length(which(RL1$Utilization < 0.9)) > 0
until_under_90_flag

# armo un lista con los 3 objetos creados antes
list_rl1 <- list("RL1", until_under_90_flag, util_stats_rl1)

list_rl1
names(list_rl1) <- c("Machine", "Stats", "LowThreshold")
list_rl1  

# otro camino
rm(list_rl1)
list_rl1
list_rl1 <- list(Machine="RL1", Stats=until_under_90_flag, LowTtreshold=util_stats_rl1)
list_rl1

# Extraer componentes de la lista
# 3 caminos
# [] vamos a retornar las listas
## [[]] vamos a retornar al objeto actual
#$ igual a [[]] pero facil
list_rl1

# vamos a la lista
list_rl1[1]

# vamos al objeto
list_rl1[[1]]

# ortra forma
list_rl1$Machine

list_rl1[[2]]
list_rl1[[3]]

typeof(list_rl1[2])
typeof(list_rl1[[3]])

# cómo accedo al tercer elemento del segundo vectos
list_rl1[2][3]
list_rl1[[2]]
list_rl1

# quiero entrar a un elemento de la lista entro al tercer vector [3] y luego al 2do dato [2]
list_rl1[[3]][2]
list_rl1[[3]][3]

# otra forma
list_rl1$LowTtreshold[3]

#Si quier agregar una columna
list_rl1[4] <- "New Information"
list_rl1

list_rl1 <- list_rl1[c(1,3,2,4),]
list_rl1

# quiero ver los NA

# veo específicamente cuales col tiene NA
RL1[is.na(RL1$Utilization),]

# quiero analizar todo el data.frame
is.na(RL1$Utilization)

RL1[is.na(RL1$Utilization),"PosixTime"]

#Si lo quiero agregar a la lista

list_rl1$UnkonownHours = RL1[is.na(RL1$Utilization),"PosixTime"]
list_rl1

# remover algo


list_rl1[4] <- NULL
list-_rl1

#Data frame para esta maquina
list_rl1$Data <- RL1
list_rl1
summary(list_rl1)

#subsetting


list_rl1

list_rl1[[4]][1]
list_rl1$UnkonownHours[1]
list_rl1$UnkonownHours


# quiero ver algun elemento de la lista ejemplo del 1 al 2
list_rl1[1:2]

# ver el 1 y 4
list_rl1[c(1,4)]


# kiero busca mediante los nombres de las listas
sub_list_rl1 <-  list_rl1[c("Machine", "LowThreshold")]
sub_list_rl1

#quiero ir directamente al elemento 
sub_list_rl1[[2]][2]
sub_list_rl1$LowThreshold[2]

# construir una serie de tiempo "plot"

install.packages("ggplot2")


library(ggplot2)


p <- ggplot(data = util)
myplot <- p + geom_line(aes(x=PosixTime, y=Utilization, colour=Machine), size=1.2) +
  facet_grid(Machine~.) + geom_hline(yintercept = 0.9, colour="gray", size=1.2,linetype=3)

#puedo agregar el grafico a mi lista


list_rl1$Plot <- myplot
list_rl1
str(list_rl1)

list_rl1

###########################################################3
###########################################################

########## NUEVE SECCIÓN #################################
#########################################################

getwd()
setwd("C:/Users/ezequiel.eliano/Desktop/Codeo R")

# otra forma
set("./Codeo R")

# leo el cvs de chicago y le agrego row.name=1 para q me lea desde la primera columna
Chicago <- read.csv("Chicago-F.csv", row.names = 1)
NewYork <- read.csv("NewYork-F.csv", row.names = 1)
Houston <- read.csv("Houston-F.csv", row.names = 1)
SanFrancisco <- read.csv("SanFrancisco-F.csv", row.names = 1)

# chequeo: ¿Hay un dataframe

is.data.frame(Chicago)

# Ahora kiero convertir en una Matrix
Chicago <- as.matrix(Chicago)
NewYork <- as.matrix(NewYork)
Houston <- as.matrix(Houston)
SanFrancisco <- as.matrix(SanFrancisco)

# chequeo
is.matrix(Chicago)

# ahora vamos a poner todo en una lista
Weather <- list(Chicago=Chicago,NewYork=NewYork,Houston=Houston,SanFrancisco=SanFrancisco)
Weather

#quiero selecional cualquier elemento dentro de la lista y sub lista
Weather[[3]][7]
Whather$Houston

## vamos a utlizar la funcion apply

# antes un poco de teoría

# Si tengo una matriz apply(M,1,mean) te genera una media en cada "fila"¡
# en cambio si la formula es = apply(M,2,mean) te genera una media en cada "COLUMNA"¡


# formulas de Apply

 # >>apply: es para matrices
 # >>tapply: para hacer lo mismo extrayendo un vecto
 # >>lapply: es para aplicar a elementos de una "Lista"
 # << sapply: es una version simplificada de lapply
############################

# volvemos a las matrices cargadas

#quiero calcular la mediade la fila (1=fila)
apply(Chicago, 1, mean)
Chicago
#chek, viendo 1 fila en particular
mean(Chicago["DaysWithPrecip",])

#quiero calcular la mediade la fila (2=Columna)
apply(Chicago, 2, mean)

#  creando un loop
# encuantro la mediana en cada fila
#1. via loop

output <- NULL # preparando un vector vacío
# armamos el loop para cada fila de la 1 a la 5 (i in 1:5), luego sacarmos la media
#en cada fila de la matri de Chicago (Chicago[i,1])

for(i in 1:5){ # correr el ciclo
  output[i] <- mean(Chicago[i,])
} 

output # vamoa a ver que tiene

# les argregamos los nombres
names(output) <- row.names(Chicago)
output

## Via 2. Lo hacemos x un camino mas rapido 
apply(Chicago,1,mean)


# AHORA vemos ""lapply"" ( aplica una funcion a un vector)
# transpongo
t(Chicago)

# tambien puedo transponer entrando en la lista
t(Weather$Chicago)

# pero si kiero hacer todos a la vez, es + facil así:
lapply(Weather,t) # t of chicago , t of New York etc...

#agrego una fila mas a la matriz con rbind
rbind(Chicago, Newrow=1:12)

# puedo hacer lo mismo en la lista
lapply(Weather,rbind,NewRow=1.12)

#otro ejemplo
rowMeans(Chicago)
apply(Chicago,1, mean)

lapply(Weather,rowMeans)
#Hay otras
#rowMeans
#colMeans


# Combinar lapply with the [ ] Operator

Weather

Weather$Chicago[1]
Weather[[1]][1,1]

# utlizo "[" par realizar la acción de corte
#quiero ver la temperatura de todos los enero de todas las ciudades
lapply(Weather, "[",1,1)

#quiero ver todo la tempetaruta
lapply(Weather,"[", 1,) 

# quiero ver todos los datos de marzo de todas las ciudades


lapply(Weather,"[", ,3)



# Anidar formulas


lapply(Weather,rowMeans)

# kiero ver la primer fila 1 por ejemplo = x[1,]
lapply(Weather,function(x) x[1,])

# si quiero ver la columna numero 12
lapply(Weather,function(x) x[,12])

# Ahora tambien puedo agregar funciones (hago la dif de temperatura en c/ciudad de la primera fila menos la segunda)
lapply(Weather,function(z) z[1,]-z[2,])

# Usar sapply

# media High_F de julio
lapply(Weather,"[", 1,1)

# ahora aplico la media
# hago una especie de transposición //uso saplly que de devuelve como una matriz con la info mas comprimida
sapply(Weather,"[", 1, 7)

# kiero hacer la media del ultimo cuarto
lapply(Weather,"[", 1, 10:12)
sapply(Weather,"[", 1, 10:12)

# otro ejemplo
lapply(Weather,rowMeans)
sapply(Weather,rowMeans)

#redondeo a dos decimales
round(sapply(Weather,rowMeans),2)

# Otro ejemplo
lapply(Weather,function(z) round(z[1,] - z[2,]/z[2,],2))

sapply(Weather,function(z) round(z[1,] - z[2,]/z[2,],2))
# sin redondeo
sapply(Weather,function(z) z[1,] - z[2,]/z[2,])

# puedo usar sapply como lapply con "simplify = F"

sapply(Weather,rowMeans,simplify = F)
# PERO LO PUEDO USAR SACANDO EL SIMPLIFY O 
sapply(Weather,rowMeans,simplify = T)

# Anidando apply Funtions

lapply(Weather,rowMeans)
sapply(Weather,rowMeans)


apply(Chicago,1, max)

#apply across whole 

lapply(Weather,apply,1,max) # recomendado

#otra forma con function
lapply(Weather,function(x) apply(x,1,max))


# ahora armo la matriz
# Entonces tengo 

sapply(Weather,apply,1,max)

## muy avanzado tutoria
#wich.max Me dice cual es el mayor dentro de un rango de valor

which.max(Chicago[1,])
Weather
# reviso cual es el nombre
names(which.max(Chicago[1,]))
Chicago # se ve que julio es la fila con el numero mas alto


## vamos a hacer una iteraci[on] buscamos los nombres donde est[an] los maximos en Chicago

apply(Chicago,1,function(x) names(which.max(x)))
lapply(Weather, function(y) apply(Chicago,1,function(x) names(which.max(x))))

# armo la matriz
sapply(Weather, function(y) apply(Chicago,1,function(x) names(which.max(x))))


      