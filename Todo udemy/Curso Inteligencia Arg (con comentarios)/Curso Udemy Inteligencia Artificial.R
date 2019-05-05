
setwd("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo-R/Inteligencia-Artificial/r-course/data/tema1")

auto <- read.csv("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo-R/Inteligencia-Artificial/r-course/data/tema1/auto-mpg.csv", header = T, sep = "")

# Cómo screapear una página

who_from_internet <- read.csv("https://frogames.es/course-contents/r/intro/tema1/WHO.csv")
view(who_from_internet)

# instalo un paquete para screpear "XML"  (tmb puede ser en json)
install.packages("XML")
library(XML)

# vamos a crear una variable donde tengo el fichero

url <- "C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo-R/Inteligencia-Artificial/r-course/data/tema1/cd_catalog.xml"

# Hago le screpeo y despues 
xmlDoc <- xmlParse(url) # XMLInternalDocument
rootnote <- xmlRoot(xmlDoc) # me sirve para obtener los datos

# se puede consutar
rootnote[2]

# fabircamos un CSV / Dataframe
cds_data <- xmlSApply(rootnote, function(x) xmlSApply(x, xmlValue))

# Lo transponemos
cds.catalog <- data.frame(t(cds_data), row.names = NULL)


# Ahora vamos a trabajar con HTLM

# xpathSapply()
# getNodeset()


population_url <- "C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo-R/Inteligencia-Artificial/r-course/data/tema1/WorldPopulation-wiki.htm"

# generamos una tabla que tiene otras tablas (es decir screapiamos todo el ontenido de la pagina)
tables <- readHTMLTable(population_url)


# como es una lista de listas puedo aceder a una lista en particular

most_populate <- tables[[6]]
head(most_populate,3)

# kiero armar un 
custom_table <- readHTMLTable(population_url, which = 6)
View(custom_table)

# para leer json

install.packages("jsonlite")
library(jsonlite)

dat.1 <-  fromJSON("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo-R/Inteligencia-Artificial/r-course/data/tema1/students.json")
dat.2 <-  fromJSON("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo-R/Inteligencia-Artificial/r-course/data/tema1/student-courses.json")


# leemos json 



url <-  "https://finance.yahoo.com/webservice/v1/symbols/allcurrencies/quote?format=json"
#  o 
url <- "http://www.floatrates.com/daily/usd.json"
currencies <- fromJSON(url)

install.packages("curl")
library(curl)

currency.data <- currencies$list$recourses$recourse$fields
dat.1$Email

# como llamo a la información 
head(dat.1,3)
currency.data[1:5,1:2]
dat.1[c(2,5,8),]





# existe toJson, que sirve para








