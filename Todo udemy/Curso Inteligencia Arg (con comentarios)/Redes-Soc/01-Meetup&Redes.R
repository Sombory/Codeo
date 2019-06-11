
# Hacemos un analisis de la red social Meetup
# Luego de ingresar como usuario y de ingresar a la API: https://www.meetup.com/es-ES/meetup_api/

#API MEETUP
# Mis accesos
#Key: 7ojmpmeiffrh2vr5gsjps3prr9
#pass: ismggko1i4jadc7deqch5bebrg
34167b746b461c5578324f6b1b774128
# generamos una función mi pass
api_key <- "34167b746b461c5578324f6b1b774128"

# creamos una series de grupos para analizar
# Antes podemos ingresa a la web de Meetup: https://secure.meetup.com/meetup_api/console/
# ahi a la derecha "V1, v2, v3" q, son distintos grupos 
# Elijo x ej. https://api.meetup.com/2/groups ... cargo un tema, pais, ciudad, radio, etc y hago clic en "Show Response"

# Copio lo siguiente entonces a partir de este link: https://api.meetup.com/2/groups?&sign=true&photo-host=public&topic=mathematics&radius=1000&country=es&city=Madrid&page=20

topic <- "mathematics"
country <- "ES"
city <- "Madrid"
radius <- 50
fields <- "id,name,members"

url <- paste("https://api.meetup.com/2/groups?&topic=",topic,
             "&country=",country,
             "&city=",city,
             "&radius=",radius,
             "&only=",fields,
             "&key=",api_key,sep="")
url

# IMPORTANTE: desde esta Web se puede limpiar y ordenar un Json para procesar
# http://jsoneditoronline.org/
# serive para pasar los dtos de corridos de https://secure.meetup.com/meetup_api/console/?path=/2/groups
install.packages("jsonlite")
library(jsonlite)
# Copiando el json y luego guardandoló desde http://jsoneditoronline.org/ opcion "Save Disc"
meetup.data <- fromJSON("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo/Nuevo udemy II/Datos Inteligencia-Artificial/r-course/data/tema9/meetup-mathematics.json")
# luego cargamos la lista de elementos
groups <- meetup.data$results
head(groups) # este grupo solo tiene una fila
View(meetup.data)
meetup.data$results

# vamos a hacer una llamada a los datos
# En esta corrida por ejemplo extraigo los "members"
install.packages("RCurl")
install.packages("bitops")
library(bitops)
library(RCurl)

meetup.getUsers <- function(groups, api_key){
  users <- as.data.frame(NULL)
  for (i in 1:nrow(groups)) {
    url <- paste0("https://api.meetup.com/2/members?group_id=", groups$id[i],
                  "&only=id&key=",api_key)
    while (url!= "") {
      temp_json <- fromJSON(RCurl::httpGET(url))
      if(class(temp_json$results) == "data.frame"){
        tests <- cbind(group_id=groups$id[i],member_id=temp_json$results)
        users <- rbind(users,tests)
      }
      url <- temp_json$meta$`next`
    }
    print(paste0("Hemos recuprado los miembros del grupo ",i))
  }
  u <- data.frame(group_id = users$group_id, user_id = users$id)
  u
}

# vemos lo que nos genero.. Obtenemos 
embers <- meetup.getUsers(groups, api_key)

# NOTA:  SI ME DICE Q NO TENGO AUTORIZACION O "Bad Request"

# clono la versión anterior, en vez de optener los usuario obtenemos los "grupos" (getuser)
# uso el #topic
meetup.getGroups <- function(topic, api_key, 
                             country = "ES", city = "Madrid", radius = 100){
  groups <- as.data.frame(NULL)
  url <- paste0("https://api.meetup.com/2/groups?topic=", topic,
                "&country=",country,"&city=",city, "&radius=",radius,
                "&only=id,name,members&key=",api_key)
  while (url!= "") {
    temp_json <- fromJSON(RCurl::httpGET(url))
    if(class(temp_json$results) == "data.frame"){
      groups <- rbind(groups,temp_json$results)
    }
    url <- temp_json$meta$`next`
  }
  print(paste0("Hemos recuprado los grupos del tema ",topic))
  
  groups
}

# NOTA:  SI ME DICE Q NO TENGO AUTORIZACION O "Bad Request: siginifica q la peticion esta mal hecha"
groups <- meetup.getGroups("big data", api_key, radius = 1000)
groups <- meetup.getGroups("mathematics", api_key, radius = 1000)
groups

members <- meetup.getUsers(groups, api_key)
# veo la cantidad de miembros por grupo
members

hiking.data <- fromJSON("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo/Nuevo udemy II/Datos Inteligencia-Artificial/r-course/data/tema9/hiking-ny.json")
View(hiking.data)
hiking.groups <- hiking.data$results
hiking.members <- meetup.getUsers(hiking.groups, api_key)
View(hiking.data)

# se puede resudir la corrida // por ej. quedándose con los mas relevantes
# el data table es mas eficiente y rápida que el data.frame
install.packages("data.table")
library(data.table)
# filtro los miembros (creo un data.tabla a partir de data.framede members)
# Solo dale usuario con mas de 4 miembros SD[.N > 4]
users <- setDT(members)[, .SD[.N > 4], by = user_id]
View(users)
save(users, file="C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo/Nuevo udemy II/Datos Inteligencia-Artificial/r-course/data/tema9/meetup-users.Rdata")