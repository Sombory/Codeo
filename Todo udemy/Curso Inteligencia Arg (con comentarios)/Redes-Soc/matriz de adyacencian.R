
#Representación de grafos
install.packages("Matrix")
load("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo/Nuevo udemy II/Datos Inteligencia-Artificial/r-course/data/tema9/meetup-hiking.Rdata")
View(users)
# Podemos ver los usuarios unicos
unique(users$group_id)
unique(users$user_id)

# armo una estructura sparseMatrix. Es un estructura muy buena p/analizar datos
library(Matrix)
# ScparseMatrix es una forma de guardar la info
group_membership = sparseMatrix(users$group_id, users$user_id, x=T)
head(group_membership)
# Se multiplican las 2 matrizes la trans. y no transp (p/mult. se hace con porcentaje %)
adjacency <- t(group_membership) %*% group_membership
#  donde los usuarios estan tanto en filas como en col. Y el numre de elemenos q compartes son la cantidad de gripos q hay en la matriz   
head(summary(adjacency))

# hacemos una matriz, p/ donde en las absisas estan los grupos..y tratamos de ver cuanto miembros se comparte en cada grupo
users_edgelist <- as.data.frame(summary(adjacency))
summary(users_edgelist)

# como nos sobre información x q se repite filtramos de la siguiente manera
# Me quedo sólo con los users de "j" mayores a "i". Luego pongo la coma p/ q aplique en todas las col
# Asi elimino los casos donde J es = i y donde j es menor a i
users_edgelist.upper <- users_edgelist[users_edgelist$i<
                                         users_edgelist$j,]
# Ahora puedo trabajar con objetos mas filtrados y menos pesados
save(users_edgelist.upper, file = "C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo/Todo udemy/Curso Inteligencia Arg (con comentarios)/Redes-Soc/meetup-hiking-edgelist.Rdata")
