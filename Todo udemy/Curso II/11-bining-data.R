
# Breack points 
# Generamos una clasificación de los datos
#antes elimino las columnas que había creado
students <- subset(students, select = -Height.rescaled)

# le digo que extremos y que media en la campana de gaus tendra mi nueva columna
bp <- c(-Inf, 1000, 31000, Inf)

# le asigno los combres a las 3 categorias
names <- c("Low", "Average","Hight")


# genero la nueva columna q se llama "Income.cat" 
students$Income.cat <- cut(students$Income, breaks = bp, labels = names)

# otra forma es asignar el nombre directamente en la formula "cut"... abro en 4 segmentos


students$Income.cat2 <- cut(students$Income, 
                            breaks = 4, 
                            labels= c("lev1","lev2","lev3","lev4")
                            )




