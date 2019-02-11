
# como hago para importar ficheros de anchura fija

students_data <- read.fwf("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo-R/Inteligencia-Artificial/r-course/data/tema1/student-fwf.txt",widths = c(4, 15, 20,15,4), col.names = c("ID","nombre","email", "carrera","año"))
View(students_data)

# Si quierio cargar sin una variable 

# r
students_data_sin_mail <- read.fwf("C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo-R/Inteligencia-Artificial/r-course/data/tema1/student-fwf.txt",widths = c(4, 15, -20,15,4), col.names = c("ID","nombre","carrera","año"))
