
family.salary = c(40000, 6000, 50000, 8000, 6000, 7000, 6000)
family.size = c(4,3,2,2,3,4,3)
family.car = c("Lujo","Compacto","Utilitario","Lujo","Compacto","Compacto", "Compacto")

family <- data.frame(family.salary,family.size,family.car)


# quiero eliminar filas completamente iguales
family.unique <- unique(family)

# si quiero ver que filas están duplicadas
family[duplicated(family),]




