library(lmmm)
buildModel(para.Lmmm.v2)

# Para pasar a formato alguna columna 
as.POSIXct.Date(Base_peru_eze_LMMM$wc_2)
as.Date(Base_peru_eze_LMMM$wc_2)


para.Lmmm.v2$wc_2<- as.Date.character(para.Lmmm.v2$wc_2)
auto_date(Modelo$wc_2)

as.Date(Modelo$wc_2,format="%m%d%y")
View(Modelo)

Base_peru_eze_LMMM$wc_2 <- as.Date(Base_peru_eze_LMMM$wc_2,"%d/%m/%Y")
str(Base_peru_eze_LMMM)

names(c_sales_competitor_tot)[19] = "s_sales_tot" 
colnames(Base_peru_eze_LMMM)[19] = "s_sales_tot"

View(Base_peru_eze_LMMM)cla
Str(Base_peru_eze_LMMM)

buildModel(Base_peru_eze_LMMM)

class(para.Lmmm.v2$wc_2)


buildModel(Modelo)


