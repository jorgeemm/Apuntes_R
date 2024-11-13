ODS <- read_excel("sdgi2017-data-web-final.xlsx", 
                  na = "n.d.")
names(ODS)

#Eliminar de la base de datos todas las variables menos algunas
datos<-subset(ODS,select=c("country", "SDG1", "SDG2","SDG3","SDG4","SDG5","SDG6","SDG7","SDG8","SDG9","SDG10","SDG11","SDG12","SDG13","SDG14","SDG15","SDG16","SDG17"))
dim(datos) #indica el número de filas y columnas del dataset

#calculamos el número de datos faltantes por columnas (variables)
colSums(is.na(datos)) #es una alternativa a apply(X=is.na(datos),MARGIN=",FUN=sum)

#calculamos el % de valores perdidos
100*colSums(is.na(datos))/(dim(datos)[1])
library(visdat)
vis_miss(datos)

#ordenar las variables de mayor a mento % de NA
vis_miss(datos, sort_miss=TRUE)
#agrupar los países según el NA
vis_miss(datos, cluster=TRUE)

#Si hay más del 5% de valores faltantes se eliminan del dataframe

borrar<-c("SDG10", "SDG14")
datos2 <-datos[,!(names(datos) %in% borrar )]

attach(datos2)
SDG1.mean<-mean(SDG1,na.rm=TRUE)
SDG1<-SDG1 %>% 
  replace(is.na(.),SDG1.mean)









