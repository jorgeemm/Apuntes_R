####Creación de una finción####
##Crear la función con el nombre que quieras. El código de la función va entre llaves {}
analisis1<-function(x,y){
  #Función para realizar el análisis estadístico de dos variable, calculando media, mediana, desviación típica y haciendo un histograma y diagrama de cajas
  x.media<-mean(x)
  x.mediana<-median(x)
  x.dt<-sd(x)
  y.media<-mean(y)
  y.mediana<-median(y)
  y.dt<-sd(y)
  
  #Colocamos estos resultados en una tabla
  valores<-c(x.media,x.mediana, x.dt,y.media,y.mediana,y.dt)
  etiquetas<-c("x.media","x.mediana", "x.dt","y.media","y.mediana","y.dt")
  tabla.resumen<-data.frame(etiquetas,valores)
  
  #Realizamos los gráficos --> todos en una misma ventana
  par(mfrow=c(2,2)) #para añadir variós gráficos en 1. Los números indican el número de filas y de columnas. Los comandos siguientes se asignan en orden, de izq a dcha, y de arriba a abajo
  hist(x,main="Histograma de la variable x", col="blue", ylab="Frecuencia",xlab="",col.lab="brown",col.axis="maroon4")
  hist(y,main="Histograma de la variable y", col="cyan", ylab="Frecuencia",xlab="",col.lab="brown",col.axis="maroon4")
  boxplot(x,main="Diagrama de caja de x", col="blue", ylab="Frecuencia",col.lab="brown",col.axis="maroon4")
  boxplot(y,main="Diagrama de caja de y", col="cyan", ylab="Frecuencia",col.lab="brown",col.axis="maroon4")
  
  return(tabla=tabla.resumen) #para que muestre la tabla que hemos creado antes
}

analisis1(EXPRNC,SALARY)

#Añadir la tabla resumen en bonito
analisis1b<-function(x,y){
  x.media<-mean(x)
  x.mediana<-median(x)
  x.dt<-sd(x)
  y.media<-mean(y)
  y.mediana<-median(y)
  y.dt<-sd(y)
  
  par(mfrow=c(2,2)) #para añadir variós gráficos en 1. Los números indican el número de filas y de columnas. Los comandos siguientes se asignan en orden, de izq a dcha, y de arriba a abajo
  hist(x,main="Histograma de la variable x", col="blue", ylab="Frecuencia",xlab="",col.lab="brown",col.axis="maroon4")
  hist(y,main="Histograma de la variable y", col="cyan", ylab="Frecuencia",xlab="",col.lab="brown",col.axis="maroon4")
  boxplot(x,main="Diagrama de caja de x", col="blue", ylab="Frecuencia",col.lab="brown",col.axis="maroon4")
  boxplot(y,main="Diagrama de caja de y", col="cyan", ylab="Frecuencia",col.lab="brown",col.axis="maroon4")
  
  #Para cambiar la tabla anterior
  library(kableExtra)
  Variable<-c("X", "Y")
  Media<-c(x.media,y.media)
  Mediana<-c(x.mediana,y.mediana)
  DT<-c(x.dt,y.dt)
  tabla.resumen<-data.frame(Variable, Media, DT)
  
  #Para que aparezca en la ventana de viewer la tabla
  tabla.resumen %>%
    kbl(caption="Resumen estdístico") %>% #añadir título a la tabla
    kable_classic(full_width=F, html_font="Cambria")#Diferentes opciones de visualización
}
analisis1b(EXPRNC,SALARY)

#Función personalizada para cualquier dataframe que se use
analisis1c<-function(x,i,j){
  x.media<-mean(x[,i])
  x.mediana<-median(x[,i])
  x.dt<-sd(x[,i])
  y.media<-mean(y[,j])
  y.mediana<-median(y[,j])
  y.dt<-sd(y[,j])
  
  library(kableExtra)
  Variable<-c(names(x[i],names(x[j])))
  Media<-c(x.media,y.media)
  Mediana<-c(x.mediana,y.mediana)
  DT<-c(x.dt,y.dt)
  tabla.resumen<-data.frame(Variable, Media, DT)
  
  par(mfrow=c(2,2)) #para añadir variós gráficos en 1. Los números indican el número de filas y de columnas. Los comandos siguientes se asignan en orden, de izq a dcha, y de arriba a abajo
  hist(x[,i],main=paste("Histograma de", names(x)[i]), xlab="", col="blue", ylab="Frecuencia",xlab="",col.lab="brown",col.axis="maroon4")
  hist(x[,j],main=paste("Histograma de", names(x)[j]), xlab="", col="cyan", ylab="Frecuencia",xlab="",col.lab="brown",col.axis="maroon4")
  boxplot(x[,i],main=paste("Diagrama de caja de",names(x)[i]), col="blue",col.axis="maroon4")
  boxplot(x[,j],main=paste("Diagrama de caja de",names(x)[j]), col="cyan",col.axis="maroon4")
  
  tabla.resumen %>%
    kbl(caption="Resumen estdístico") %>% #añadir título a la tabla
    kable_classic(full_width=F, html_font="Cambria") 
}

analisis1c(EMPLEADOS,1,4)












