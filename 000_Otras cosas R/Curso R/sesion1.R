EMPLEADOS <- read.csv("C:/Users/Usuario/OneDrive - Universidad Carlos III de Madrid/Escritorio/Curso R/employees.txt", sep="")

attach(EMPLEADOS)
str(EMPLEADOS)

####Datos cualitativos####

###Transformar variable númeria en factores (cualitativa en cuantitativa)
EDUC <- factor(EDUC, levels=c(1:3), labels = c("Secundaria","Grado","Postgrado"))
MGT<-factor(MGT,levels=c(0,1),labels("No", "Sí"))
#c(1:3) es = que c(1, 2, 3) 

##Tablas de frecuencia
table(EDUC)
#frecuencias relativas: se dividen las frecuencias por el total de la muestra
table (EDUC)/sum(table(EDUC))

##Diagrama de barras##
barplot(table(EDUC))
#Para que muestre las frecuencias relativas y no los totales en el eje y
barplot(table (EDUC)/sum(table(EDUC)))
##Modificiaión del gráfico
#Añadir etiqutas al eje, título al diagrama y cambiar colores todas las barras
barplot(table(EDUC), xlab="Nivel de formación", ylab="Frecuencia absoluta", main="Daigrama de baras", col="blue")
#Un color para cada barra
barplot(table(EDUC), xlab="Nivel de formación", ylab="Frecuencia absoluta", main="Daigrama de baras", 
        col=c("blue", "yellow", "pink")
#Añadir leyenda al gráfico
barplot(table(EDUC), xlab="Nivel de formación", ylab="Frecuencia absoluta", main="Daigrama de baras", 
        col=c("blue", "yellow", "pink"), legend-text=c("Secundaria","Grado","Postgrado"), names.arg=FALSE)
#Cambiar la leyenda de situio        
barplot(table(EDUC), xlab="Nivel de formación", ylab="Frecuencia absoluta", main="Daigrama de baras",
        col=c("blue", "yellow", "pink"),names.arg=FALSE)
leyend("topleft", c("Secundaria","Grado","Postgrado"), cex=0,8, fil=c("blue", "yellow", "pink")
#Tamaño de letra por defecto cex=1

dp<-barplot(table(EDUC), xlab="Nivel de formación", ylab="Frecuencia absoluta", ylim=c(0,20),
        main="Diagrama de barras", col=c("blue", "yellow", "pink"),
        names.arg=FALSE)
text(dp,table(EDUC)+0,5,labels=table(EDUC),cex=0.8,col="brown") #Añade el valor encima de la barra

#Cambiar tamaño de la leyenda
legend("topright", legend = c("Secundaria", "Grado", "Postgrado"),
       fill=c("blue","yellow", "pink"),
       border="black",
       cex=0.8)
#Poner el g´rafico en horizontal: las=1 // etiquetas en horizontal horiz=TRUE 
barplot(table(EDUC), xlab="Frecuencia absoluta", col=c("blue", "yellow", "pink"), cex.names=0.7,
        cex.lab=0.7,cex.axis=0.7,horiz=TRUE,las=1)
title(main=list("Diagrama de barras", font=4, col="grey")) #font=4 es para poner el título en cursiva


##Diagrama de sectores##
gr <-table(EDUC)
pie(gr)
#R incluye numerosas paletas de colores en la libreria graphics
library(graphics)
pie(gr, col=rainbow(length(gr))) #length indica el número de colores que hay. Puede ser un número o directamente el tipo de g´rafico, para que lo haga automáticamente
pie(gr, col=rainbow(length(gr), alpha=0.2)) #alpha añade transparencia

#añadir frecuencias relativas y guardar el gráfico como imágen

jpeg(file="diagrama de sectores.jpeg")
prop.EDUC<-table(EDUC)/sum(table(EDUC))
prop2.EDUC<-round(prop.EDUC*100) #Pasar de decimales a %
etiquetas.EDUC<-c("Secundaria", "Grado", "Postgrado")
etiquetas <- paste(etiquetas.EDUC,prop2.EDUC)
etiquetas<-paste(etiquetas, "%", sep="") #sep="" elimina la separación enetre el valor y el %
pie(gr, labels=etiquetas, col=rainbow(length(etiquetas)), main="Diagrama de sextores")
legend("topright", c("Secundaria", "Grado", "Postgrado"), cex=0.8,fil=rainbow(length(table(EDUC))))
       
dev.off() #cierra lo de la primera línea


####Análisis de datos cuantitativos####
##Tabla de frecuencias##
#En lugar de hacer una tabla de frecuencia normal, se crean intervalos
cut(EXPRNC, breaks=c(0,3,5,10,20)) #breaks establece los valores en los que se crean los intervalos 0 a 3, 3 a 5...
table(cut(EXPRNC,breaks=c(0,3,5,10,20)))
transform(table(cut(EXPRNC,breaks=c(0,3,5,10,20)))) #para ponerlo en vertical
#asignar nobmres a las columnas
tabla.frecuencias<-transform(table(cut(EXPRNC,breaks=c(0,3,5,10,20)))) #para ponerlo en vertical
names(tabla.frecuencias)<-c("Intervalo","Frecuancia absoluta") ####Este comando sirve para renombrar cualquier tabla que se cree
tabla.frecuencias

#Histograma para datos cuantitativos discretos
hist(EXPRNC,breaks=c(0,3,5,10,20)) #si no se añade la opición breaks, R muestra los rangos que le interesa

hist(SALARY,
     main ="Distribución del salario",
     xlab="Salario",
     ylab="Número de personas",
     xlim=c(10000,30000), #el límite de valores del eje x
     col="blue",
     border="white")



##Medidas descriptivas##
#Media
mean(SALARY)
#Mediana
median(SALARY)
#percentiles y cuartriles
quantile(SALARY)
quantile(SALARY,probs=0.65) #un percentil específico
quantile(SALARY,probs=c(0.25,0.65))

#Añadir al histograma líneas de medidas descriptivas
hist(SALARY,
     main ="Distribución del salario",
     xlab="Salario",
     ylab="Número de personas",
     xlim=c(10000,30000), #el límite de valores del eje x
     col="blue",
     border="white")
abline(v=mean(SALARY),lwd=4, lty=5, col="red")#v indica la cordenada del eje x donde se situará la línea //lwd es el tamaño de la línea, y lty el típo (lineas, puntos contínua)
text("media", x=mean(SALARY), y=10,font=4, col="red") #añadir texto sobre la línea
text("media", x=mean(SALARY)+2000, y=10,font=4, col="red")#para desplazar algo la etiqueta


hist(SALARY,
     freq=FALSE,                              #para representar densidades de frecuencia, no frecuencia absoluta
     main ="Distribución del salario",
     xlab="Salario",
     ylab="Número de personas",
     xlim=c(10000,30000), #el límite de valores del eje x
     col="blue",
     border="white")
curve(dnorm(x, mean=mean(SALARY), sd=sd(SALARY)), add=TRUE, col="red", lwd=3) #add=TUE para que lo superponga al histograma
lines(density(SALARY),lwd=4, col="pink")
#La curva roja es la teórica (distribución normal) y la rosa la real, lo que sirve para ver si es una distribución normal o no

#Calcular coeficiente de asimetría
library(e1071)
skewness(SALARY)

##Diagrama de cajas y bigotes##
boxplot(SALARY)
boxplot(SALARY, horizontal=TRUE)
#Diagrama en función de una segunda variable
boxplot(SALARY~MGT,
        main="Salario en función del cargo que desempeña en la empresa", cex.main=0.9,
        xlab="salario",
        ylab="Cargo de gestión",
        names=c("No","Si"),
        horizontal = T, las=1)
#Añadir la media sobre el gráfico
grupo_no <- subset(EMPLEADOS, MGT==0) #Se seleccionan los cargos que no tienen un cargo de gestión
grupo_si<-subset(EMPLEADOS, MGT==1)

points(mean(grupo_no$SALARY),1,pch=25,bg="red") #el simbolo del $ es para seleccionar solo los datos del grupo de la variable SALARY
points(mean(grupo_si$SALARY),2,pch=22,bg="blue")

boxplot(SALARY~MGT,
        main="Salario en función del cargo que desempeña en la empresa", cex.main=0.9,
        xlab="salario",
        ylab="",
        cex.lab=0.9,
        cex.axis=0.8,
        names=c("no", "si"),
        col=rainbow(2, alpha=0.2),
        border=rainbow(2, v=0.6),
        las=1,
        horizontal=T,
        yaxt="n",  #n de null en el eje y: elemina el eje y
        frame=F)
legend("bottomright", title="Cargo de gestión", levels(MGT), fill=rainbow(2, alpha=0.2), cex=0.8)






