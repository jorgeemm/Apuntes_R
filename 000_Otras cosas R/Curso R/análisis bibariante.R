####Análisis bivariante####

attach(EMPLEADOS)

####Datos cualitativos####
#tabla de frecuencia absoluta
table(EDUC,MGT)
#tabla de contingencia
prop.table(table(EDUC,MGT))
#tabla condicionada de la variable MGT según las diferenctes categorías de EDUc
prop.table(table(EDUC,MGT), 1) #para el valor 1 de EDUC
#Frecuencias absolutas marginales
addmargins(table(EDUC,MGT))
addmargins(prop.table(table(EDUC, MGT)))
#redondear los decimales
round(addmargins(prop.table(table(EDUC, MGT))),2) #el último número son los decimales

#gráfico de asociación para ver si existe algún tipo de relación entre las variables
assocplot(table(EDUC,MGT))

#Chi
chisq.test(table(EDUC,MGT))


####Datos cuntitativos####
#Salarios por grupos
tapply(SALARY,MGT,mean)
boxplot(SALARY~MGT,data=EMPLEADOS,
        xlab="MGT",ylab="Salario")

####ANOVA####
fm=aov(lm(SALARY~MGT))
summary(fm)
###Supuestos del modelo
##1-Distribución normal
#Ejemplos con gráficos
boxplot(fm$residuals, horizontal=TRUE)
hist(fm$residuals)
qqnorm(fm$residuals)
qqline(fm$residuals)

#Comprobaición con el test de shafiro
shapiro.test(residuals(fm))

##2-contraste de homoscedasticidad
bartlett.test(SALARY~MGT)

##3-independencia de los residuos
plot(fm$residuals) #la distribución de los residuos no es aleatoria


####Modelo de regresión lineal####
estudio <- read_excel("Estudio de Mercado.xls")
attach(estudio)
names(estudio)
dim(estudio)
#relación lineal entre ingreso y gasto ocio
cov(ingreso,gastoOcio) #covarianza de 0 es que no hay relación, número + que hay relación positiva, negativo que hay relación negativa. No indica la fortaleza de la relación
cor(ingreso,gastoOcio)
plot(ingreso, gastoOcio)
plot(ingreso, gastoOcio,
     pch=16, #tamaño de los puntos
     col="red",
     xlab="ingresos",ylab="Gasto en ocio")

#Histograma de las variables
par(mfrow=c(1,2))
hist(ingreso, col="blue",main="")
hist(gastoOcio, col="cyan",main="")

#Relación entre el ingreso y el gasto
par(mfrow=c(1,1))
cor(ingreso, gasto)
plot(ingreso, gasto) #La forma de embudo indica un problema de herasticidad en los datos: las variables son muy asimétricas
#Transformar las variables en logaritmos para que sean más simétricas
logingreso<-log(ingreso)
loggasto<-log(gasto)
plot(logingreso, loggasto) 

par(mfrow=c(1,2))
hist(logingreso)
hist(loggasto)

cor(logingreso, loggasto)

#Recta de regresión
regresión <-lm(loggasto~logingreso)
summary(regresión)
#Para ahorrarte un paso: summary(lm(loggasto~logingreso))

#recta de regresión estimada
par(mfrow=c(1,1))
plot(logingreso, loggasto, pch=20, bg="wheat4")
abline(regresión, col="slateblue",lwd=2)

#comprobación de los datos atípicos
cuartiles<-quantile(logingreso)
RI<-cuartiles[4]-cuartiles[2]
limitesuperior<-cuartiles[4]+1.5*RI
limiteinferior<-cuartiles[2]-1.5*RI

limitesuperior
limiteinferior

logingreso[logingreso>limitesuperior]
logingreso[logingreso<limiteinferior]

#Eliminación del dato atípico
n<-length(logingreso)
pos<-c(1:n)
atípico<-pos[logingreso>limitesuperior]

logingreso2<-logingreso[pos!=atípico]
length(logingreso2)

loggasto2<-loggasto[pos!=atípico]

regresion2<-lm(loggasto2~logingreso2)
summary(regresion2)

#comparación modelos
par(mfrow=c(1,2))
plot(logingreso, loggasto, pch=21, bg="cyan",
     cex.axis=0.7, cex.lab=0.8)
abline(regresión, col="red", lwd=2)
plot(logingreso2,loggasto2, pch=21, bg="cyan",
     cex.axis=0.7, cex.lab=0.8)
abline(regresion2, col="red", lwd=2)
#Ajustar el rango de los ejes para que sea igual
plot(logingreso, loggasto, pch=21, bg="cyan",
     cex.axis=0.7, cex.lab=0.8,
     xlim=c(8.5,12),ylim=c(8.5,11.5))
abline(regresión, col="red", lwd=2)
title(main="Modelo de regresión inicial", cex.main=0.7)
plot(logingreso2,loggasto2, pch=21, bg="cyan",
     cex.axis=0.7, cex.lab=0.8,
     xlim=c(8.5,12),ylim=c(8.5,11.5))
abline(regresion2, col="red", lwd=2)
title(main="Modelo de regresión sin atípico", cex.main=0.7)







