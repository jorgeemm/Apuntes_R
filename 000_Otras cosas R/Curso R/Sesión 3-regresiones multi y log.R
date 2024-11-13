####Lectura de datos desde una página web y limpieza####

autompg=read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data")
view(autompg)

#Añadir nombres a las variables
colnames(autompg)=c("mpg","cyl","disp","hp","wt","acc","year","origin","name")

#Eliminar errores -> pasar a valores perdidos
autompg=subset(autompg,autompg$hp!="?")
#Eliminamos también los coches de 3 y 5 ciilindradas porque son raros
autompg=autompg[autompg$cyl!=5,] #Delante de la coma en un [] van las filas y detrás las columntas. Como detrás no se indica nada, es porque se quedan todas las columnas
autompg=autompg[autompg$cyl!=3,]
#Ver el tipo de variables
str(autompg)
autompg$hp=as.numeric(autompg$hp)
str(autompg)

####Modelo de regresión múltiple####
library(scatterplot3d)
scatterplot3d(autompg$hp, autompg$wt, autompg$mpg, pch=19,color="blue")

#Estimación del modelo
mpg_model<-lm(mpg~hp+wt, data=autompg)

#Ver coeficientes y significatividad
mpg_model
coef(mpg_model)
summary(mpg_model) #Se puede pedir que muestre solo uno de los valores. Ej.: summary(mpg_model)$coef summary(mpg_model)$r.squared
confint(mpg_model,level=0.99)

mpg_model<-lm(mpg~hp, data=autompg)

ggplot(autompg, aes(hp,mpg))+geom_point()

B0_mpg<-coef(mpg_model)[1]
B1_mpg<-coef(mpg_model)[2]

ggplot(autompg, aes(hp,mpg))+geom_point()+
  geom_abline(intercept=B0_mpg, slope=B1_mpg,col="violet",lty=1,lwd=1)

#Incorporar variable dummy
#Creamos una nueva variable dummy llamada domestic
autompg$domestic<-as.numeric(autompg$origin==1) #crea la variable dando el valor 1 cuando la variable origin tiene los valores 1
autompg$domestic<-as.factor(autompg$domestic)

ggplot(autompg, aes(hp, mpg, col=domestic))+geom_point(size=2,alpha=I(0.7))

mpg_hp_dom<-lm(mpg~hp+domestic,data=autompg)
summary(mpg_hp_dom)

#Realmente existen dos rectas de regresión, con la misma pendiente, pero una con un B0 para EEUU y otra para los de fuera
B0_extr<-coef(mpg_hp_dom)[1]
B0_dom<-coef(mpg_hp_dom)[1]+coef(mpg_hp_dom)[3]

B1_extr<-coef(mpg_hp_dom)[2]
B1_dom<-coef(mpg_hp_dom)[2]

ggplot(autompg, aes(hp,mpg,col=domestic))+
  geom_point(size=2,alpha=I(0.7))+
  geom_abline(intercept=B0_dom,slope=B1_dom,col="turquoise2",lty=1,lwd=1)+
  geom_abline(intercept=B0_extr,slope=B1_extr,col="salmon1",lty=1,lwd=1)
  
####Interacciones####
mpg_hp_int=lm(mpg~hp+domestic+hp:domestic,data=autompg)
summary(mpg_hp_int)

B0_extr<-coef(mpg_hp_int)[1]
B0_dom<-coef(mpg_hp_int)[1]+coef(mpg_hp_int)[3]
B1_extr<-coef(mpg_hp_int)[2]
B1_dom<-coef(mpg_hp_int)[2]+coef(mpg_hp_int)[4]

ggplot(autompg,aes(hp,mpg,col=domestic))+
  geom_point(size=2,alpha=I(0.7))+
  geom_abline(intercept=B0_dom,slope=B1_dom,col="turquoise2",lty=1,lwd=1)+
  geom_abline(intercept=B0_extr,slope=B1_extr,col="salmon1",lty=1,lwd=1)


#Para ver que modelo es mejor se utiliza ANOVA
#Hiotesis nula: el primero
#Hipótesisi alternativa: el segundo
anova(mpg_hp_dom,mpg_hp_int)
#Se rechaza la hipótesis nula, el mejor es el segundo

####Regresión logística####
library(ISLR2)
library(vcd)
library(tidyverse)


#VP los positivos clasisifacos correctamente
#VN negativos clasificados correctamente
#FM positivos clasificados incorrectamente como negativos
#FP negativos clasificados icnorrectamente como positivos
#Exactitud=(VP+VN)/Total
#Tasa de error=(FP+FN)/Total

names(Default)
summary(Default)

#Análisis exploratorio
ggplot(Default, aes(default,balance, color=default))+
  geom_boxplot()+
  geom_jitter(width = .1)+
  theme_bw()+
  theme(legend.position="null")

ggplot(Default, aes(default,income, color=default))+
  geom_boxplot()+
  geom_jitter(width = .1)+
  theme_bw()+
  theme(legend.position="null")

ggplot(Default)+
  geom_bar(aes(default,fill=student))

#Creación del dataset train y test
ntrain<-nrow(Default)*0.8
ntest<-nrow(Default)*0.2

set.seed(161)
index_train<-sample(1:nrow(Default),size=ntrain)
train<-Default[index_train,]
test<-Default[-index_train,]

summary(train)
summary(test)

#Estimación del modelo
logit_reg<-glm(default~balance+student+income,data=train,family="binomial")

#Predicciones
predicted_values<-predict(logit_reg,test,type="response") #las predicciones las hace con el 20% que se dejo fuera
predicted_class<-ifelse(predicted_values>0.5,"Yes","No") 

performance_data<-data.frame(observed=test$default,predicted=predicted_class) #crear una dataframe con una columna (valores observados) y las predicciones, para poder compararlas

#Observaciones positivas, negativas, predicciones positivas y negativas
positive<-sum(performance_data$observed=="Yes")
negative<-sum(performance_data$observed=="No")
predicted_positive<-sum(performance_data$predicted=="Yes")
predicted_negative<-sum(performance_data$predicted=="No")
total=nrow(performance_data)

data.frame(positive,negative,predicted_positive,predicted_negative)

#Clasificación: tasas absolutas
tp<-sum(performance_data$observed=="Yes" & performance_data$predicted=="Yes")
tn<-sum(performance_data$observed=="No" & performance_data$predicted=="No")
fp<-sum(performance_data$observed=="No" & performance_data$predicted=="Yes")
fn<-sum(performance_data$observed=="Yes" & performance_data$predicted=="No")
data.frame(tp,tn,fp,fn)

#Clasificación: porcentajes
accuracy<-(tp+tn)/total
error_rate<-(fp+fn)/total
sensitivity<-tp/positive
especificity<-tn/negative
precision<-tp/predicted_positive
npv<-tn/predicted_negative
data.frame(accuracy,error_rate,sensitivity,especificity,precision,npv)

#Estos resultados se cambian modificando en punto de corte en predicte_class. En función de si interesa más tener una tasa de erro mayor, una precisión... se establecerá un punto de corte u otro


#Matriz de confusión: para visualizar cómo es el modelo
matriz_confusión<-table(performance_data,
                        dnn=c("observaciones", "predicciones"))
matriz_confusión

mosaic(matriz_confusión,shade=T,colorize=T,
       gp=gpar(fill=matrix(c("green", "red", "red", "green"),2,2)))

#evaluación del modelo
anova(logit_rep,test=Chisq)

##Gráfico de probabilidades, únicamente para una variable
#Modelo de regresión
modelo<-glm(default~balance, data=train, family="binomial")

#Predicciones del modelo con ggplot
Default<-Default %>% 
  mutate(default2=0) %>% 
  mutate(default2=if_else(Default$default=="Yes",1,default2))
#comprobacioens
table(Default$default)
table(Default$default2)

#se crea un vextor con nuevos valores interpolados en el rango de observaciones
nuevos_puntos<-seq(from=min(Default$balance),to=max(Default$balance), by=100)

#Predicción de los nuevos puntos según el modelo
#Si se indica se.fit=TRUE se debuelve el error estándar de cada predicción
#Junto con el valroe de la predicción (fit)
predicciones<-predict(modelo,data.frame(balance=nuevos_puntos),se.fit=TRUE)

#Mediante la función logit se transfroman los log_ODDs a probabilidades
predicciones_logit<-exp(predicciones$fit)/(1+exp(predicciones$fit))

#Se calcula el límite inferior y suprerior del IC adel 95% sustrayendo e incrementamos el logODDs de cada predicción. Una vez calculados se transforman las probabilidades con la función logit
limite_inferior<-predicciones$fin-1.96*predicciones&se.fit
limite_inferior_logit<-exp(limite_inferior)/(1+exp(limite_inferior))
limite_superior<-predicciones$fit+1.96*predicciones$se.fit
limite_superior_logit<-exp(limite_superior)/(1+exp(limite_superior))

#Se crea un dataframe con lso nuevos puntos y sus predicciones
datos_curva<-data.frame(balance=nuevos_puntos,
                        probabilidades_default=predicciones_logit,
                        limite_inferior_logit=limite_inferior_logit,
                        limite_superior_logit=limite_superior_logit)
ggplot(Default, aes(balance, defaut2))+
  geom_point(aes(color=as.factor(default2)),shape="I", size=3)+
  geom_line(data=datos_curva,aes(y=probabilidades_default), col="red")+
  geom_line(datos=datos_curva,aes(-y=límite_inferior_logit), linetype="daesh")+
  geom_line(datos_curva, aes(y=limite_superior_logit),linetype="daesh")+
  theme_bw()+
  labs(title="Modelo de regresión logística default~balance",
       y="P(default=1|balance)", y="Balance")+
  theme(legend.position = "null")+
  theme(plot.title = element_text(size=10))





