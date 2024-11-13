library(ggplot2)
library(ISLR2) #librería con conjunto de datos
library(tidyverse)

ggplot(data=College)#el data= se puede poner o no

####Diagramas de dispersión####
#Diagrama de dispersión entre el coste de la matrícula y el gasto del estudiante
ggplot(College)+
  geom_point(aes(x=Outstate,y=Expend))

#puntos de distinto color según públicas o privadas
ggplot(data=College,aes(Outstate,Expend,color=Private)) + geom_point()
#distintas geometrías según sean públicas o privadas
ggplot(College,aes(Outstate,Expend,color=Private,shape=Private)) + geom_point()

#Crear una variable nueva que identifique a las universidades con el mayor porcentaje de estudiantes del top 10
College %>% #el simboloo %>% sirve para encadenar difeentes líneas de código 
  mutate(topUniversity=0) %>% 
  mutate(topUniversity=if_else(Top10perc>75,1,topUniversity)) %>%
  summarise(total=sum(topUniversity)) #para obtener el total de universidades a las que hemos asignado el 1

#guardar las modificaciones en un fichero txt
College %>%
  mutate(topUniversity=0) %>% 
  mutate(topUniversity=if_else(Top10perc>75,1,topUniversity)) %>%
  write.table("collegenuevo.txt")
  
#guardar los cambios en un nuevo dataframe
College2<-College %>%
  mutate(topUniversity=0) %>% 
  mutate(topUniversity=if_else(Top10perc>75,1,topUniversity))
College2$topUniversity<-as.factor(College2$topUniversity)
str(College2)

#Gráfico de puntos con 4 variables
ggplot(College2, aes(Outstate, Expend, color=Private,shape=topUniversity))+geom_point()

#Gráfico 5 variables (tamaño del putno, asociado a una variable numérica)
ggplot(College2, aes(Outstate, Expend, color=Private,shape=topUniversity,size=perc.alumni))+geom_point()
#Transparencia para que no se solapen tanto
ggplot(College2, aes(Outstate, Expend, color=Private,shape=topUniversity,size=perc.alumni, alpha=0.2))+geom_point()


####Diagramas de barras####
ggplot(College2)+geom_bar(aes(x=Private))

#Crear un formato común para los títulos que se vaan a usar
TemaTítulo=element_text(size=rel(2), #Tamaño relativo del título
                        vjust=2, #justificación vertical, para separarlo del g´rafico
                        face="bold",
                        color="red",
                        lineheight = 1.5) #Separación entre líneas(solo se añade si el título va a ser demasiado largo y no entra en una línea)

ggplot(College2)+geom_bar(aes(x=Private, fill=Private))+
  theme(text=element_text(size=8))+
  ggtitle("Distribución de luniversidades \n por tipo de titularidad")+ #\n es para indicar que se salta a la siguiente línea
  labs(x="Universidad privada", y="Nº de unibersidades")+
  theme(plot.title=TemaTítulo) +
  theme(axis.title=element_text(face="bold", color="blue", size=rel(1.5)))

#Incorporar la frecuencia absoluta de cada clase
#Construir una tabla de frecuencias, sobre la que se realiza el diagrama de barras
Tabla<-College %>% 
  group_by(Private) %>% 
  summarise(Total=n()) #Total es el nombre del objeto que ponemos nosotros. n() es como un sumatorio, indica lo que se pide y ya
Tabla

#Incorporar esta información al diagrama de barras
ggplot(Tabla, aes(x=Private, y=Total, fill=Private))+
  geom_bar(width=0.9, stat="identity", position = position_dodge())+
  theme(text=element_text(size=8))+
  ggtitle("Distribución de luniversidades \n por tipo de titularidad")+ #\n es para indicar que se salta a la siguiente línea
  labs(x="Universidad privada", y="Nº de universidades")+
  theme(plot.title=TemaTítulo) +
  theme(axis.title=element_text(face="bold", color="blue", size=rel(1.5)))+
  geom_text(aes(label=Total),vjust=1.5,color="black",size=4)

#Añadir los porcentajes
Tabla<-College %>% 
  group_by(Private) %>%
  summarise(Total=n()) %>% 
  mutate(Porcentaje=round(Total/sum(Total)*100, 1)) #round es para redondear a un decimal (el 1 del final)

ggplot(Tabla, aes(x=Private, y=Total, fill=Private))+
  geom_bar(width=0.9, stat="identity", position = position_dodge())+
  theme(text=element_text(size=8))+
  ggtitle("Distribución de luniversidades \n por tipo de titularidad")+ #\n es para indicar que se salta a la siguiente línea
  labs(x="Universidad privada", y="Nº de universidades")+
  theme(plot.title=TemaTítulo) +
  theme(axis.title=element_text(face="bold", color="blue", size=rel(1.5)))+
  geom_text(aes(label=paste0(Total,"","","(",Porcentaje, "%",")")), #la diferencia entre paste y paste0 es que no se dejan espacios entre el número y el % //las "" nada dentro son espacios
            vjust=-0.3,color="black",size=4) 
#En vertical
ggplot(Tabla, aes(x=Private, y=Total, fill=Private))+
  geom_bar(width=0.9, stat="identity", position = position_dodge())+
  theme(text=element_text(size=8))+
  ggtitle("Distribución de luniversidades \n por tipo de titularidad")+ #\n es para indicar que se salta a la siguiente línea
  labs(x="Universidad privada", y="Nº de universidades")+
  theme(plot.title=TemaTítulo) +
  theme(axis.title=element_text(face="bold", color="blue", size=rel(1.5)))+
  geom_text(aes(label=paste0(Total,"","","(",Porcentaje, "%",")")), #la diferencia entre paste y paste0 es que no se dejan espacios entre el número y el % //las "" nada dentro son espacios
            vjust=-0.3,color="black",size=4) +
  facet_grid(~"Diagrama de barras para el tipo de universidad")+
  theme(legend.position="")+ #Para borrar la leyenda
  coord_flip() #Para poner las barras en vertical

####Histogramas####
ggplot(College)+ geom_histogram(aes(x=Outstate))

#Incluir una línea con el valor de la media
ggplot(College)+ geom_histogram(aes(x=Outstate))+
  geom_vline(aes(xintercept=mean(Outstate)), color="slateblue", lwd=2, lty=3)+
  geom_text(geom="text",label="matríclura media", x=mean(College$Outstate)+3000, y=50, col="slateblue")

#Modificar algunas opciones del histograma inicial
ggplot(data=College)+
  geom_histogram(aes(x=Outstate), fill="lightblue", col="white")+
  labs(X="Coste de matrícula", y="Nº de universidades")
  
ggplot(data=College)+
  geom_histogram(aes(x=Outstate), fill="lightblue", col="white")+
  labs(X="Coste de matrícula", y="Nº de universidades", title="Distribución")+
  theme(plot.title=element_text(face="bold", color="wheat",size=15,hjust=.5,vjust=1))+
  theme(axis.text.x=element_text(color="violet",size=10),
        axis.text.y=element_text(color="violet", size=10))

ggplot(College)+
  geom_histogram(aes(x=Outstate,fill=Private, col=Private),alpha=0.5)+
  labs(x="Coste", y="NºUniversidades")

#Especia de histograma que muestra el % de públcias y privadas en cada posición del coste
ggplot(College)+
  geom_histogram(aes(x=Outstate,fill=Private),col="white",position="fill")+
  labs(x="Coste", y="NºUniversidades")
  
#Separar los histogramas 
ggplot(data=College)+
  geom_histogram(aes(x=Outstate), fill="lightblue", col="white")+
  labs(X="Coste de matrícula", y="Nº de universidades", title="Distribución")+
  facet_wrap(~Private)
#Cambair la orientación (dos filas en lugar de dos columnas)
ggplot(data=College)+
  geom_histogram(aes(x=Outstate), fill="lightblue", col="white")+
  labs(X="Coste de matrícula", y="Nº de universidades", title="Distribución")+
  facet_wrap(~Private,nrow=2)

####Diagramas de densidad####
ggplot(College, aes(x=Outstate, fill=Private))+
  geom_density(alpha=.7)
#Datos disgregados
ggplot(College)+
  geom_density(aes(x=Outstate, fill=Private))+
  facet_grid(Private~.)+
  xlab("Coste de matrícula")+
  ylab("")+
  ggtitle("distribución del coste de matrílcula")+
  theme_minimal()
  
####Diagramas de caja####
ggplot(College) +geom_boxplot(aes(x=Private, y=Outsate))

ggplot(College)+geom_boxplot(aes(x=Private, y=Outstate, fill=Private))+
  coord_flip()+
  geom_jitter(aes(x=Private, y=Outstate), size=2,alpha=.2,width=0.1)

#Incorporar la media
ggplot(College, aes(x=Private,y=Outstate))+
  geom_boxplot(aes(fill=Private))+
  theme(legend.position = "none")+
  scale_fill_brewer(palette="Blues")+
  coord_flip()+
  stat_summary(fun=mean,geom="point",shape=18,size=3,col="violet")

#Identificación de los atípicos (en la consola)
College %>% 
  group_by(Private) %>% 
  summarize(atípicos=boxplot.stats(Outstate)$out)

####Matriz de correlaciones entre diferentes variables cuantitativas####
library(ggcorrplot)

College3<-select(College,-Private) #Eliminamos la variable PRivate, que es un factor

corr<-round(cor(College3),1)

ggcorrplot(corr)+
  ggtitle("correlaciones")+
  theme(axis.text.x=element_text(color="blue",size=5),
        axis.text.y=element_text(color="blue",size=5))

####Análisis bivariados en una misma ventana####
library(cowplot) #solo para ggplot, hay que guardar cada gráfico en un objeto

#Diagrama de barras vertical y horizontal
Barra1 <- ggplot(College2,aes(Private,fill=Private))+
  geom_bar(width=.5,col="grey")+
  labs(x="Universidad Privada", y="Nº Universidades")

Barra2<-ggplot(College2, aes(topUniversity, fill=topUniversity))+
  geom_bar(width=.5,col="grey")+
  labs(x="Top University",y="Universidades con mayores Top10")+
  coord_flip()

plot_grid(Barra1,Barra2,nrow=1,align="h")


#Tres gráficas en una cuatdrícula 2x2
p1 <- ggplot(College2,aes(Private,fill=Private))+
  geom_bar(width=.5,col="grey")+
  labs(x="Universidad Privada", y="Nº Universidades")

p2<-ggplot(College2, aes(topUniversity, fill=topUniversity))+
  geom_bar(width=.5,col="grey")+
  labs(x="Top University",y="Universidades con mayores Top10")+
  coord_flip()

p3<-ggplot(College)+
  geom_histogram(aes(Outstate, fill=Private, col=Private),alpha=.5)+
                   labs(x="coste", y="Nº Universidaes")

library(patchwork) #hay que determinar el diseño de ventana

diseño1<-"
12
33"

p1+p2+p3+plot_layout(design=diseño1)


#Guardar estos resultados en un pdf
i<-1

diseño1<-"
12
33"

grafico1<-p1+p2+p3+plot_layout(design=diseño1)

ggsave(paste("gráfcico_",i,".pdf"),grafico1, width=14,height=8,device=pdf)

i<-i+1
diseño2<-"
12"
grafico2<-p1+p3+plot_layout(design=diseño2)
ggsave(paste("gráfcico_",i,".pdf"),grafico2, width=14,height=8,device=pdf)




