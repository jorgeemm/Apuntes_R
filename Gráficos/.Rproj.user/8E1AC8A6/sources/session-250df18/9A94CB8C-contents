####Directorio y librerías####
setwd("C:/Users/Usuario/OneDrive - Universidad Carlos III de Madrid/Documentos/0 - Uni/4º/Segundo Cuatri/TFG/Datos")
library(haven)
library(tidyverse)
library(ggh4x) #Para el gráfico de líneas con relleno
library(extrafont) #Para añadir nuevas fuentes de texto este y el siguiente
loadfonts(device = "win", quiet = TRUE) 
library(stargazer)

####Limpieza de variables####
datos <- read_dta("ESS tfg.dta")
#Variables dependiente e independientes
datos<-datos %>% 
  mutate(mani=ifelse(pbldmn==1,1,ifelse(pbldmn==2,0,NA))) %>% 
  mutate(mani=factor(mani, levels=c("0","1"), labels=c("No", "Sí"))) %>% 
  mutate(genero=ifelse(gndr==1,0,1)) %>% 
  mutate(genero=factor(genero,levels=c("0","1"),labels=c("Hombre", "Mujer"))) %>% 
  mutate(edad=agea) %>% 
  filter(!(edad %in% c(14,15,91,  92,  93,  94,  95,  96,  97,  98,  99, 101, 102, 103))) %>% 
  mutate(edad=as.numeric(edad)) %>% 
  mutate(gen=cut(yrbrn,breaks=c(1918,1947,1961,1975,1983,1988,2000), labels = c(1, 2, 3, 4, 5, 6))) %>%
  mutate(gen=factor(gen,levels=c("1", "2", "3", "4", "5", "6"),labels=c("Dictadura","Transición","Felipista","Aznarista","Zapaterista","Postcrisis")))

##Variables de control#
datos<-datos %>% 
  mutate(ideol=lrscale,
         interes=polintr,
         estudios=eduyrs,
         thogar=hswrk,
         social=sclmeet,#Otra opción la variable sclact- comparación en escala de mucho-poco sobre la participación en actividades sociales en comparación con otros de la misma edad
         salud=health,
         felicidad=happy,
         nacimiento=yrbrn) %>%
  filter(!(nacimiento%in%c(1902:1918, 2001,2002,2003,2004))) %>% 
  mutate(año=2002+(essround-1)*2)%>% 
  mutate(año=as.factor(año)) %>% 
  mutate(año=ifelse(año==2010,2011,ifelse(año==2012,2013,ifelse(año==2014,2015,ifelse(año==2016,2017,ifelse(año==2018,2019,ifelse(año==2002,2002,ifelse(año==2004,2004,ifelse(año==2006,2006,ifelse(año==2008,2008,NA)))))))))) %>% 
  mutate(año=as.factor(año)) %>% 
  mutate(ideol=as.numeric(ideol)) %>% 
  mutate(estudios=as.numeric(estudios)) %>% 
  mutate(salud=as.numeric(salud)) %>% 
  mutate(filicidad=as.numeric(felicidad)) %>% 
  mutate(interes=as.numeric(interes)) %>% 
  mutate(social=as.numeric(social)) %>% 
  mutate(domicil=as.numeric(domicil)) %>% 
  mutate(thogar=factor(thogar,levels=c("0","1"),labels=c("No","Sí"))) %>% 
  mutate(sitlab=ifelse(pdwrk==1,"Trabajador",ifelse(uempla==1,"Desempleado",ifelse(uempli==1,"Desempleado",ifelse(hswrk==1,"Tareas hogar/cuidado",ifelse(edctn==1,"Estudiante",ifelse(rtrd==1,"Jubilado",NA))))))) %>% 
  mutate(edadl=log(edad)) %>% 
  mutate(edad2=edad^2) %>% 
  mutate(sitlab=as.factor(sitlab)) %>% 
  mutate(interes=recode(interes,`1` = 4, `2` = 3, `3` = 2, `4` = 1))%>% 
  mutate(domicil=recode(domicil,`1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1))%>% 
  mutate(hijos18 = ifelse(essround == 9 & (rshipa2 == 2 | rshipa3 == 2 | rshipa4 == 2 | rshipa5 == 2 |rshipa6 == 2 |rshipa7 == 2 |rshipa8 == 2 |rshipa9 == 2 |rshipa10 == 2 |rshipa11 == 2 |rshipa12 == 2 |rshipa13 == 2 |rshipa14 == 2 |rshipa15 == 2 |rshipa16 == 2 |rshipa17 == 2 |rshipa18 == 2 |rshipa19 == 2 |rshipa20 == 2 |rshipa21 == 2 |rshipa22 == 2 |rshipa23 == 2 |rshipa24 == 2), 2, 1)) %>% 
  mutate(hijos18 = ifelse(is.na(hijos18), 1, hijos18)) %>% 
  mutate(hijos=ifelse(año!=2019,chldhm,ifelse(año==2019,hijos18,NA)))

variables_definitivas<-c("mani", "genero","gen", "año", "edad","edadl","edad2","nacimiento", "ideol", "interes", "estudios", "thogar", "social","hijos", "salud", "felicidad","sitlab","domicil")
datos<-datos[, variables_definitivas,drop=F]
attach(datos)

#write_dta(datos,"datosTFG.dta")

####Análisis descriptivo####
round(prop.table(table(mani,genero),2),4)*100

###Tabla descriptivos variables###
descriptivos<- as.data.frame(sapply(datos, as.numeric))
descriptivos<-descriptivos %>% 
  mutate(mani=ifelse(mani==1,0,ifelse(mani==2,1,NA))) %>% 
  mutate(genero=ifelse(genero==1,0,ifelse(genero==2,1,NA))) %>%
  mutate(hijos=ifelse(hijos==1,0,ifelse(hijos==2,1,NA))) %>% 
  select(-c("edadl","edad2","mani","nacimiento"))
stargazer(descriptivos, digits=2, type="text")
stargazer(descriptivos, digits=2, type="html",out="descriptivos.html")


###Generaciones, potestas, y género###
#Dataset con la relacion entre las tres variables
brecha_gen<-table(mani,genero,gen) #Crear la tabla de frecuencia absoluta
brecha_gen<-as.data.frame.table(brecha_gen) #Guardarla como un dataframe
brecha_gen<-brecha_gen %>% #Crar la variable donde se sume la frecuencia absluta para cada caso común de género y generación
  group_by(gen, genero) %>% #Agrupa los casos comunes de las dos variables, para que la siguiente función las tenga en cuenta como una
  mutate(Suma_Freq = sum(Freq)) %>% #Crea la variable de la suma de frecuencias
  ungroup() %>%
  mutate(porcentaje = round((Freq / Suma_Freq),4) * 100) %>%  #Crea la variable de frecuencias relativas de columna para cada una de las generaciones
  filter(mani!="No")   #Para quedarme solo con los que sí se han manifestado

#Gráfico de barras
ggplot(brecha_gen, aes(x = gen, y = porcentaje, fill = genero)) +
  scale_y_continuous(limits=c(0,30))+
  geom_hline(yintercept = c(0, 10, 20,30), color = "lightgrey", alpha = 0.5) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.94), width = 0.87, colour="black") +
  labs(x = "Generación", y = "Porcentaje de manifestantes", fill = "Género", caption = "Figura 3: Porcentaje de manifestantes en cada generación según su género") +
  theme_classic() +
  geom_text(aes(label = porcentaje, y = porcentaje + 1),
            position = position_dodge(width = 1), vjust = 1.1, size = 3) +
  theme(plot.caption = element_text(hjust=0,family="Times New Roman",size=15), #Ajustes del caption
        text = element_text(size = 11),  # Tamaño del texto del gráfico
        legend.position = "bottom",      # Posición de la leyenda
        legend.title = element_blank(),   # Elimina el título de la leyenda
        legend.text = element_text(size = 9),  # Tamaño del texto de la leyenda
        axis.title = element_text(size = 12)) +  # Tamaño del texto de los títulos de los ejes
  scale_fill_manual(values=c("Hombre"="#d8d8d8", "Mujer"="#777777"))


###Edad, protestas y género###
brecha_edad<-table(mani,genero,edad) 
brecha_edad<-as.data.frame.table(brecha_edad) 
brecha_edad<-brecha_edad %>% 
  group_by(genero, edad) %>% 
  mutate(Suma_Freq = sum(Freq)) %>% 
  ungroup() %>%
  mutate(porcentaje = Freq / Suma_Freq * 100) %>% 
  filter(mani!="No") %>% 
  mutate(edad=as.numeric(edad)+15) %>% 
  mutate(genero=factor(genero, levels=c("Total","Mujer","Hombre")))%>% 
  select(-mani,-Freq,-Suma_Freq) %>% 
  pivot_wider(names_from = genero, values_from = porcentaje)
  
#Gráfico de líneas
g<-ggplot(data = brecha_edad, aes(x=edad)) + #y=porcentaje, color=genero, group=genero
  scale_x_continuous(limits=c(15,90),breaks=c(10,20,30,40,50,60,70,80,90))+
  scale_y_continuous(limits=c(0,40),breaks=c(0,10,20,30,40))+
  geom_hline(yintercept = c(0, 10, 20,30,40), color = "lightgrey", alpha = 0.5)+
  geom_vline(xintercept=c(35,65), color="black", linetype="dashed")+
  theme_classic()+
  labs(x="Edad",y="Porcentaje de manifestantes",caption = "Figura 2: Porcentaje de manifestantes para cada edad según su género" )+
  theme(plot.caption = element_text(hjust=0,family="Times New Roman",size=15), #Ajustes del caption
        text = element_text(size = 11),
        legend.position = "bottom",
        legend.title = element_blank(), 
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 12)) +
  geom_line(aes(y=Mujer),color="#51DA2C",size=1.15)+
  geom_line(aes(y=Hombre),color="#130081",size=1.15)+
  stat_difference(aes(ymin = Hombre, ymax = Mujer), alpha = 0.5,) +
  scale_fill_manual(values = c("#51DA2C", "#130081"), labels=c("Mujer","Hombre"))
g+annotate("text",x=c(23,50,80), y=c(39,39,39),label=c("Jóvenes", "Adultos", "Ancianos"),size=3.6)


###Año, protestas y género###
brecha_año<-table(mani,genero,año) 
brecha_año<-as.data.frame.table(brecha_año) 
brecha_año<-brecha_año %>% 
  group_by(genero, año) %>% 
  mutate(Suma_Freq = sum(Freq)) %>% 
  ungroup() %>%
  mutate(porcentaje = Freq / Suma_Freq * 100) %>% 
  filter(mani!="No")

#Gráfico de barras
ggplot(brecha_año, aes(x = año, y = porcentaje, fill = genero)) +
  scale_y_continuous(limits = c(0, 40)) +
  geom_hline(yintercept = c(0, 10, 20, 30, 40), color = "lightgrey", alpha = 0.5) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.94), width = 0.87, colour = "black") +
  labs(x = "Año", y = "Porcentaje de manifestantes", fill = "Género", 
       caption = "Figura 1: Porcentaje de manifestantes en cada año según el género") +
  theme_classic()+
  theme(plot.caption = element_text(hjust = 0, family = "Times New Roman", size = 15), 
        text = element_text(size = 11),  
        legend.position = "bottom",      
        legend.title = element_blank(),   
        legend.text = element_text(size = 9),  
        axis.title = element_text(size = 12)) +
  scale_fill_manual(values = c("Hombre" = "#d8d8d8", "Mujer" = "#777777")) +
  geom_text(aes(label = round(porcentaje, 2), y = porcentaje + 1),
            position = position_dodge(width = 1), vjust = 0.8, size = 3)


####Análisis inferencial####
#Establecer referencias para las variables categóricas
datos$gen<-relevel(datos$gen,ref="Transición")
datos$sitlab<-relevel(datos$sitlab,ref="Trabajador")

##Regresión sin género##
#Modelo 1: edad + año
r1<-glm(mani~edadl+año,family="binomial",data=datos)
#Modelo 2: generaciones + año
r2<-glm(mani~gen+año,family="binomial",data=datos)
#Modelo 3:edad + gen + control actitudinales
r3<-glm(mani~edadl+gen+año+ideol+interes,family="binomial",data=datos)
#Modelo 4: edad + gen + control biográficas
r4<-glm(mani~edadl+gen+año+estudios+social+hijos+domicil,family="binomial",data=datos)
#Modelo 5: edad + gen + todo control
r5<-glm(mani~gen+edadl+año+ideol+interes+estudios+sitlab+social+hijos+domicil,family="binomial",data=datos)

stargazer(r1,r2,r3,r4,r5, type="text",
          align=TRUE, no.space=TRUE,
          column.labels = c("Modelo 1","Modelo 2","Modelo 3","Modelo 4","Modelo 5"))

stargazer(r1,r2,r3,r4,r5, type="html",out="regresiones1.html",
          align=TRUE, no.space=TRUE,
          column.labels = c("Modelo 1","Modelo 2","Modelo 3","Modelo 4","Modelo 5"))


##Regresiones con género##
#Modelo 6: edad + género
r6<-glm(mani~edadl*genero+año,family="binomial",data=datos)
#Modelo 7: generación + género
r7<-glm(mani~gen*genero+año,family="binomial",data=datos)
#Modelo 8: edad + género + control
r8<-glm(mani~edadl*genero+gen*genero+año+ideol+interes,family="binomial",data=datos)
#Modelo 9: generación + género + control
r9<-glm(mani~edadl*genero+gen*genero+año+estudios+social+hijos+domicil,family="binomial",data=datos)
#Modelo 10: todo
r10<-glm(mani~edadl*genero+gen*genero+año+ideol+interes+estudios+sitlab+social+hijos+domicil,family="binomial",data=datos)

stargazer(r6,r7,r8,r9,r10, type="text",align=TRUE, no.space=TRUE)

stargazer(r6,r7,r8,r9,r10, type="html",out="regresiones2.html",
          align=TRUE, no.space=TRUE,
          column.labels = c("Modelo 6","Modelo 7","Modelo 8","Modelo 9","Modelo 10"))


###Robustez###
##Edad lineal y cuadrática##
r11<-glm(mani~edad2+año,family="binomial",data=datos)
r12<-glm(mani~edad2+gen+año+ideol+interes+estudios+sitlab+social+hijos+domicil,family="binomial",data=datos)
r13<-glm(mani~edad+año,family="binomial",data=datos)
r14<-glm(mani~edad+gen+año+ideol+interes+estudios+sitlab+social+hijos+domicil,family="binomial",data=datos)
r15<-glm(mani~edad2*genero+año,family="binomial",data=datos)
r16<-glm(mani~gen*genero+edad2*genero+año+ideol+interes+estudios+sitlab+social+hijos+domicil,family="binomial",data=datos)
r17<-glm(mani~edad*genero+año,family="binomial",data=datos)
r18<-glm(mani~gen*genero+edad*genero+año+ideol+interes+estudios+sitlab+social+hijos+domicil,family="binomial",data=datos)

stargazer(r11,r12,r13,r14,r15,r16,r17,r18, type="html",out="regresionesrobustez.html",
          align=TRUE, no.space=TRUE)

##Test de prueba para ver cual quita la relación en la edad - modelos 4/9
r4a<-glm(mani~edadl+gen+año++sitlab+social+hijos+domicil,family="binomial",data=datos) #Sin estudios 
r4b<-glm(mani~edadl+gen+año+estudios++social+hijos+domicil,family="binomial",data=datos) #Sin sitlab
r4c<-glm(mani~edadl+gen+año+estudios+sitlab++hijos+domicil,family="binomial",data=datos) #Sin social
r4d<-glm(mani~edadl+gen+año+estudios+sitlab+social++domicil,family="binomial",data=datos) #Sin hijos
stargazer(r4a,r4b,r4c,r4d, type="text",align=TRUE, no.space=TRUE)
stargazer(r4b,align=TRUE, no.space=TRUE, type="html",out="modelo4_definitivo.html")
r9b<-glm(mani~edadl*genero+gen*genero+año+estudios+social+hijos+domicil,family="binomial",data=datos)
stargazer(r9b, type="text",align=TRUE, no.space=TRUE)
stargazer(r9b,align=TRUE, no.space=TRUE, type="html",out="modelo9_definitivo.html")


####Pruebas otras cosas####

###Gráfico de barras con intervalo de confianza
#Forma 1
brecha_gen<-table(mani,genero,gen) #Crear la tabla de frecuencia absoluta
brecha_gen<-as.data.frame.table(brecha_gen) #Guardarla como un dataframe
brecha_gen<-brecha_gen %>% #Crar la variable donde se sume la frecuencia absluta para cada caso común de género y generación
  group_by(gen, genero) %>% #Agrupa los casos comunes de las dos variables, para que la siguiente función las tenga en cuenta como una
  mutate(Suma_Freq = sum(Freq)) %>% #Crea la variable de la suma de frecuencias
  ungroup() %>%
  mutate(porcentaje = round((Freq / Suma_Freq),4) * 100) %>%  #Crea la variable de frecuencias relativas de columna para cada una de las generaciones
  mutate(p = (Freq / Suma_Freq)) %>%
  filter(mani!="No") %>%   #Para quedarme solo con los que sí se han manifestado
  mutate(error=(sqrt((p*(1-p))/16.857))) %>% 
  mutate(intervaloi=(p-1.96*error)*100) %>% 
  mutate(intervalos=(p+1.96*error)*100)

ggplot(brecha_gen, aes(x = gen, y = porcentaje, fill = genero)) +
  scale_y_continuous(limits = c(0, 30)) +
  geom_hline(yintercept = c(0, 10, 20, 30), color = "lightgrey", alpha = 0.5) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.94), width = 0.87, colour = "black") +
  geom_errorbar(aes(ymin = intervaloi, ymax = intervalos), position = position_dodge(width = 0.94), width = 0.87) +
  geom_text(aes(label = porcentaje), position = position_dodge(width = 0.94), vjust = -0.5, size = 3) +
  labs(x = "Generación", y = "Porcentaje de manifestantes", fill = "Género", 
       caption = "Figura 3: Porcentaje de manifestantes en cada generación según su género") +
  theme_classic() +
  theme(plot.caption = element_text(hjust = 0, family = "Times New Roman", size = 15), 
        text = element_text(size = 11),  
        legend.position = "bottom",      
        legend.title = element_blank(),   
        legend.text = element_text(size = 9),  
        axis.title = element_text(size = 12)) +
  scale_fill_manual(values = c("Hombre" = "#d8d8d8", "Mujer" = "#777777"))


#Forma 2
brecha_año<-table(mani,genero,año) 
brecha_año<-as.data.frame.table(brecha_año) 
brecha_año<-brecha_año %>% 
  group_by(genero, año) %>% 
  mutate(Suma_Freq = sum(Freq)) %>% 
  ungroup() %>%
  mutate(porcentaje = Freq / Suma_Freq * 100) %>% 
  filter(mani!="No") %>% 
  mutate(p = (Freq / Suma_Freq)) %>%
  mutate(error=(sqrt((p*(1-p))/16.903))) %>% 
  mutate(intervaloi=(porcentaje-1.96*error)) %>% 
  mutate(intervalos=(porcentaje+1.96*error))

ggplot(brecha_año, aes(x = año, y = porcentaje, fill = genero)) +
  scale_y_continuous(limits=c(0,40))+
  geom_hline(yintercept = c(0, 10, 20,30,40), color = "lightgrey", alpha = 0.5) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.94), width = 0.87, colour="black") +
  labs(x = "Generación", y = "Porcentaje de manifestantes", fill = "Género", caption = "Figura 1: Porcentaje de manifestantes en cada año según el género") +
  theme_classic() +
  geom_text(aes(label = round(porcentaje,2), y = porcentaje + 1),
            position = position_dodge(width = 1), vjust = 1.1, size = 3) +
  theme(plot.caption = element_text(hjust=0,family="Times New Roman",size=15), #Ajustes del caption
        text = element_text(size = 11),  # Tamaño del texto del gráfico
        legend.position = "bottom",      # Posición de la leyenda
        legend.title = element_blank(),   # Elimina el título de la leyenda
        legend.text = element_text(size = 9),  # Tamaño del texto de la leyenda
        axis.title = element_text(size = 12)) +  # Tamaño del texto de los títulos de los ejes
  scale_fill_manual(values=c("Hombre"="#d8d8d8", "Mujer"="#777777"))+
  geom_errorbar(aes(ymin = intervaloi, ymax = intervalos), position = position_dodge(width = 0.94), width = 0.87)
  


###Otra froam de hacer las interacciones
#Modelo 6: edad + género
datos$generoedad<-interaction(datos$edadl,datos$genero)
datos$generoedad<-relevel(datos$generoedad,ref="Crisis.Hombre")
r6<-glm(mani~edad:genero,family="binomial",data=datos)
#Modelo 7: generación + género
datos$gengen<-interaction(datos$gen,datos$genero)
datos$gengen<-relevel(datos$gengen,ref="Crisis.Hombre")
r7<-glm(mani~gengen,family="binomial",data=datos)
#Modelo 8: edad + género + control
r8<-glm(mani~gengen+año+ideol+interes+estudios+sitlab+social ,family="binomial",data=datos)
#Modelo 9: generación + género + control
r9<-glm(mani~gen*genero+año+ideol+interes+estudios+sitlab+social ,family="binomial",data=datos)
#Modelo 10: todo
r10<-glm(mani~gengen+año+ideol+interes+estudios+sitlab+social ,family="binomial",data=datos)




