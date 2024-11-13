setwd("C:/Users/Usuario/OneDrive - Universidad Carlos III de Madrid/Escritorio/Curso R")
library(readxl)
BeneficiosNA <- read_excel("C:/Users/Usuario/OneDrive - Universidad Carlos III de Madrid/Escritorio/Curso R/BeneficiosNA.xlsx")
attach(BeneficiosNA)
names(BeneficiosNA)

#Ver si hay varianles perdidas
is.na(Experiencia)
sum(is.na(Experiencia))
sum(is.na(Genero))
sum(is.na(Salario))

####Crear función para ver los perdidos de todo un datafram3####
f.imputar <-function(x){
  n<-dim(x)[1] #primer componente del objeto  --> el número de observaciones del fichero
  p<-dim(x)[2] #segundo componente -< número de columnas
  
  variables<-names(x)
  z<-rep(0,p) #se repite el 0 en función del número de variables. Si hay valores perdidos, aparecen estos en lugar del 0
  
  for(j in 1:p){ #lo que aparece entre llaves lo hace para cada una de las columnas
    z[j]<-sum(is.na(x[,j]))
  }
  
  tabla<-data.frame(variables,z)
  names(tabla)<-c("Variable", "Nº datos faltantes")
  
  list(Datos.faltantes=tabla) #se usa list y no return si hay más de un objeto (en este caso se podría usar también return)
  
}

f.imputar(BeneficiosNA)


####Imputar valores perdidos####
f.imputar2<-function(x) {
  n<-dim(x)[1] 
  p<-dim(x)[2]
  variables<-names(x)
  z<-rep(0,p)
  
  z<-apply(X=is.na(x), MARGIN=2, FUN=sum) #X mayúscula es la función que se va a utilizar para aplicar a ese dataframe / MARGUIN=1 aplica la función por filas, MARGIN=2 lo aplica sobre las columnas / FUN: una vez que se ha visualizado por columnas si hay valores perdisos, ejecuta otra función (sum en este caso)
  
  for(j in 1:p){
  print(j) #muestra los subíndices, si se están recorriendo todas las columnas. Es para ver más facilmente si hay errores
  
  xj<-x[[j]]  # con el doble corchete te quedas SOLO con los valores de la primera columna     
  zj<-sum(is.na(xj))
  pos<-c(1:n)
  if(zj>0){
    #identificar las posiciones con datos faltantes. Indica dodne estan las posiciones de valores faltantes
    posNA<-pos[is.na(xj)]
    #número de observaciones con datos faltantes en la variable j
    nx<-length(posNA)
    #calculamos la media únicamente para los datos sin NA
    xj.media<-mean(xj[!is.na(xj)]) #otra opción es mean(xj, na.rm=TRUE)
    #se sustituyen los valores faltantes por la media calculada
    xj[posNA]<-xj.media
    #sustituímos el dataframe inicial por esta nueva columna
    x[,j]<-xj
  }
  }
   list(datos.faltantes=z,datos.nuevos=x)        
}

resultado<-f.imputar2(BeneficiosNA)
resultado$datos.faltantes
apply(X=is.na(resultado$datos.nuevos),MARGIN = 2,FUN=sum)
head(resultado$datos.nuevos) #muestra las seis


#Género es una variable categórica, no númerica -> no se puede imputar por la media
str(BeneficiosNA)
BeneficiosNA$Genero<-factor(BeneficiosNA$Genero, levels = c(0,1), labels = c("M","F"))

f.imputar3<-function(x){
  library(e1071)
  n<-dim(x)[1] 
  p<-dim(x)[2]
  variables<-names(x)
  z<-rep(0,p)
  
  z<-apply(X=is.na(x), MARGIN=2, FUN=sum) #X mayúscula es la función que se va a utilizar para aplicar a ese dataframe / MARGUIN=1 aplica la función por filas, MARGIN=2 lo aplica sobre las columnas / FUN: una vez que se ha visualizado por columnas si hay valores perdisos, ejecuta otra función (sum en este caso)
  
  for(j in 1:p){
    print(j)
    xj<-x[[j]]
    zj<-sum(is.na(xj))
    pos<-c(1:n)
    if(zj>0){
      posNA<-pos[is.na(xj)]
      #número de observaciones con datos faltantes en la variable j
      xj.sinNA<-xj[!is.na(xj)]
      
      #únicamnete calculamos la media/meidana si se trata de uan variable cuantitativa
      if(class(xj)=="factor"){
        print(variables[j])
        
        #si es un factor reemplazamos por la clase con frecuencia máxima(moda)
        #la moda se calcula sobre el vector de observacioens sin valores faltantes
        #la función no es para que devuelva la moda (posición), sino el valor con el que se corresponde esta (masculino o femenino)
        xj[posNA]<-levels(xj.sinNA)[table(xj.sinNA)==max(table(xj.sinNA))]
        
        #sustituimos el dataframe inicial por esta nueva columna
        x[,j]<-xj
      }
      else{
        #Si es numérica analizamos si la distribución es simétrica. En este caso, se utiliza la mediana para imputar los datos
        coef.asimetria<-abs(skewness(xj.sinNA))
        print(coef.asimetria)
        if(coef.asimetria<0.2){
          #en este caso, se puede considerar que la distribución es simétrica, asi que se sustituyen los valores faltatnes por la media
          xj[posNA]<-mean(xj.sinNA)
        }
        else{
          xj[posNA]<-median(xj.sinNA)
        }
        x[,j]<-xj
      }
    }
  }
  list(datos.faltantes=z,datos.nuevos=x) 
}

resultado<-f.imputar3(BeneficiosNA)
apply(X=is.na(resultado$datos.nuevos),MARGIN=2,FUN=sum)
head(resultado$datos.nuevos)



