#Librerías usadas en el código: la librería que se debe abrir 100% antes de comenzar es la de tidyverse y stringr, el resto se pueden abrir también previamente o ejecutar en el momento de usar el comando(se pone el nombre de la librería :: y el comando. Ej.: Datawapprr::dw_data_to_chart())
library(tidyverse)
library(stringr)
#library(DatawRappr)
#library(purrr)

####1. Limpiar los datos####
#Lo mínimo para comenzar es una base de datos con dos variables, una con el nombre de los partidos (o lo que se quiera analizar, eje-x), y otra con el valor numérico que se quiere representar en el gráfico
#A partir de estas dos variables, se calculan la media y los intervalos de confianza para hacer un gráfico de errores (caso 1) o la mediana y los percentiles para un gráfico estilo boxplot (caso2)
#En ambos casos, se debe crear una variable transformando el partido en númerica (será la que se sitúe en el eje x del gráfico)

##También se debe crear una lista con los colores correspondientes a cada partido.
colores <- c("PSOE" = "#FF1C1C","PP" = "#17589d","Vox" = "#63ac33","Sumar" = "#E51C55","Podemos" = "#AF56FF","SALF" = "#613737")
lista_colores<-NULL
lista_colores$map<- as_tibble_row(colores) %>% as.list() #Se guarda la lista de los colores asociados a cada partido como una variable llamada map.
DatawRappr::dw_edit_chart(chart_id, visualize=list("color-category"=lista_colores, "color-by-colum"=T))

#Caso 1: media e intervalos
datos_grafico <- datos %>% 
  group_by(partido) %>% 
  summarise(
    n = n(),
    med = mean(ideol),
    sd = sd(ideol)) %>% 
  mutate(
    se = sd / sqrt(n),
    ic = se * qt((1 - 0.05) / 2 + 0.5, n - 1),
    linferior = media - ic,
    lsuperior = media + ic,
    posicion_x = as.numeric(partido),
    color = colores[partido]) %>% 
  select(c(partido,position_x,med,linferior,lsuperior,colores)) #IMPORTANTE: es necesario que todos los dataframe que se quieran usar tengan estas 6 variables, ya que se usan después para editar el gráfico

#Caso 2: mediana y percentiles
datos_grafico <- datos %>%
  group_by(partido) %>%
  summarise(p05 = quantile(ideol, .05),
            med = median(ideol),
            p95 = quantile(ideol, .95)) %>%
  mutate(posicion_x = as.numeric(partido),
         color = colores[partido]) %>% 
  mutate(party = fct_relevel(party, c("PP", "PSOE", "VOX", "SUMAR", "Otros"))) #Este comando es opcional para reordenar las variables del eje x



####2.Añadir los datos al gráfico####
#El gráfico (nube de puntos) puede crearse desde R (grafico<-DatawRappr::dw_create_chart(type="d3-scatter-plot")) o desde la página, y después guardar su id para los futuros comandos.
#Despúes se añaden los datos a ese gráfico, lo que creará los popups (en el eje x irá la variable partido en numérica -posicion_x- y en el y la media o mediana)
DatawRappr::dw_data_to_chart(datos_grafico, chart_id)



####3.Creación de las columnas del gráfico####
#Está hecho con el caso 2, si fuese el 1 (barras de error) solo habría que cambiar p05 por linferior y p95 por lsuperior 

##Columnas: 
dw_width <- 0.5
columnas_min <- datos_grafico %>%
  mutate(value = p05, 
         opacity = 0.4) %>%
  mutate(dw_range = str_glue( "{posicion_x-dw_width/2}, 0, {posicion_x-dw_width/2}, {value}, {posicion_x+dw_width/2}, {value}, {posicion_x+dw_width/2}, 0, @color:{color} @width:20 @opacity:{opacity}")) %>%
  #A partir de las diferentes variables, el comando anterior crea una nueva columna para cada partido con el formato de texto que se necesita para añadir una columna en Datawrapper
  pull(dw_range) %>% str_c( collapse = "\n")
  #Este comando deja como única variable del dataframe la del texto necesario para la columna, transformándolo de dataframe a vector de cadenas de texto. La segunda parte une todas las cadenas creadas (una por partido) en una sola, separándolas, separando cada una de ellas con un salto de línea.

columnas_max <- datos_grafico %>%
  mutate(value = p95,
        opacity = 0.4) %>%
  mutate(dw_range = str_glue( "{posicion_x-dw_width/2}, 0, {posicion_x-dw_width/2}, {value}, {posicion_x+dw_width/2}, {value}, {posicion_x+dw_width/2}, 0, @color:{color} @width:20 @opacity:{opacity}")) %>%
  pull(dw_range) %>% str_c( collapse = "\n")

##Líneas horizontales y verticales
lineas_med <- datos_grafico %>%
  mutate(x = as.numeric(partido),
         y = round(med, 1)) %>% #El round es opcional
  transmute(partido, color,
            line = str_glue("{x-0.3}, {y}, {x+0.3}, {y}") %>% as.character() ) %>%
  mutate(line = str_glue("{line} @color:{color} @width:2 @opacity:0.95") ) %>%
  pull(line) %>% str_c( collapse = "\n")

lineas_v <- datos_grafico %>%
  mutate(x = as.numeric(partido) + 0.45,
         y_min = round(p05, 1),
         y_max = round(p95, 1)) %>%
  transmute( partido, color,
             line = str_glue("{x}, {y_min}, {x}, {y_max}") %>% as.character() ) %>%
  mutate( line = str_glue("{line} @color:{color} @width:1.5 @opacity:0.65") ) %>%
  pull(line) %>% str_c( collapse = "\n")

##Una vez creadas todas las líneas y barras, se añaden al gráfico.
#Para ello, primero se unen todas en un único objeto
ranges <- str_c( columnas_min,"\n", columnas_max,"\n", lineas_med,"\n", lineas_v, collapse = "\n") #Lo del final de collapse = "\n" pertenece a la librería stringr, y es necesario para que no se raye el gráfico al actualizarlo (si no lo pones o no tienes abierta la librería no se pasan las líneas de una línea a la siguiente). No me funcionaba así que he puesto entre cada uno de los objetos el "\n" de forma manual, pero añadiendo lo del final debería ser innecesario.
DatawRappr::dw_edit_chart(chart_id, visualize =  list( "highlight-range" = ranges ))



####4. Añadir los textos al gráfico####
#Partidos en eje x
texts_labels <- datos_grafico %>%
  transmute(
    x = as.numeric(partido),
    y = 1,
    align = "mc", #EL texto se muestra centrado con las coordenadas especificadas
    bold = "TRUE",
    text = str_glue("{partido}")) %>%
  mutate_all(as.character)

# los números de las columnas
texts_numbers <- datos_grafico %>%
  transmute(
    value = med,
    x = as.numeric(partido),
    y = med + 0.2,
    size = 10,
    color = colores,
    align = "bc", #EL texto se muestra sobre de las coordenadas especificadas
    bold = "TRUE", bg = "TRUE",
    text = str_glue("{scales::number(value, accuracy = 1, decimal.mark = ',')}")) %>%
  mutate_all(as.character)

# los números de intervalos
texts_numbers_p05 <- datos_grafico %>%
  transmute(
    value= p05,
    x = as.numeric(partido) - 0.45,
    y = p05,
    size=10,
    color = colores,
    align = "tc", #EL texto se muestra debajo de las coordenadas especificadas
    text = str_glue("{scales::number(y, accuracy = 1, decimal.mark = ',')}")) %>%
  mutate_all(as.character)

texts_numbers_p95 <- datos_grafico %>%
  transmute(
    x = as.numeric(partido) + 0.45,
    y = p95,
    size=10,
    color = colores,
    align = "bc",
    text = str_glue("{scales::number(y, accuracy = 1, decimal.mark = ',')}") ) %>%
  mutate_all(as.character)

texts_out <- texts_numbers %>%
  bind_rows(texts_numbers_p05) %>%
  bind_rows(texts_numbers_p95) %>%
  bind_rows(texts_labels) %>%
  purrr::transpose() %>% #Convierte el dataframe a una lista de listas, donde cada sublista representa una fila del dataframe con sus correspondientes columnas
  as.list()

DatawRappr::dw_edit_chart(chart_id, visualize=list("text-annotations"=texts_out))



####5.Añadri las descripciones emergentes de cada partido al pasar el ratón por encima####
#Para hacerlo se debe crear una lista con los sigueitnes elementos, que son los que requiere el código del gráfico:
descripción_partidos<-data.frame(
  body=str_glue("Media ideológica: {{{{med}}}} \n \n Limite máximo: {{{{p95}}}} \n \n Límite minimo: {{{{p05}}}}"), #El comando str_glue interpreta las dos llaves {{}} como indicativo de que tiene que usar los datos de una variable con ese nombre, pero como en este caso se quieren mantener de un modo literal, en lugar de dos se deben añadir 4 llaves {{{{}}}}, que en el resultado final se mostrarán como solo 2. #Para que el salto de línea quede reflejado en el gráfico, hay que añadir dos veces el \n.
  title="{{ partido }}",
  sticky=FALSE,
  enabled=TRUE)

#Antes de añadirlo, el dataframe debe ser convertido en una lista común con las cuatro variables (formato necesario para que funcione):
descripción_partidos <- as.list(descripción_partidos)

DatawRappr::dw_edit_chart(chart_id, visualize = list( "tooltip" = descripción_partidos))


