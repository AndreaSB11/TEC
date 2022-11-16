setwd("/Users/andreasb/Desktop/CVNL/Datos")

data <- read.csv("EAV_2021.csv")
library(tidyr)
library(dplyr)
library("tidyverse")
library(ggplot2)
library(corrplot)

#Renombramos las columnas de interes
newdata = data %>% rename(
  nivel_de_estudios_1 =cp6_1_1,
  nivel_de_estudios_2 =cp6_2_1,
  violencia_dentro_de_los_hogares=p93,
  violencia_en_la_via_publica=p94,
  piropos_en_la_calle=p95_1,
  mujeres_mayor_riesgo_de_ser_agredidas=p95_2,
  mujeres_responsables_por_forma_de_vestir=p95_3,
  mayor_problema_de_seguridad=p96)

#Seleccionamos las columnas que son de interes en un nuevo data frame
new <-newdata [,c("nivel_de_estudios_1",
                  "nivel_de_estudios_2",
                  "violencia_dentro_de_los_hogares", 
                  "violencia_en_la_via_publica",
                  "piropos_en_la_calle",
                  "mujeres_mayor_riesgo_de_ser_agredidas",
                  "mujeres_responsables_por_forma_de_vestir",
                  "mayor_problema_de_seguridad")] 

#Ver NA de nuestro data frame por columnas y en general: 
sum(is.na(new)) #Todo el data frame contiene un total de 4407 NA`s
sum(is.na(new$nivel_de_estudios_2)) #Contiene 4166 NA´s
sum(is.na(new$nivel_de_estudios_1)) #Contiene 241 NA´s
#Las columnas de nivel de estudios contienen todos los NA´s de la base seleccionada, la variable: nivel_de_estudios_2 hace referencia a 
#¿Cuál es el nivel máximo de estudios actualmente cursando (si sigue estudiando)?
#La variable nivel_de_estudios_2 hace referencia a 
# ¿Cuál es el nivel máximo de estudios terminado (si ya no estudia)?
# Es decir la mayoria 241 de los encuestados es estudiante, mientras que 4166 ya no estudia.
#Partiendo de lo anterior para trabajar el tema de educación se decidió trabajar con la variable nivel_de_estudios_1 omitiendo las observaciones con NA´s

new <-newdata [,c("nivel_de_estudios_1",
                  "violencia_dentro_de_los_hogares", 
                  "violencia_en_la_via_publica",
                  "piropos_en_la_calle",
                  "mujeres_mayor_riesgo_de_ser_agredidas",
                  "mujeres_responsables_por_forma_de_vestir",
                  "mayor_problema_de_seguridad")] 
#Borramos los NA`s
new <- new[!is.na(new$nivel_de_estudios_1),]

#Verificamos que la base no contenga NA´s
sum(is.na(new))