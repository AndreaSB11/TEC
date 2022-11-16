
setwd("/Users/andreasb/Desktop/CVNL/Datos")

new <- read.csv("new.csv")
library(tidyr)
library(dplyr)
library("tidyverse")
library(ggplot2)
library(corrplot)

#Nos enfocamos en la correlación que tiene la variable dde nivel educativo con las demás variables relacionadas con violencia.
var_1 <- cor.test(new$nivel_de_estudios_1, new$violencia_dentro_de_los_hogares)
#Si existe correlación 

var_2 <- cor.test(new$nivel_de_estudios_1, new$violencia_en_la_via_publica)
#Si existe correlación 

#No existe correlación:
var_3 <- cor.test(new$nivel_de_estudios_1, new$piropos_en_la_calle)
var_4<- cor.test(new$nivel_de_estudios_1, new$mujeres_mayor_riesgo_de_ser_agredidas)
var_5 <- cor.test(new$nivel_de_estudios_1, new$mujeres_responsables_por_forma_de_vestir)
