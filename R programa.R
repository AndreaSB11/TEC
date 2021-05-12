rm(list = ls())
library(ggplot2)
library(gapminder)

#Asignar a la variable datos la tabla gapminder
datos <- gapminder

#Crear gráfico
grafico <- ggplot(
  datos, 
  aes(x = gdpPercap, y= lifeExp, colour = continent)+ gem_pont())

#Mostrar gráfico 
grafico