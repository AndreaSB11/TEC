#Minería y construcción de base de datos TW 
#Equipo 4 

#Fijar dirección de trabajo  
setwd ("/Users/andreasb/Desktop/TEC/Concentración/Actividades/act2")

#Librerías a utilizar 
library(rtweet)
library(tm)
library(tidytext)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidytext)
library(wordcloud)
library(textdata)

# Importante para minar TW
auth_setup_default()
rtweet_user()

#Librería para quitar las stopwords de los textos 
library(stopwords)
stop_words_es <- stopwords(language = "es", source="snowball")

#Palabras que se analizarán en TW: 
#Desaparecidas
#Feminicidios 
#FiscaliaNL

#============================================================================
#Palabra FiscaliaNL 
#Minar TW
FiscaliaNL_tw<-search_tweets("FiscaliaNL", n=5000)

#Construcción de base de datos
col1.1 <- FiscaliaNL_tw$created_at
col2.1 <- FiscaliaNL_tw$full_text
base.datos1 <- data_frame(col1.1,col2.1)
write.csv(base.datos1, "FiscaliaNLtw.csv")

tidy_FiscaliaNL_tw<- FiscaliaNL_tw %>%
  select(created_at,full_text) %>%
  unnest_tokens("word", full_text)
head(tidy_FiscaliaNL_tw)


#Limpieza de texto
quitar <- c("fiscalianl", "rt", "https", "t.co","02", "04d","1","10", "100", 
            "11", "13", "14", "15", "18", "1º","04sonpatrocinadores","0ukkuglzob",
            "0umjjzohmc","0y6ckjm9y3","100milpersonasdesaparecidad","16jvd","1ggk0cokyh",
            "1lkjfhocze","1r1nug2ib2")

df_limpio2 <- tidy_FiscaliaNL_tw %>%
  filter(!(word %in% stop_words_es)) %>%
  filter(!(word %in% quitar)) %>%
  count(word) %>%
  arrange(desc(n))

library(syuzhet)
texto_fiscalia <- df_limpio2$word
sentimientos_fiscaliaNL <- get_nrc_sentiment(df_limpio2$word, lang="spanish")

barplot(
  colSums(prop.table(sentimientos_fiscaliaNL[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = topo.colors (12),
  main = "Análisis de sentimientos de tweets que contienen la palabra 'FiscaliaNL'",
  xlab="Emociones", ylab = NULL)


#============================================================================
#Palabra desaparecidas
#Minar TW
desaparecidas_tw <-search_tweets("desaparecidas", n=5000)

#Construcción de base de datos
col1.2 <- desaparecidas_tw$created_at
col2.2 <- desaparecidas_tw$full_text
base.datos2 <- data_frame(col1.2,col2.2)
write.csv(base.datos2, "DesaparecidasTW.csv")

tidy_desaparecidas_tw <- desaparecidas_tw %>%
  select(created_at,full_text) %>%
  unnest_tokens("word", full_text)
head(tidy_desaparecidas_tw)

#Limpiar el texto
quitar <- c("rt", "pedrootamendi", "equ","https",
            "t.co","leirisgsm","30","16","2","4","kenialopezr","d")

df_limpio3 <- tidy_desaparecidas_tw %>%
  filter(!(word %in% stop_words_es)) %>%
  filter(!(word %in% quitar)) %>%
  count(word) %>%
  arrange(desc(n))

library(syuzhet)
texto_desaparecidas <- df_limpio3$word
sentimientos_desaparecidas <- get_nrc_sentiment(df_limpio3$word, lang="spanish")

barplot(
  colSums(prop.table(sentimientos_desaparecidas [, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.8,
  col = topo.colors (12),
  main = "Análisis de sentimientos de tweets que contienen la palabra 'desaparecidas'",
  xlab="Emociones", ylab = NULL)

#============================================================================
#Palabra feminicidio
#Minar TW
feminicidio_tw <-search_tweets("feminicidio", n=5000)

#Construcción de base de datos
col1 <- feminicidio_tw$created_at
col2 <- feminicidio_tw$full_text
base.datos3 <- data_frame(col1,col2)
write.csv(base.datos3, "FeminicidiosTW.csv")

tidy_feminicidio_tw <- feminicidio_tw %>%
  select(created_at,full_text) %>%
  unnest_tokens("word", full_text)
head(tidy_feminicidio_tw)

#Limpiar el texto
quitar <- c("rt", "pedrootamendi", "equ","https",
            "t.co","leirisgsm","30","16","2","4","kenialopezr","d")

df_feminicido <- tidy_feminicidio_tw %>%
  filter(!(word %in% stop_words_es)) %>%
  filter(!(word %in% quitar)) %>%
  count(word) %>%
  arrange(desc(n))

library(syuzhet)
sentimiento_feminicidio <- get_nrc_sentiment(df_feminicido$word, lang="spanish")

barplot(
  colSums(prop.table(sentimiento_feminicidio  [, 1:8])),
  space = 0.1,
  horiz = FALSE,
  las = 1,
  cex.names = 0.8,
  col = topo.colors (12),
  main = "Análisis de sentimientos de tweets que contienen la palabra 'feminicidio'",
  xlab="Emociones", ylab = NULL)













