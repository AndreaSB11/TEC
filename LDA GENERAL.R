#LDA COMPLETO 

## Instalacion de librerias
# Instalar - Cargar tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}

# Instalar - Cargar tidytext                                                       
if(require(tidytext) == FALSE){                                                
  install.packages('tidytext')                                                 
  library(tidytext)                                                            
}else{                                                                          
  library(tidytext)                                                            
}

# Instalar - Cargar stopwords                                                       
if(require(stopwords) == FALSE){                                                
  install.packages('stopwords')                                                 
  library(stopwords)                                                            
}else{                                                                          
  library(stopwords)                                                            
}

# Instalar - Cargar SnowballC                                                       
if(require(SnowballC) == FALSE){                                                
  install.packages('SnowballC')                                                 
  library(SnowballC)                                                            
}else{                                                                          
  library(SnowballC)                                                            
}

# Instalar - Cargar tm                                                       
if(require(tm) == FALSE){                                                
  install.packages('tm')                                                 
  library(tm)                                                            
}else{                                                                          
  library(tm)                                                            
}

# Instalar - Cargar topicmodels                                                       
if(require(topicmodels) == FALSE){                                                
  install.packages('topicmodels')                                                 
  library(topicmodels)                                                            
}else{                                                                          
  library(topicmodels)                                                            
}

## Instalacion de librerias
# Instalar - Cargar wordcloud                                                     
if(require(wordcloud) == FALSE){                                                
  install.packages('wordcloud')                                                 
  library(wordcloud)                                                            
}else{                                                                          
  library(wordcloud)                                                            
}


# Cargamos nuestras noticias
noticias = read_csv("")

# Creamos un ID y nuestra categoria
noticias_1 = noticias %>%
  mutate(
    # Creamos un ID unico
    id = titulo,
    # Corregimos los nombres en autoria
    autor = str_trim(
      str_remove_all(
        string = autor,
        pattern = '\n|\\*|\\d+|V?\\.|/ ?[\\w]+'), 
      side = 'both'
    ),
    autor = if_else(autor=="", 'Ahora Noticas', autor),
    # Eliminamos urls y espacios dobles
    texto_limpio = str_remove_all(
      string = texto,
      pattern = 'https://.*|bit\\.ly.+'),
    texto_limpio = str_replace_all(texto_limpio, ' {2,}',' '),
  )

# Extraemos bigramas para encontrar nombres propios
bigramas = noticias_1 %>%
  unnest_tokens(
    input = texto_raw, 
    output = 'bigramas', 
    token = 'ngrams', n = 2,
    to_lower = FALSE
  )  View (bigramas) %>%
  # Contamos cuantas veces aparecen
  count(bigramas,  sort = TRUE) %>%
  # Filtramos con una expression regular para obtener solo nombres propios
  filter(
    str_detect(
      string = bigramas,
      pattern = '[A-ZAÉÍÓÚÑ]\\w+ [A-ZAÉÍÓÚÑ\\d][\\w]*'
    )
  ) %>%
  # Unimos con un _ en vez de un espacio
  mutate(
    ners = str_replace_all(bigramas, ' ', '_')
  ) %>%
  # Nos quedamos con el bigrama original y las ners
  select(
    bigramas, ners
  ) 

# Creamos un diccionario
bigramas = setNames(bigramas$ners, bigramas$bigramas)

# Definimos nuestras palabras de paro
stop_words = tibble(palabra = c(stopwords('es'),'así','si','sí','sino','ayer',
                                'hoy','año','sólo'))

# Creamos nuestra bolsa de palabras
noticias_tokens = noticias_1 %>%
  # Replazamos los bigramas por las ners
  mutate(
    texto_limpio = str_replace_all(texto_limpio,bigramas)
  ) %>%
  # Tokenizamos
  unnest_tokens(
    output = "palabra", 
    token = "words", 
    input = texto_limpio
  ) %>%
  # Removemos las palabras de paro
  anti_join(
    stop_words
  )  %>%
  # filtramos todo lo que no sea una palabra
  filter(
    !str_detect(palabra,'[^\\w_]|\\d'),
  )

# Creamos nuestra DTM
noticias_matriz = noticias_tokens %>%
  # Conteo de palabras por articulo
  count(id, palabra) %>%
  # Creamos la dtm
  cast_dtm(
    # Identificador de cada documento
    document = id, 
    # Terminos a evaluar
    term = palabra,
    # Valor de las celdas
    value = n, 
    # Ponderadores
    weighting = tm::weightTf
  )

# Eliminamos elementos ralos
matriz_menos_rala = noticias_matriz %>%
  removeSparseTerms(sparse = 0.9)

# Fijamos nuestro conjunto de prueba y entrenamiento
sample_size = floor(0.90 * nrow(matriz_menos_rala))
set.seed(1111)
train_ind = sample(nrow(matriz_menos_rala), size = sample_size)
train = matriz_menos_rala[train_ind, ]
test = matriz_menos_rala[-train_ind, ]

# Ajustamos nuestro modelo
lda_model = LDA(train, k = 5, method = 'Gibbs',
                control = list(seed = 1111))

# Evaluar log-verosimilitud
logLik(lda_model)

# Evaluar perplejidad o grado de sorpresa(menos es mejor)
perplexity(lda_model, newdata = train) 
perplexity(lda_model, newdata = test) 

# Estructura
str(lda_model)

# Definiendo el optimo
# NOTA: NO EXISTE UNA REGLA DE DEDO, HAY VECES QUE UN RESULTADO INTUITIVO Y DE 
# POCOS TOPICOS ES MEJOR QUE UNO ANALITICO CON CIENTOS DE TOPICOS
modelos = c()
for(k in seq(2,20)){
  # Ajustamos nuestro modelo
  lda_model = LDA(train, k = k, method = 'Gibbs',
                  control = list(seed = 1111))
  metricas = tibble(
    topics = k,
    perplejidad = perplexity(lda_model, newdata = test),
    verosimilitud = logLik(lda_model)[[1]]
  )
  modelos = bind_rows(modelos, metricas)
}

# Graficando las metricas
ggplot(
  data = modelos,
  aes(
    x = topics,
    y = perplejidad
  )
) +
  geom_point() +
  geom_line()

ggplot(
  data = modelos,
  aes(
    x = topics,
    y = verosimilitud
  )
) +
  geom_point() +
  geom_line()


# Ajustamos nuestro modelo final
lda_model = LDA(train, k = 5, method = 'Gibbs',
                control = list(seed = 1111))

# Estructura
str(lda_model)

# Evaluar log-verosimilitud
logLik(lda_model)

# Evaluar perplejidad o grado de sorpresa(menos es mejor)
perplexity(lda_model, newdata = train) 
perplexity(lda_model, newdata = test) 

# Probabilidad de documento perteneciente a un topico (gamma)
gammas = tidy(lda_model, matrix="gamma")
head(gammas)

# Podemos visualizar la propension de topicos en los documentos
gammas %>% 
  mutate(topic = as.factor(topic)) %>% 
  ggplot(
    aes(
      x=gamma,
      y=document, 
      fill = topic
    )
  ) + 
  geom_col()

# La matriz de probabilidades
gammas %>%
  spread(topic, gamma) 

# Mapa de calor
gammas %>%
  ggplot(
    aes(
      x = topic,
      y = document,
      fill = gamma
    )
  ) +
  geom_tile()

#Si agrupamos las gammas
grouped_gammas <- gammas %>%
  group_by(document) %>%
  arrange(desc(gamma)) %>%
  slice(1) %>%
  group_by(topic)

# Numero de documentos por topico preponderante
grouped_gammas %>% 
  tally(topic, sort=TRUE)

# Presencia promedio de cada topico en los documentos
grouped_gammas %>% 
  summarise(avg=mean(gamma)) %>%
  arrange(desc(avg))

# Boxplot
ggplot(
  data = gammas,
  aes(
    x = factor(topic),
    y = gamma,
    fill = factor(topic)
  )
) +
  geom_boxplot()

# Violines
ggplot(
  data = gammas,
  aes(
    x = factor(topic),
    y = gamma,
    fill = factor(topic)
  )
) +
  geom_violin()

# dispersion
ggplot(
  data = gammas,
  aes(
    x = factor(topic),
    y = gamma,
    col = factor(topic)
  )
) +
  geom_jitter()


# Probabilidad de palabra perteneciente a un topico (beta)
betas = tidy(lda_model, matrix="beta")
head(betas)

# Top15 de palabras para cada topico
terms(lda_model, k=15)

# Display wordclouds one at a time
for (j in 1:5) {
  # Generate a table with word frequences for topic j
  word_frequencies <- betas %>% 
    mutate(n = trunc(beta * 10000)) %>% 
    filter(topic == j)
  
  #Hasta aquí me sale sólo con una fuente. 
  
  
  
  # Display word cloud
  wordcloud(words = word_frequencies$term, 
            freq = word_frequencies$n,
            max.words = 100,
            #  min.freq=1,
            #  max.words=10,
            scale = c(5, 1),
            colors = c("DarkOrange", "CornflowerBlue", "DarkRed"), 
            family = "Arial",
            rot.per = 0)
}

# Podemos renomrar los topicos a partir de los hallazgos para hacer el analisis mas claro

