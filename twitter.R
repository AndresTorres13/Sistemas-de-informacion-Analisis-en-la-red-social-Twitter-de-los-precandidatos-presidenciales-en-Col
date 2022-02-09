setwd("F:/Descargas")
candidatos<-read.csv("candidatos.csv", encoding="UTF-8")

#ANALISIS DE CANDIDATOS

library(dplyr)

candidatos %>% group_by(candidato) %>% summarise(numero_tweets = n()) 

# Selección de variables
tweets <- candidatos %>% select(id, tweet, candidato, fecha_creacion)

# Se renombran las variables con nombres más prácticos
tweets <- tweets %>% rename(autor = candidato, fecha = fecha_creacion,
                            texto = tweet, tweet_id = id)

#Limpieza de Texto

library(tidyverse)

limpiar_tokenizar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  # Tokenización por palabras individuales
  nuevo_texto <- str_split(nuevo_texto, " ")[[1]]
  # Eliminación de tokens con una longitud < 2
  nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1})
  return(nuevo_texto)
}

# Se aplica la función de limpieza y tokenización a cada tweet
library(purrr)

tweets <- tweets %>% mutate(texto_tokenizado = map(.x = texto,
                                                   .f = limpiar_tokenizar))
tweets %>% select(texto_tokenizado) %>% head()

#Analisis Exploratorio
library(tidyr)

tweets_tidy <- tweets %>% select(-texto) %>% unnest()
tweets_tidy <- tweets_tidy %>% rename(token = texto_tokenizado)
#Distribuión temporal de los tweets

library(lubridate)
library(ggplot2)

windows()
ggplot(tweets, aes(x = as.Date(fecha), fill = autor)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "5 month") +
  labs(x = "fecha de publicación", y = "número de tweets") +
  facet_wrap(~ autor, ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

#Frecuencia de palabras
windows()

tweets_tidy %>%  ggplot(aes(x = autor)) + geom_bar() + coord_flip() + theme_bw()
#palabras distintas
tweets_tidy %>% select(autor, token) %>% distinct() %>%
  ggplot(aes(x = autor)) + geom_bar() + coord_flip() + theme_bw()

#longitud media de los tweets
tweets_tidy %>% group_by(autor, tweet_id) %>% summarise(longitud = n()) %>%                      group_by(autor) %>%
  summarise(media_longitud = mean(longitud),
            sd_longitud = sd(longitud)) %>%
  ggplot(aes(x = autor, y = media_longitud)) +
  geom_col() +
  geom_errorbar(aes(ymin = media_longitud - sd_longitud,
                    ymax = media_longitud + sd_longitud)) +
  coord_flip() + theme_bw()

#Removiendo palabras sin informacion relevante
library(tm)
lista_stopwords <- tm::stopwords("spanish")
lista_stopwords <- c(lista_stopwords, "país","vamos","ser","aquí","vamos",
                     "hoy","si","solo","hable","ver","va","siempre","gracias")

# Se filtran las stopwords
tweets_tidy <- tweets_tidy %>% filter(!(token %in% lista_stopwords))


tweets_tidy %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
  top_n(5, n) %>% arrange(autor, desc(n)) %>%
  ggplot(aes(x = reorder(token,n), y = n, fill = autor)) +
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  facet_wrap(~autor,scales = "free", ncol = 1, drop = TRUE)

#nube de palabras

library(wordcloud)
library(RColorBrewer)

wordcloud_custom <- function(grupo, df){
  print(grupo)
  wordcloud(words = df$token, freq = df$frecuencia,
            max.words = 400, random.order = FALSE, rot.per = 0.35,
            colors = brewer.pal(8, "Dark2"))
}

df_grouped <- tweets_tidy %>% group_by(autor, token) %>% count(token) %>%
  group_by(autor) %>% mutate(frecuencia = n / n()) %>%
  arrange(autor, desc(frecuencia)) %>% nest() 
windows()
par(mfrow=c(2,2))
walk2(.x = df_grouped$autor, .y = df_grouped$data, .f = wordcloud_custom)

#correlacion entre candidatos

library(gridExtra)
library(scales)

tweets_spread <- tweets_tidy %>% group_by(autor, token) %>% count(token) %>%
  spread(key = autor, value = n, fill = NA, drop = TRUE)

cor_matrix <- matrix(NA,7,7)
cor_matrix <- as.data.frame(cor_matrix)
names(cor_matrix)<-names(tweets_spread[,2:8])
rownames(cor_matrix) <- names(cor_matrix)
cor_matrix[1,1]<-1
cor<-cor.test(~ `Alejandro Gaviria` + `Federico Gutiérrez`, method = "pearson", data = tweets_spread)
cor_matrix[2,1]<-round(cor$estimate,2)
cor<-cor.test(~ `Alejandro Gaviria` + `Francia Márquez Mina`, method = "pearson", data = tweets_spread)
cor_matrix[3,1]<-round(cor$estimate,2)
cor<-cor.test(~ `Alejandro Gaviria` + `Gustavo Petro`, method = "pearson", data = tweets_spread)
cor_matrix[4,1]<-round(cor$estimate,2)
cor<-cor.test(~ `Alejandro Gaviria` + `Ing Rodolfo Hernandez`, method = "pearson", data = tweets_spread)
cor_matrix[5,1]<-round(cor$estimate,2)
cor<-cor.test(~ `Alejandro Gaviria` + `Juan Manuel Galán`, method = "pearson", data = tweets_spread)
cor_matrix[6,1]<-round(cor$estimate,2)
cor<-cor.test(~ `Alejandro Gaviria` + `Sergio Fajardo`, method = "pearson", data = tweets_spread)
cor_matrix[7,1]<-round(cor$estimate,2)
cor<-cor.test(~ `Federico Gutiérrez` + `Alejandro Gaviria`, method = "pearson", data = tweets_spread)
cor_matrix[1,2]<-round(cor$estimate,2)
cor_matrix[2,2]<-1
cor<-cor.test(~ `Federico Gutiérrez` + `Francia Márquez Mina`, method = "pearson", data = tweets_spread)
cor_matrix[3,2]<-round(cor$estimate,2)
cor<-cor.test(~ `Federico Gutiérrez` + `Gustavo Petro`, method = "pearson", data = tweets_spread)
cor_matrix[4,2]<-round(cor$estimate,2)
cor<-cor.test(~ `Federico Gutiérrez` + `Ing Rodolfo Hernandez`, method = "pearson", data = tweets_spread)
cor_matrix[5,2]<-round(cor$estimate,2)
cor<-cor.test(~ `Federico Gutiérrez` + `Juan Manuel Galán`, method = "pearson", data = tweets_spread)
cor_matrix[6,2]<-round(cor$estimate,2)
cor<-cor.test(~ `Federico Gutiérrez` + `Sergio Fajardo`, method = "pearson", data = tweets_spread)
cor_matrix[7,2]<-round(cor$estimate,2)
cor<-cor.test(~ `Francia Márquez Mina` + `Alejandro Gaviria`, method = "pearson", data = tweets_spread)
cor_matrix[1,3]<-round(cor$estimate,2)
cor<-cor.test(~ `Francia Márquez Mina` + `Federico Gutiérrez`, method = "pearson", data = tweets_spread)
cor_matrix[2,3]<-round(cor$estimate,2)
cor_matrix[3,3]<-1
cor<-cor.test(~ `Francia Márquez Mina` + `Gustavo Petro`, method = "pearson", data = tweets_spread)
cor_matrix[4,3]<-round(cor$estimate,2)
cor<-cor.test(~ `Francia Márquez Mina` + `Ing Rodolfo Hernandez`, method = "pearson", data = tweets_spread)
cor_matrix[5,3]<-round(cor$estimate,2)
cor<-cor.test(~ `Francia Márquez Mina` + `Juan Manuel Galán`, method = "pearson", data = tweets_spread)
cor_matrix[6,3]<-round(cor$estimate,2)
cor<-cor.test(~ `Francia Márquez Mina` + `Sergio Fajardo`, method = "pearson", data = tweets_spread)
cor_matrix[7,3]<-round(cor$estimate,2)
cor<-cor.test(~ `Gustavo Petro` + `Alejandro Gaviria`, method = "pearson", data = tweets_spread)
cor_matrix[1,4]<-round(cor$estimate,2)
cor<-cor.test(~ `Gustavo Petro` + `Federico Gutiérrez`, method = "pearson", data = tweets_spread)
cor_matrix[2,4]<-round(cor$estimate,2)
cor<-cor.test(~ `Gustavo Petro` + `Francia Márquez Mina`, method = "pearson", data = tweets_spread)
cor_matrix[3,4]<-round(cor$estimate,2)
cor_matrix[4,4]<-1
cor<-cor.test(~ `Gustavo Petro` + `Ing Rodolfo Hernandez`, method = "pearson", data = tweets_spread)
cor_matrix[5,4]<-round(cor$estimate,2)
cor<-cor.test(~ `Gustavo Petro` + `Juan Manuel Galán`, method = "pearson", data = tweets_spread)
cor_matrix[6,4]<-round(cor$estimate,2)
cor<-cor.test(~ `Gustavo Petro` + `Sergio Fajardo`, method = "pearson", data = tweets_spread)
cor_matrix[7,4]<-round(cor$estimate,2)
cor<-cor.test(~ `Ing Rodolfo Hernandez` + `Alejandro Gaviria`, method = "pearson", data = tweets_spread)
cor_matrix[1,5]<-round(cor$estimate,2)
cor<-cor.test(~ `Ing Rodolfo Hernandez` + `Federico Gutiérrez`, method = "pearson", data = tweets_spread)
cor_matrix[2,5]<-round(cor$estimate,2)
cor<-cor.test(~ `Ing Rodolfo Hernandez` + `Francia Márquez Mina`, method = "pearson", data = tweets_spread)
cor_matrix[3,5]<-round(cor$estimate,2)
cor<-cor.test(~ `Ing Rodolfo Hernandez` + `Gustavo Petro`, method = "pearson", data = tweets_spread)
cor_matrix[4,5]<-round(cor$estimate,2)
cor_matrix[5,5]<-1
cor<-cor.test(~ `Ing Rodolfo Hernandez` + `Juan Manuel Galán`, method = "pearson", data = tweets_spread)
cor_matrix[6,5]<-round(cor$estimate,2)
cor<-cor.test(~ `Ing Rodolfo Hernandez` + `Sergio Fajardo`, method = "pearson", data = tweets_spread)
cor_matrix[7,5]<-round(cor$estimate,2)
cor<-cor.test(~ `Juan Manuel Galán` + `Alejandro Gaviria`, method = "pearson", data = tweets_spread)
cor_matrix[1,6]<-round(cor$estimate,2)
cor<-cor.test(~ `Juan Manuel Galán` + `Federico Gutiérrez`, method = "pearson", data = tweets_spread)
cor_matrix[2,6]<-round(cor$estimate,2)
cor<-cor.test(~ `Juan Manuel Galán` + `Francia Márquez Mina`, method = "pearson", data = tweets_spread)
cor_matrix[3,6]<-round(cor$estimate,2)
cor<-cor.test(~ `Juan Manuel Galán` + `Gustavo Petro`, method = "pearson", data = tweets_spread)
cor_matrix[4,6]<-round(cor$estimate,2)
cor<-cor.test(~ `Juan Manuel Galán` + `Ing Rodolfo Hernandez`, method = "pearson", data = tweets_spread)
cor_matrix[5,6]<-round(cor$estimate,2)
cor_matrix[6,6]<-1
cor<-cor.test(~ `Juan Manuel Galán` + `Sergio Fajardo`, method = "pearson", data = tweets_spread)
cor_matrix[7,6]<-round(cor$estimate,2)
cor_matrix[1,7]<-cor_matrix[7,1]
cor_matrix[2,7]<-cor_matrix[7,2]
cor_matrix[3,7]<-cor_matrix[7,3]
cor_matrix[4,7]<-cor_matrix[7,4]
cor_matrix[5,7]<-cor_matrix[7,5]
cor_matrix[6,7]<-cor_matrix[7,6]
cor_matrix[7,7]<-1

cor_matrix

#Analisis de Sentimiento

#descarga de archivo
download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv",
              "lexico_afinn.en.es.csv")
afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  tbl_df()
afinn <- afinn[,1:2]
tweets_sent <- inner_join(x = tweets_tidy, y = afinn,
                          by = c("token" = "Palabra"))

#Puntuación de algunos tweets
tweets_sent %>% group_by(autor, tweet_id) %>%
  summarise(sentimiento_promedio = sum(Puntuacion)) %>%
  head()

#Porcentajes de sentimiento
tweets_sent %>% group_by(autor, tweet_id) %>%
  summarise(sentimiento_promedio = sum(Puntuacion)) %>%
  group_by(autor) %>%
  summarise(positivos = 100 * sum(sentimiento_promedio > 0) / n(),
            neutros = 100 * sum(sentimiento_promedio == 0) / n(),
            negativos = 100 * sum(sentimiento_promedio  < 0) / n())

#Diagrama de barras de sentimiento
windows()
tweets_sent %>% group_by(autor, tweet_id) %>%
  summarise(sentimiento_promedio = sum(Puntuacion)) %>%
  group_by(autor) %>%
  summarise(positivos = 100*sum(sentimiento_promedio > 0) / n(),
            neutros = 100*sum(sentimiento_promedio == 0) / n(),
            negativos = 100*sum(sentimiento_promedio  < 0) / n()) %>%
  ungroup() %>%
  gather(key = "sentimiento", value = "valor", -autor) %>%
  ggplot(aes(x = autor, y = valor, fill = sentimiento)) + 
  geom_col(position = "dodge", color = "black") + coord_flip() +
  labs(x="Candidato", y="Porcentaje") +
  theme_bw()

#Evolucio de sentimiento a traves de una función del tiempo

library(lubridate)
tweets_sent %>% mutate(anyo = year(fecha),
                       mes = month(fecha),
                       anyo_mes = ymd(paste(anyo, mes, sep="-"),truncated=2)) %>%
  group_by(autor, anyo_mes) %>%
  summarise(sentimiento = mean(Puntuacion)) %>%
  ungroup() %>%
  ggplot(aes(x = anyo_mes, y = sentimiento, color = autor)) +
  geom_point() + 
  geom_smooth() + 
  labs(x = "fecha de publicación") +
  facet_wrap(~ autor, ncol = 1) +
  theme_bw() +
  theme(legend.position = "none")

#ANALISIS DE MENCIONES

#datos
petro <- read.csv("Menciones.petro.csv", encoding="UTF-8")
candidato <- rep("Gustavo Petro",66)
petro <-data.frame(petro,candidato)
petro<- petro[,c(2,5,6)]
fico <- read.csv("Menciones.fico2.csv", encoding="UTF-8")
candidato <- rep("Federico Gutiérrez",78)
fico <-data.frame(fico,candidato)
fico<- fico[,c(2,5,6)]
gaviria <- read.csv("Menciones.Gaviria.csv", encoding="UTF-8")
candidato <- rep("Alejandro Gaviria",84)
gaviria <-data.frame(gaviria,candidato)
gaviria<- gaviria[,c(2,5,6)]
francia <- read.csv("Menciones.Francia.csv", encoding="UTF-8")
candidato <- rep("Francia Márquez Mina",70)
francia <-data.frame(francia,candidato)
francia<- francia[,c(2,5,6)]
rodolfo <- read.csv("Menciones.Rodolfo.csv", encoding="UTF-8")
candidato <- rep("Ing Rodolfo Hernandez",91)
rodolfo <-data.frame(rodolfo,candidato)
rodolfo<- rodolfo[,c(2,5,6)]
fajardo1 <- read.csv("Menciones.Fajardo.csv", encoding="UTF-8")
fajardo2 <- read.csv("Menciones.Fajardo2.csv", encoding="UTF-8")
fajardo <- rbind(fajardo1,fajardo2)
candidato <- rep("Sergio Fajardo",33)
fajardo <-data.frame(fajardo,candidato)
fajardo<- fajardo[,c(2,5,6)]
galan <- read.csv("Menciones.Galan.csv", encoding="UTF-8")
candidato <- rep("Juan Manuel Galán",56)
galan <-data.frame(galan,candidato)
galan<- galan[,c(2,5,6)]

menciones <-rbind(gaviria,fico,francia,petro,rodolfo,galan,fajardo)
id <- c(1:478)
menciones <-data.frame(id,menciones)

# Se renombran las variables con nombres más prácticos
tweets <- menciones %>% rename(autor = candidato, fecha = fecha_creaccion,
                               texto = tweet, tweet_id = id)

#Limpieza de Texto

library(tidyverse)

limpiar_tokenizar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  # Tokenización por palabras individuales
  nuevo_texto <- str_split(nuevo_texto, " ")[[1]]
  # Eliminación de tokens con una longitud < 2
  nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1})
  return(nuevo_texto)
}

# Se aplica la función de limpieza y tokenización a cada tweet
library(purrr)

tweets <- tweets %>% mutate(texto_tokenizado = map(.x = texto,
                                                   .f = limpiar_tokenizar))
tweets %>% select(texto_tokenizado) %>% head()

#Analisis Exploratorio
library(tidyr)

tweets_tidy <- tweets %>% select(-texto) %>% unnest()
tweets_tidy <- tweets_tidy %>% rename(token = texto_tokenizado)
#Distribuión temporal de los tweets

library(lubridate)
library(ggplot2)

windows()
ggplot(tweets, aes(x = as.Date(fecha), fill = autor)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "5 month") +
  labs(x = "fecha de publicación", y = "número de tweets") +
  facet_wrap(~ autor, ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

#Frecuencia de palabras
windows()

tweets_tidy %>%  ggplot(aes(x = autor)) + geom_bar() + coord_flip() + theme_bw()
#palabras distintas
tweets_tidy %>% select(autor, token) %>% distinct() %>%
  ggplot(aes(x = autor)) + geom_bar() + coord_flip() + theme_bw()

#longitud media de los tweets
tweets_tidy %>% group_by(autor, tweet_id) %>% summarise(longitud = n()) %>%                      group_by(autor) %>%
  summarise(media_longitud = mean(longitud),
            sd_longitud = sd(longitud)) %>%
  ggplot(aes(x = autor, y = media_longitud)) +
  geom_col() +
  geom_errorbar(aes(ymin = media_longitud - sd_longitud,
                    ymax = media_longitud + sd_longitud)) +
  coord_flip() + theme_bw()

#Removiendo palabras sin informacion relevante
library(tm)
lista_stopwords <- tm::stopwords("spanish")
lista_stopwords <- c(lista_stopwords,    "rt", "país","vamos","ser","aquí","vamos",
                     "hoy","si","solo","hable","ver","va","siempre","gracias")

# Se filtran las stopwords
tweets_tidy <- tweets_tidy %>% filter(!(token %in% lista_stopwords))


tweets_tidy %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
  top_n(5, n) %>% arrange(autor, desc(n)) %>%
  ggplot(aes(x = reorder(token,n), y = n, fill = autor)) +
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  facet_wrap(~autor,scales = "free", ncol = 1, drop = TRUE)

#nube de palabras

library(wordcloud)
library(RColorBrewer)

wordcloud_custom <- function(grupo, df){
  print(grupo)
  wordcloud(words = df$token, freq = df$frecuencia,
            max.words = 400, random.order = FALSE, rot.per = 0.35,
            colors = brewer.pal(8, "Dark2"))
}

df_grouped <- tweets_tidy %>% group_by(autor, token) %>% count(token) %>%
  group_by(autor) %>% mutate(frecuencia = n / n()) %>%
  arrange(autor, desc(frecuencia)) %>% nest() 
windows()
par(mfrow=c(2,4))
walk2(.x = df_grouped$autor, .y = df_grouped$data, .f = wordcloud_custom)

#correlacion entre candidatos

library(gridExtra)
library(scales)

tweets_spread <- tweets_tidy %>% group_by(autor, token) %>% count(token) %>%
  spread(key = autor, value = n, fill = NA, drop = TRUE)

cor_matrix <- matrix(NA,7,7)
cor_matrix <- as.data.frame(cor_matrix)
names(cor_matrix)<-names(tweets_spread[,2:8])
rownames(cor_matrix) <- names(cor_matrix)
cor_matrix[1,1]<-1
cor<-cor.test(~ `Alejandro Gaviria` + `Federico Gutiérrez`, method = "pearson", data = tweets_spread)
cor_matrix[2,1]<-round(cor$estimate,2)
cor<-cor.test(~ `Alejandro Gaviria` + `Francia Márquez Mina`, method = "pearson", data = tweets_spread)
cor_matrix[3,1]<-round(cor$estimate,2)
cor<-cor.test(~ `Alejandro Gaviria` + `Gustavo Petro`, method = "pearson", data = tweets_spread)
cor_matrix[4,1]<-round(cor$estimate,2)
cor<-cor.test(~ `Alejandro Gaviria` + `Ing Rodolfo Hernandez`, method = "pearson", data = tweets_spread)
cor_matrix[5,1]<-round(cor$estimate,2)
cor<-cor.test(~ `Alejandro Gaviria` + `Juan Manuel Galán`, method = "pearson", data = tweets_spread)
cor_matrix[6,1]<-round(cor$estimate,2)
cor<-cor.test(~ `Alejandro Gaviria` + `Sergio Fajardo`, method = "pearson", data = tweets_spread)
cor_matrix[7,1]<-round(cor$estimate,2)
cor<-cor.test(~ `Federico Gutiérrez` + `Alejandro Gaviria`, method = "pearson", data = tweets_spread)
cor_matrix[1,2]<-round(cor$estimate,2)
cor_matrix[2,2]<-1
cor<-cor.test(~ `Federico Gutiérrez` + `Francia Márquez Mina`, method = "pearson", data = tweets_spread)
cor_matrix[3,2]<-round(cor$estimate,2)
cor<-cor.test(~ `Federico Gutiérrez` + `Gustavo Petro`, method = "pearson", data = tweets_spread)
cor_matrix[4,2]<-round(cor$estimate,2)
cor<-cor.test(~ `Federico Gutiérrez` + `Ing Rodolfo Hernandez`, method = "pearson", data = tweets_spread)
cor_matrix[5,2]<-round(cor$estimate,2)
cor<-cor.test(~ `Federico Gutiérrez` + `Juan Manuel Galán`, method = "pearson", data = tweets_spread)
cor_matrix[6,2]<-round(cor$estimate,2)
cor<-cor.test(~ `Federico Gutiérrez` + `Sergio Fajardo`, method = "pearson", data = tweets_spread)
cor_matrix[7,2]<-round(cor$estimate,2)
cor<-cor.test(~ `Francia Márquez Mina` + `Alejandro Gaviria`, method = "pearson", data = tweets_spread)
cor_matrix[1,3]<-round(cor$estimate,2)
cor<-cor.test(~ `Francia Márquez Mina` + `Federico Gutiérrez`, method = "pearson", data = tweets_spread)
cor_matrix[2,3]<-round(cor$estimate,2)
cor_matrix[3,3]<-1
cor<-cor.test(~ `Francia Márquez Mina` + `Gustavo Petro`, method = "pearson", data = tweets_spread)
cor_matrix[4,3]<-round(cor$estimate,2)
cor<-cor.test(~ `Francia Márquez Mina` + `Ing Rodolfo Hernandez`, method = "pearson", data = tweets_spread)
cor_matrix[5,3]<-round(cor$estimate,2)
cor<-cor.test(~ `Francia Márquez Mina` + `Juan Manuel Galán`, method = "pearson", data = tweets_spread)
cor_matrix[6,3]<-round(cor$estimate,2)
cor<-cor.test(~ `Francia Márquez Mina` + `Sergio Fajardo`, method = "pearson", data = tweets_spread)
cor_matrix[7,3]<-round(cor$estimate,2)
cor<-cor.test(~ `Gustavo Petro` + `Alejandro Gaviria`, method = "pearson", data = tweets_spread)
cor_matrix[1,4]<-round(cor$estimate,2)
cor<-cor.test(~ `Gustavo Petro` + `Federico Gutiérrez`, method = "pearson", data = tweets_spread)
cor_matrix[2,4]<-round(cor$estimate,2)
cor<-cor.test(~ `Gustavo Petro` + `Francia Márquez Mina`, method = "pearson", data = tweets_spread)
cor_matrix[3,4]<-round(cor$estimate,2)
cor_matrix[4,4]<-1
cor<-cor.test(~ `Gustavo Petro` + `Ing Rodolfo Hernandez`, method = "pearson", data = tweets_spread)
cor_matrix[5,4]<-round(cor$estimate,2)
cor<-cor.test(~ `Gustavo Petro` + `Juan Manuel Galán`, method = "pearson", data = tweets_spread)
cor_matrix[6,4]<-round(cor$estimate,2)
cor<-cor.test(~ `Gustavo Petro` + `Sergio Fajardo`, method = "pearson", data = tweets_spread)
cor_matrix[7,4]<-round(cor$estimate,2)
cor<-cor.test(~ `Ing Rodolfo Hernandez` + `Alejandro Gaviria`, method = "pearson", data = tweets_spread)
cor_matrix[1,5]<-round(cor$estimate,2)
cor<-cor.test(~ `Ing Rodolfo Hernandez` + `Federico Gutiérrez`, method = "pearson", data = tweets_spread)
cor_matrix[2,5]<-round(cor$estimate,2)
cor<-cor.test(~ `Ing Rodolfo Hernandez` + `Francia Márquez Mina`, method = "pearson", data = tweets_spread)
cor_matrix[3,5]<-round(cor$estimate,2)
cor<-cor.test(~ `Ing Rodolfo Hernandez` + `Gustavo Petro`, method = "pearson", data = tweets_spread)
cor_matrix[4,5]<-round(cor$estimate,2)
cor_matrix[5,5]<-1
cor<-cor.test(~ `Ing Rodolfo Hernandez` + `Juan Manuel Galán`, method = "pearson", data = tweets_spread)
cor_matrix[6,5]<-round(cor$estimate,2)
cor<-cor.test(~ `Ing Rodolfo Hernandez` + `Sergio Fajardo`, method = "pearson", data = tweets_spread)
cor_matrix[7,5]<-round(cor$estimate,2)
cor<-cor.test(~ `Juan Manuel Galán` + `Alejandro Gaviria`, method = "pearson", data = tweets_spread)
cor_matrix[1,6]<-round(cor$estimate,2)
cor<-cor.test(~ `Juan Manuel Galán` + `Federico Gutiérrez`, method = "pearson", data = tweets_spread)
cor_matrix[2,6]<-round(cor$estimate,2)
cor<-cor.test(~ `Juan Manuel Galán` + `Francia Márquez Mina`, method = "pearson", data = tweets_spread)
cor_matrix[3,6]<-round(cor$estimate,2)
cor<-cor.test(~ `Juan Manuel Galán` + `Gustavo Petro`, method = "pearson", data = tweets_spread)
cor_matrix[4,6]<-round(cor$estimate,2)
cor<-cor.test(~ `Juan Manuel Galán` + `Ing Rodolfo Hernandez`, method = "pearson", data = tweets_spread)
cor_matrix[5,6]<-round(cor$estimate,2)
cor_matrix[6,6]<-1
cor<-cor.test(~ `Juan Manuel Galán` + `Sergio Fajardo`, method = "pearson", data = tweets_spread)
cor_matrix[7,6]<-round(cor$estimate,2)
cor_matrix[1,7]<-cor_matrix[7,1]
cor_matrix[2,7]<-cor_matrix[7,2]
cor_matrix[3,7]<-cor_matrix[7,3]
cor_matrix[4,7]<-cor_matrix[7,4]
cor_matrix[5,7]<-cor_matrix[7,5]
cor_matrix[6,7]<-cor_matrix[7,6]
cor_matrix[7,7]<-1

cor_matrix

#Analisis de Sentimiento

tweets_sent <- inner_join(x = tweets_tidy, y = afinn,
                          by = c("token" = "Palabra"))

#Puntuación de algunos tweets
tweets_sent %>% group_by(autor, tweet_id) %>%
  summarise(sentimiento_promedio = sum(Puntuacion)) %>%
  head()

#Porcentajes de sentimiento
tweets_sent %>% group_by(autor, tweet_id) %>%
  summarise(sentimiento_promedio = sum(Puntuacion)) %>%
  group_by(autor) %>%
  summarise(positivos = 100 * sum(sentimiento_promedio > 0) / n(),
            neutros = 100 * sum(sentimiento_promedio == 0) / n(),
            negativos = 100 * sum(sentimiento_promedio  < 0) / n())

#Diagrama de barras de sentimiento
windows()
par(mfrow=c(1,1))
tweets_sent %>% group_by(autor, tweet_id) %>%
  summarise(sentimiento_promedio = sum(Puntuacion)) %>%
  group_by(autor) %>%
  summarise(positivos = 100*sum(sentimiento_promedio > 0) / n(),
            neutros = 100*sum(sentimiento_promedio == 0) / n(),
            negativos = 100*sum(sentimiento_promedio  < 0) / n()) %>%
  ungroup() %>%
  gather(key = "sentimiento", value = "valor", -autor) %>%
  ggplot(aes(x = autor, y = valor, fill = sentimiento)) + 
  geom_col(position = "dodge", color = "black") + coord_flip() +
  labs(x="Candidato", y="Porcentaje") +
  theme_bw()

#Evolucio de sentimiento a traves de una función del tiempo

library(lubridate)
tweets_sent %>% mutate(anyo = year(fecha),
                       mes = month(fecha),
                       anyo_mes = ymd(paste(anyo, mes, sep="-"),truncated=2)) %>%
  group_by(autor, anyo_mes) %>%
  summarise(sentimiento = mean(Puntuacion)) %>%
  ungroup() %>%
  ggplot(aes(x = anyo_mes, y = sentimiento, color = autor)) +
  geom_point() + 
  geom_smooth() + 
  labs(x = "fecha de publicación") +
  facet_wrap(~ autor, ncol = 1) +
  theme_bw() +
  theme(legend.position = "none")
