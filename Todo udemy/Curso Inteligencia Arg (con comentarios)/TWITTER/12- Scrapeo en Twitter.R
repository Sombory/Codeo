
# TWITTER << analisis del sentimiento>>
install.packages(c("twitteR", "RColorBrewer","plyr","ggplot2","devtools","httr"))
require(devtools) # ojo q no me instaló
install.packages(c("twitteR", "RColorBrewer","plyr","ggplot2","devtools","httr"))
require(devtools) # ojo q no me instaló
install_url("https://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz")
install_url("https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz")
install_url("https://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
install.packages("https://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz",repos = NULL, type="source",dependencies = T)
install.packages("tm", dependencies = T)
install.packages("ftp://cran.r-project.org/pub/R/scr/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz", repos = NULL)
install.packages('tm')
install.packages('SnowballC')
install.packages("wordcloud")
install.packages("package:NLP")
install.packages("NLP")
install.packages("RColorBrewer")
install.packages("Rstem")
install.packages("tidyselect")
library(devtools)
library(tidyselect)
library(RColorBrewer)
library(NLP)
library(tm)
library(SnowballC)
library(wordcloud)
library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(slam)
library(ROAuth) # no me instala 
library(stringr)
library(ggplot2)
library(httr)
library(wordcloud)
library(sentiment)
library(RCurl) # no me instala 
library(syuzhet)

# guardo mis users 
api_key <- "XcfrnRxi6aNEl4ObCsDJDPGCI"
api_secret <- "1YqdR8z6PQeVWjmlFyySRLsS4qXUvWJdEIjgoykqBSldIJimjM"
acces_token <- "2385418470-xTIz1382sHMnzRcdB56p8oS4AFF8HeA5IARzdtj"
acces_token_secret <- "lwhBGqm62Ta40cUK3ncaItypc8smCjlwS4R1TgHpSJHI9"

# me logue y seteo mi usiario
setup_twitter_oauth(api_key,api_secret,acces_token,acces_token_secret)

#scrapero una palabra ej: "machinelarning"
tweets <- searchTwitter("rosario", n = 1500, lang="en")
View(tweets)

# Me quedo con los textos
texts <- sapply(tweets,function(x)x$getText())
head(texts)

# LIMPIO La BASES
clean.data <- function(text){
  #eliminar re-tweets y @  del texto original (w espacio libre) // Es una expresion regular > RT|VIA)((?:\\b\\W*@\\w+)+)
  text = gsub("(RT|VIA)((?:\\b\\W*@\\w+)+)","",text)
  text = gsub("@\\w+","",text)
  #eliminar signos de puntuación y digitos 0 a 9
  text= gsub("[[:punct:]]","",text)
  text= gsub("[[:digit:]]","",text)
  #eliminar link de html, tabulaciones, y espacio adicionales
  text = gsub("http\\w+","",text)
  text = gsub("[\t]{2,}","",text)
  text = gsub("^\\s+|\\s+$","",text)
}

## cualquier duda con lo anterior >> ver expresiones REGULARES
texts <- clean.data(texts)
head(texts)

# vamos a gestionar los NA, 
# primero pasamos todo a minúscula // utilizo tryCath para gestionar errores
# ademas tranforma a NA todo lo que no puede ser corrido
handle.error <- function(x){
  #Crea el valor omitido
  y = NA
  #Try_catch error
  try_error <- tryCatch(tolower(x), error=function(e) e)
  # si hay error
  if(!inherits(try_error,"error"))
    y = tolower(x)
  # devolvemos el resultado
  return(y)
}


# Finalmente, elimino los errores que habia convertido en NA
texts = sapply(texts,handle.error)
head(texts)

texts <- texts[!is.na(texts)]
names(texts) <- NULL
View(texts)

# ANALIZAMOS DE SENTIMIENTO
install_url("https://cran.r-project.org/src/contrib/Archive/RSentiment/RSentiment_2.2.tar.gz")
library(RSentiment)
#############

# primer usamos esta funcion (del paquete sentiment). Clasifica en distintas categorias
# alegría (joy), tristeza (sad), etc.
class_emo <- classify_emotion(texts,algorithm="bayes", prior=1)

# analizo que tipo de emoción tiene mas presencia
head(class_emo)

# veo en la 7ma columna (esta columna es odnd es donde dice "surproce, joy, sad etc)
emotion <- class_emo[,7]
emotion[is.na(emotion)] <- "unknown"
head(emotion)

# analisis de la "POLARIDAD" (ver si 1 tweet es "positivo", "negativo" o "neutro")
class_pol <- classify_polarity(texts, algorith="bayes")
head(class_pol)

# Extraemos la 4ta columna donde nos dice: "positive", "negative", "etc"
polarity <- class_pol[,4]
sent_df <- data.frame(text=texts,
                      emotion=emotion, polarity=polarity, stringsAsFactors = F)

View(sent_df)

# Ordenmo los factores de la columna Emoción de forma decreciente
sent_df <- within(sent_df,emotion <- factor(emotion,levels = names(sort(table(emotion),decreasing=T))))


library(RColorBrewer)
library(ggplot2)


ggplot(sent_df, aes(x=emotion))+ 
  geom_bar(aes(y= ..count.., fill=emotion))+
  scale_fill_brewer(palette = "Set2")+ 
  labs(x="Categorías de emosion", y ="Número de Tweets")+
  labs(title = "Analisis de Sentimiento acerca de Machine_Learning")
# el grafico anterior tiene mucho Unknow

ggplot(sent_df, aes(x=polarity))+
  geom_bar(aes(y= ..count.., fill=polarity))+
  scale_fill_brewer(palette = "Dark2")+
  labs(x="Categorias de polaridad", y="Numero de Tweets")+
  labs(title="Análisis de Sentimiento acerca de ML")

?RColorBrewer
