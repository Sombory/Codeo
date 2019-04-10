
# TWITTER << analisis del sentimiento>>

install.packages(c("twitteR", "RColorBrewer","plyr","ggplot2","devtools","httr"))

require(devtools) # ojo q no me instaló

install_url("https://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz")
install_url("https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz")
install_url("https://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")


library(slam)
library(sentiment)
library(twitteR)

# guardo mis users 
api_key <- "XcfrnRxi6aNEl4ObCsDJDPGCI"
api_secret <- "1YqdR8z6PQeVWjmlFyySRLsS4qXUvWJdEIjgoykqBSldIJimjM"
acces_token <- "2385418470-xTIz1382sHMnzRcdB56p8oS4AFF8HeA5IARzdtj"
acces_token_secret <- "lwhBGqm62Ta40cUK3ncaItypc8smCjlwS4R1TgHpSJHI9"

# me logue y seteo mi usiario
setup_twitter_oauth(api_key,api_secret,acces_token,acces_token_secret)

#scrapero una palabra ej: "machinelarning"
tweets <- searchTwitter("machinelarning", n = 1500, lang="en")
View(tweets)

texts <- sapply(tweets,function(x)x$getText())
clean.data <- function(text){
  #eliminar re-tweets y @  del texto original (w espacio libre)
  text = gsub("(RTIVIA)((?:\\b\\W*@\\w+)+)","",text)
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
  # crea el valor omitido
  y = NA
  #try_catch error
  try_error <- tryCatch(tolower(x), error=function(e) e)
  # si hay error
  if(!inherits(try_error,"error"))
  # devolvemos el resultado
  return(y)
}

texts = sapply(texts,handle.error)
head(texts)

# finalmente, elimino los errores que habia convertido en NA

texts <- texts[!is.na(texts)]
names(texts) <- NULL

#finalizamos la limpieza
##############

# ANALIZAMOS LOS DATOS
#########################
install_url("https://cran.r-project.org/src/contrib/Archive/RSentiment/RSentiment_2.2.tar.gz")
library(RSentiment)
######################################################

# sigo con el curso a pesar de no poder ingresar a "library sentiment""¡¡
calculate_sentiment(texts)
texts


class_emo <- classify_emotion(texts,algorithm="bayes", prior=1.0)
# analizo que tipo de emoción tiene mas presencia
head <- (class_emo)

emotion <- class_emo[,7]
emotion <- [is.na(emotion)] <- "unknown"
head(emotion)

# analisis de la polaridad

class_pol <- classify_polaruty(text, algorith="bayes")
head(class_pol)

polarity <- class_pol[,4]


sent_df <- data.frame(text=texts,
                      emotion=emotion, polarity=polarity, stringsAsFactors = F)

# ordenmo los factores a la emoción

sent_df <- within(send_df,emotion <- factor(emotion,levels = names(table(emotion),decreasing=T)))

library(RColorBrewer)
library(ggplot2)


ggplot(sent_df, aes=(x=emotion))+geom_bar(aes(y=..count, fill=emotion))

scale_fill_brewer(palette = "Set2")+ 
  labs(x="Categorías de emosion", y ="Número de Tweets")+
  labs(title = "Analisis de Sentimiento acerca de ML")


ggplot(sent_df, aes=(x=polarity))
         geom_bar(aes(y=..count..,fill=polarity))+
         scale_fill_brewer(pallete="set3")+
         labs(x="Categorias de polaridad", y="Numero de Tweets")
         labs(title="Análisis de Sentimiento acerca de ML")
         







#####################################################################
install.packages("syuzhet")
library(syuzhet)
calculate_custom_sentiment(texts,positivewords = bueno, negativewords = malo)
head(class_emo)
install.packages("sentiment")
library(sentiment)
library(sentimentr)
install.packages("Rcpp")
install.packages("plyr")
install.packages("sentimentr")
classify_emotion?
############################################################################

