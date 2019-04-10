
# ACTUALIZO r
install.packages("installr")

# Actualiza R
library(installr)
updateR()
# Instala / carga el paquete
if(!require(installr)) {install.packages("installr"); require(installr)} 
# Instala R, mueve y actualiza paquetes
updateR(F, T, T, F, T, F, T)

############################
# PAQUETES PARA SCRAP DE TWEETER
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

## autorizarion ingreso de tweeter ((no esta funcionando))
oauth_endpoints(authorize ="https://api.twitter.com/oauth",access= "https://api.twitter.com/oauth/access_token")
oauth_endpoints(authorize)

# guardo mis Users 
api_key <- "XcfrnRxi6aNEl4ObCsDJDPGCI"
api_secret <- "1YqdR8z6PQeVWjmlFyySRLsS4qXUvWJdEIjgoykqBSldIJimjM"
acces_token <- "2385418470-xTIz1382sHMnzRcdB56p8oS4AFF8HeA5IARzdtj"
acces_token_secret <- "lwhBGqm62Ta40cUK3ncaItypc8smCjlwS4R1TgHpSJHI9"

# me logue0 y seteo mi Usiario
setup_twitter_oauth(api_key,api_secret,acces_token,acces_token_secret)

# SCRAPEO palabra "trum"
some_tweets <- searchTwitter("trump", n = 10, since= "2016-01-01", lang="en")
View(some_tweets)
lenght.some_tweets <- length(some_tweets)
lenght.some_tweets

#Puedo exportar la base de datos 
some_tweets.df <- ldply(some_tweets,function(t) t$toDataFrame())
write.csv(some_tweets.df, "tweets.csv")

# get the text
some_txt <- sapply(some_tweets, function(x) x$getText())
# Cleaning 1- remove people name, RT text etc.
some_txt1 <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",some_txt)
# Cleaning 2- remove html link
some_txt2 = gsub("http[^[:blank:]]+","",some_txt1)
# Cleaning 3 - remove people names
some_txt3 <- gsub("@\\w+","", some_txt2)
# Cleaning 4 remove puntuation
some_txt4 <- gsub("[[:punct:]]"," ",some_txt3)
# Cleaning 5 remove puntuation
some_txt5 <- gsub("[^[:alnum:]]"," ",some_txt4)
# Exporting Excel
write.csv(some_txt5, "tweets1.csv")

# Cleaning wordcorps and claning
some_txt6 <- Corpus(VectorSource(some_txt5))
some_txt6 <- tm_map(some_txt6, removePunctuation)
some_txt6 <- tm_map(some_txt6, content_transformer(tolower))
some_txt6 <- tm_map(some_txt6, removeWords, stopwords("english"))
some_txt6 <- tm_map(some_txt6, stripWhitespace)

# Building Wordclud
pal <- brewer.pal(8, "Dark2")
wordcloud(some_txt6, min.freq = 5, max.words = Inf, width=1000, height=1000, random.order = F, color=pal)

# Sentiment Analysis
mysentiment <- get_nrc_sentiment(some_txt5)
SentimentScore <- data.frame(colSums(mysentiment[,]))
# puedo ver en una palabra los disntitos sentimientos
SentimentScore

names(SentimentScore) <- "Score"
SentimentScore <- cbind("sentiment" = row.names(SentimentScore),SentimentScore)

rownames(SentimentScore) <- NULL

### antes de correr // ordeno el data frame
names(SentimentScore) <- c("repetido", "sentiment", "Score")
SentimentScore$repetido <- NULL

SentimentScore

## por algún motivo NO lo está tomando al gráfico //
ggplot(data = SentimentScore, aes(x=sentiment, y=Score)) +
 geom_bar(aes(fill = mysentiment), stat="identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab ("Score") + ggtitle("Total sentiment Score Based on Tweets")


 