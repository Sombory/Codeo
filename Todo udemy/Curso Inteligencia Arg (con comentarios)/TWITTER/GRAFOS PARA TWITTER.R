
#Analisis de redes con Twitter
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

library(twitteR)
library(igraph)
library(dplyr)

### CAMBIAR LAS api

# api_key <- "Km07yXQyohsRRx6vr4DbKYF5C"
# api_secret <- "RDMCCXdwdmDZL0zI2Polj4KGG8Mc86B0IjfRNjfjShQJtmle6P"
# access_token <- "207177829-zzm2C1cG1oHxE77xgqGOpAay3PMBYhTpchEMtXBC"
# access_token_secret <- "Xh5GIQkQbzJnIXoHezfrtxrSdSHtR4KMhL6xX8ggYS0ub"

# Usa mis APIs
api_key <- "XcfrnRxi6aNEl4ObCsDJDPGCI"
api_secret <- "1YqdR8z6PQeVWjmlFyySRLsS4qXUvWJdEIjgoykqBSldIJimjM"
acces_token <- "2385418470-9G6yE89sE5W54WFZM189NMV4hWubhfNNOtcpxpM"
acces_token_secret <- "VVOhPgYcreilxHJqSPS7lHQnpR3UPwjMgXQW3Rfh2Wvy1"

setup_twitter_oauth(api_key, api_secret, acces_token, acces_token_secret)
# bajo un  tag  y aclaro el volúmen
# Este tipo de informes sirven para ver la ACTUALIDAD, el scrip corre desde ahora para atrás
all_tweets <- searchTwitter("Rosario", n = 1000)
# Armo la lista data.frame
all_tweets <- twListToDF(all_tweets)
View(all_tweets)
# puedo armar UNA MUESTRA: 
# sample_tweets <- all_tweets[1:200,]
# o también
sample_tweets <- all_tweets

# En la muestra "sample_tweets", separo la columna si son (true) o no (false) rt-tweets
# Con Split DIVIDO LOS TWEETS
split_tweets <- split(sample_tweets, sample_tweets$isRetweet)
# Como se observa me queda una lista de 2 elementos donde hay y no RT
View(split_tweets)

# solo busco lo retweets > todos los RT se deben extraer desde la 5ta palabra
# substr (sub-streem, busco la columna text), le digo a la función donde tiene q cortar  desde el caracter 5":" (porq todos los RT terminan en :)
# es decir busco el usaurio a partir del caracter 5 hasta los ":" y armo una nueva col
retweets <- mutate(split_tweets[['TRUE']],
                   sender = substr(text, 5, regexpr(":",text)-1))
# podemos ver los usuario que RT con esa columna (se llama sender)
View(retweets)

# hacemos una lista de ¿Quién a RT a quién? -->lista de aristas<--
# "sender" es el emisor quien genera el tweet
# "reciever" es quien retweetea 
edge_list <- as.data.frame(cbind(sender = tolower(retweets$sender),
                                 receiver = tolower(retweets$screenName)))

edge_list <- count(edge_list, sender, receiver)
#ya tenemos la lista de quien twittea y quien RT ese tweet
View(edge_list)

##################################################
# PODEMOS EXPORTAR ESTO A .CSV para hacer una corrida a Gephi
# PODEMOS ANALIZAR LOS COMUNIDADES
write.csv(edge_list,"nodes.csv",row.names = FALSE)
################################################

# Ahora creamos una Network
tweets_graph <- graph_from_data_frame(d=edge_list, directed = T)
save(tweets_graph, file="C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo/Todo udemy/Curso Inteligencia Arg (con comentarios)/TWITTER/tweets-from-rosario.Rdata")


plot(tweets_graph, 
     layout=layout.fruchterman.reingold,
     vertex.color = "blue",
     vertex.size = degree(tweets_graph, mode = "in"),
     vertex.label = NA,
     edge.arrow.size = 0.5,
     edge.arrow.width = 0.5,
     edge.width = edge_attr(tweets_graph)$n,
     edge.color=hsv(h=.6, s=.7, v=.3, alpha = 0.8),
     main="Tweets sobre Rosario")

# este algoritmo mejora la calidad de los RT
# estra un subconunto de datos y lo extiende al resto de los datos
library(devtools)
devtools::install_github("analyxcompany/ForceAtlas2")
library(ForceAtlas2)

force_layout <- layout.forceatlas2(tweets_graph,
                                   iterations = 300,
                                   plotstep = 40)

plot(tweets_graph, 
     layout = force_layout,
     vertex.color = "blue",
     vertex.size = degree(tweets_graph, mode = "in"),
     vertex.label = NA,
     edge.arrow.size = 0.5,
     edge.arrow.width = 0.5,
     edge.width = edge_attr(tweets_graph)$n,
     edge.color=hsv(h=.9, s=1, v=.7, alpha = 0.5),
     main="Tweets sobre Rosario")

### nuevamente pasa usar en Gephi
write.graph(tweets_graph,
            file="C:/Users/ezequiel.eliano/Desktop/Codeo R/Codeo/Todo udemy/Curso Inteligencia Arg (con comentarios)/TWITTER/tweets-from-rosario.graphml",
            format= "graphml")