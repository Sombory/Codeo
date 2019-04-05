
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
  text= gsub("[[:digits:]]","",text)
  #eliminar link de html, tabulaciones, y espacio adicionales
  text = gsub("http\\w+","",text)
  text = gsub("[\t]{2,}","",text)
  text = gsub("^\\s+|\\s+$","",text)
  
  
  
  
  
}




