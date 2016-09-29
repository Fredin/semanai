library(twitteR)
library(ggplot2)
library(syuzhet)
library(dplyr)

consumer_key <- "6sl2fnRjFkBIKJAgcLBynkYCY"
consumer_secret <- "uQYQIuXzSUR9AlJ1VDkOpL53X5NgT3kYaLIBzaTqF7sz6rQ9y4"
access_token <- "97104418-tthj38FmkNtbOs2O3M6w8M7lTw9UOXhlW6CZmGLWC"
access_secret <- "TWyrHcAuRvX0oCYjhcxbJ0VHd77c6YoPncWK74qEvckgk"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

searchTwitteR("iphone")
searchTwitter("iphone")

tweets_BLM <- searchTwitter("BlackLivesMatter", n = 200)
BLM <- twListToDF(tweets_BLM)

sentence <- iconv(enc2utf8(BLM$text), sub = "byte")

sentence <- gsub("[[:punct:]]", "", sentence)

sentence <- tolower(sentence)

BLM$text <- sentence

#NRC Sentiment
BLM$text <- as.character(BLM$text)
hms <- get_nrc_sentiment(BLM$text)
hms$negative <- hms$negative*-1
hms$score <- hms$negative+hms$positive
BLM$score <- (cbind(hms$score))
BLM$tweet <- ifelse(BLM$score > 0, "positive", 
                    ifelse(BLM$score < 0, "negative", "neutral"))

#Afinn Sentiment
BLM$text <- as.character(BLM$text)
hms <- get_sentiment(BLM$text, method = c("afinn"))
hms <- t(hms)
hms <- t(hms)
BLM$afinn <- hms
BLM$tweeta <- ifelse(BLM$afinn > 0, "positive", 
                     ifelse(BLM$afinn < 0, "negative", "neutral"))

#Bing Sentiment
BLM$text <- as.character(BLM$text)
hms <- get_sentiment(BLM$text, method = c("bing"))
hms <- t(hms)
hms <- t(hms)
BLM$bing <- hms
BLM$tweetb <- ifelse(BLM$bing > 0, "positive", 
                    ifelse(BLM$bing < 0, "negative", "neutral") )

multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  library(grid)
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if(is.null(layout)) {
   layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if(numPlots == 1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for(i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, 
                                      layout.pos.col = matchidx$col))
    }
  }
}

p1 <- ggplot(BLM, aes(x = text, y = score, fill = tweet))+
  geom_bar(stat = "identity", position = "identity")+
  theme(axis.ticks = element_blank(), axis.text.y = element_blank(), legend.position = "none",
        axis.title.y = element_blank())+
  labs(list(x = "Tweet", y = "NRC Sentiment"))+ coord_flip()

p2 <- ggplot(BLM, aes(x = text, y = afinn, fill = tweeta))+
  geom_bar(stat = "identity", position = "identity")+
  theme(axis.ticks = element_blank(), axis.text.y = element_blank(), legend.position = "none",
        axis.title.y = element_blank())+
  labs(list(x = "Tweet", y = "Afinn Sentiment"))+ coord_flip()

p3 <- ggplot(BLM, aes(x = text, y = bing, fill = tweetb))+
  geom_bar(stat = "identity", position = "identity")+
  theme(axis.ticks = element_blank(), axis.text.y = element_blank(), legend.position = "none",
        axis.title.y = element_blank())+
  labs(list(x = "Tweet", y = "Bing Sentiment"))+ coord_flip()


multiplot(p1,p2,p3, cols = 3)

