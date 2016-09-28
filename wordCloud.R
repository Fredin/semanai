### Esto es en gran parte código de Gastón Sanchez. El código original y mucho más lo
### pueden encontrar en https://sites.google.com/site/miningtwitter/ Un súper
### sitio si apenas estan empezando a minar y visualizar texto con R.

library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)

# Primero hay que logearse a la API de twitter. Ver twitter_Auth.R

# get the twits
mySearch <- searchTwitter("#semanai", n = 500, since = "2016-09-26")
tweets <- twListToDF(mySearch)$text

# create a corpus
corpus <- Corpus(VectorSource(tweets),
                      readerControl = list(language = "spanish"))

# remove non-utf-8 characters, URLs, usernames and hashtags
corpus <- tm_map(corpus,content_transformer(
  function(x) iconv(x,"utf-8","latin1", sub = "")))
corpus <- tm_map(corpus,content_transformer(
  function(x) gsub("http\\S*","", x,ignore.case=TRUE)))
corpus <- tm_map(corpus,content_transformer(
  function(x) gsub("@\\S*","", x,ignore.case=TRUE)))
corpus <- tm_map(corpus,content_transformer(
  function(x) gsub("#\\S*","", x ,ignore.case=TRUE)))

# create document term matrix applying some transformations
tdm <- TermDocumentMatrix(corpus,
                         control = list(language = "spanish",
                                        removePunctuation = TRUE,
                                        stopwords = c(stopwords("spanish"),
                                                      "semanai"),
                                        removeNumbers = TRUE,
                                        tolower = TRUE))
# define tdm as matrix
m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE)
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

# plot wordcloud and save in png format

png("wordCloud.png", width=12, height=8, units="in", res=300)

wordcloud(dm$word, dm$freq,
          min.freq = 5, max.words = 200,
          random.order=FALSE, colors=brewer.pal(8, "Dark2"))

dev.off()