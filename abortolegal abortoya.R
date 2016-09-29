library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)

API_key <- "6mXhbmHVGOEIgIG4dgU6VCzKm"
API_secret <- "MfybR9LBd6bhJ7NC1zcWSLSFuJ7SdARVhycPFKcQrsmbeojS4R"
Access_token <- "55032065-1aoUKdCFXGuEmDk7SdY41iYUgj6HNrx9LWSL9Z7fr"
Access_token_secret <- "ZmtXhXmhKQ6ZxTFjWZftLym1UKoFs5dnxit7OyhrZddmT"

setup_twitter_oauth(API_key, API_secret, Access_token, Access_token_secret)
hashtags <- c("#abortolibre", "#AbortoYa")
needle <- paste(hashtags, collapse = " OR ")
tweets <- searchTwitter(needle, n = 5000)
df <- twListToDF(tweets)$text

corpus <- Corpus(VectorSource(df),
                 readerControl = list(language = "spanish"))

corpus <- tm_map(corpus,content_transformer(
  function(x) iconv(x,"utf-8","latin1", sub = "")))
corpus <- tm_map(corpus,content_transformer(
  function(x) gsub("http\\S*","", x,ignore.case=TRUE)))
corpus <- tm_map(corpus,content_transformer(
  function(x) gsub("@\\S*","", x,ignore.case=TRUE)))
corpus <- tm_map(corpus,content_transformer(
  function(x) gsub("#\\S*","", x ,ignore.case=TRUE)))

tdm <- TermDocumentMatrix(corpus,
                          control = list(language = "spanish",
                                         removePunctuation = TRUE,
                                         stopwords = c(stopwords("spanish"),
                                                       "abortolibre", "aborto", "abortoya"),
                                         removeNumbers = TRUE,
                                         tolower = TRUE))
m = as.matrix(tdm)
word_freqs = sort(rowSums(m), decreasing=TRUE)
dm = data.frame(word=names(word_freqs), freq=word_freqs)


png("wordCloud.png", width=12, height=8, units="in", res=300)

wordcloud(dm$word, dm$freq,
          min.freq = 10, max.words = 400,
          random.order=FALSE, colors=brewer.pal(7, "Set1"))

dev.off()
word_freqs

