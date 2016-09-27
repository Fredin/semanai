### Esto es >95% código de Gastón Sanchez. El código original y mucho más lo
### pueden encontrar en https://sites.google.com/site/miningtwitter/ Un súper
### sitio si apenas estan empezando a minar y visualizar texto con R.

library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)

# Primero hay que logearse a la API de twitter. Por motivos de seguridad omito
# esa parte.

# get the twits
mySearch <- searchTwitter("paz", n = 500)


# Get rid of non-utf-8 characters
rawTweets <- twListToDF(mySearch)$text
convTweets <- iconv(rawTweets, to = "utf-8")
tweets <- (convTweets[!is.na(convTweets)])

# create a corpus
corpus <- Corpus(VectorSource(tweets),
                      readerControl = list(language = "spanish"))

# remove URLs
corpus <- tm_map(corpus,content_transformer(function(x) gsub("\bhttp.*\b","",x,ignore.case=TRUE, perl = TRUE)))
corpus <- tm_map(corpus,content_transformer(function(x) gsub("\b@.*\b","",x,ignore.case=TRUE, perl = TRUE)))


# create document term matrix applying some transformations
tdm <- TermDocumentMatrix(corpus,
                         control = list(language = "spanish", removePunctuation = TRUE,
                                        stopwords = stopwords("spanish"),
                                        removeNumbers = TRUE, tolower = TRUE))
# define tdm as matrix
m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE)
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

# plot wordcloud
wordcloud(dm$word[2:500], dm$freq[2:300], random.order=FALSE, colors=brewer.pal(8, "Dark2"))

# save the image in png format
png("wordCloud.png", width=12, height=8, units="in", res=300)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()