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
mySearch <- searchTwitter("@realDonaldTrump", n = 100)

# Get rid of non-utf-8 characters
rawTweets <- twListToDF(mySearch)$text
convTweets <- iconv(rawTweets, to = "utf-8")
tweets <- (convTweets[!is.na(convTweets)])

# create a corpus
corpus <- Corpus(VectorSource(tweets),
                      readerControl = list(language = "english"))

# remove URLs
corpus <- tm_map(corpus,
                      content_transformer(function(x) gsub("http.*",".",x,ignore.case=TRUE)))

# create document term matrix applying some transformations
tdm <- TermDocumentMatrix(corpus,
                         control = list(language = "english", removePunctuation = TRUE,
                                        stopwords = c(stopwords("english")),
                                        removeNumbers = TRUE, tolower = TRUE))
# define tdm as matrix
m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE)
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

# plot wordcloud
wordcloud(dm$word[1:100], dm$freq[1:100], random.order=FALSE, colors=brewer.pal(8, "Dark2"))

# save the image in png format
png("felipeCloud.png", width=12, height=8, units="in", res=300)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()
