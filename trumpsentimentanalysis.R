library(twitteR)
library(RCurl)
library(rjson)
library(stringr)
library(tm)
library(wordcloud)

consumer_key <-"mHs92DjgRepIFDQ0iEcGMRot8"
consumer_secret <- "Vl3RajxtGBR67lUmvWUM29Sj356a567z7xJ9oPElqE3OfxW7WF"
access_token <-"85979755-a1G2natu500JDOCUBlDvCFKU9nMQfpWMHSc6jiqne"
access_secret <-"sqtl93KznkSTplozyRUYr6OvtHsyh5SjFyWZ5QntBEn1R"
key <- "mHs92DjgRepIFDQ0iEcGMRot8"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tweets = searchTwitter("Trump", 500, lang="en")

tweet_txt = sapply(tweets, function(x) x$getText())


clean.text <- function(some_txt)
{
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  some_txt = gsub("@\\w+", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  some_txt = gsub("[[:digit:]]", "", some_txt)
  some_txt = gsub("http\\w+", "", some_txt)
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  some_txt = gsub("amp", "", some_txt)
  try.tolower = function(x)
  {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  
  some_txt = sapply(some_txt, try.tolower)
  some_txt = some_txt[some_txt != ""]
  names(some_txt) = NULL
  return(some_txt)
}

tweet_clean = clean.text(tweet_txt)
tweet_num = length(tweet_clean)


# clean text
tweet_clean = clean.text(tweet_txt)
tweet_num = length(tweet_clean)
# data frame (text, sentiment)
tweet_df = data.frame(text=tweet_clean, sentiment=rep("", tweet_num),stringsAsFactors=FALSE)

tweet_corpus = Corpus(VectorSource(tweet_clean))
tdm = TermDocumentMatrix(tweet_corpus, control = list(removePunctuation = TRUE,stopwords = c("the", "they", "them", "donald", "trump's", "trumps", stopwords("english")), removeNumbers = TRUE, tolower = TRUE))

Sentiment <- get_nrc_sentiment(tweet_clean)

tweetssentiment <- cbind(tweet_clean, Sentiment)

sentimentTotals <- data.frame(colSums(Sentiment[,c(2:10)]))
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiments" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL

ggplot(sentimentTotals, aes(x = sentiments, y = count)) +
  geom_bar(aes(fill = sentiments), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets"
)
