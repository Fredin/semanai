consumer_key <- "6sl2fnRjFkBIKJAgcLBynkYCY"
consumer_secret <- "uQYQIuXzSUR9AlJ1VDkOpL53X5NgT3kYaLIBzaTqF7sz6rQ9y4"
access_token <- "97104418-tthj38FmkNtbOs2O3M6w8M7lTw9UOXhlW6CZmGLWC"
access_secret <- "TWyrHcAuRvX0oCYjhcxbJ0VHd77c6YoPncWK74qEvckgk"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

library(twitteR)
library(stringr)
library(ggplot2)


NCD <- searchTwitter("#NationalCoffeeDay", n = 2000)

df <- do.call("rbind", lapply(NCD, as.data.frame))
counts = table(df$screenName)
barplot(counts)

cc = subset(counts, counts>1)
barplot(cc, las=2, cex.names = 0.3)

df$text = sapply(df$text, function(row) iconv(row, to="UTF-8"))

trim <- function(x) sub("@", "", x)

df$to=sapply(df$text, function(tweet) str_extract(tweet, "^(@[[:alnum:]_]*)"))

df$to=sapply(df$text, function(tweet) trim(str_match(tweet, "^RT (@[[:alnum:]_]*)")[2]))

ggplot() + geom_bar(aes(x=na.omit(df$rt))) + theme(axis.text.x=element_text(angle = -90, size = 6))