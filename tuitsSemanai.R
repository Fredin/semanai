library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)

campusLocs <- read.csv("campus_locs.csv")

campusLocs$geocode <- paste(campusLocs$lat,campusLocs$lon,"20km",sep=",")

tuits <- as.data.frame(NULL)

for (i in seq_along(campusLocs[,])){
  tuitSearch <- searchTwitter("#semanai", n = 500, since = "2016-09-26",
                              geocode = campusLocs$geocode[i])
  tuitsCampus <- twListToDF(tuitSearch)
  tuitsCampus$campus <- campusLocs$campus[i]
  tuits <- rbind(tuits, tuitsCampus)

}

head(tuits,3)
tail(tuits,3)


tuits <- twListToDF(tuitSearch)$text

