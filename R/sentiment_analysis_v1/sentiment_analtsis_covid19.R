setwd("/home/paulob/Documentos/dataScience/projetos/projetos/analise_de_sentimento")

library(twitteR)
library(ROAuth)
library(tm)
library(syuzhet)

app_name = ""

consumer_key <- ""

consumer_secret <- ""

access_token <- ""

access_secret <- ""

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tweetsCovid <- searchTwitter("covid19", n=500, lang="en")

tweetsCovidDf <- twListToDF(tweetsCovid)

View(tweetsCovidDf)

covidText <- tweetsCovidDf$text

covidText <- tolower(covidText)

covidText <- gsub("rt", "", covidText)

covidText <- gsub("@\\w+", "", covidText)

covidText <- gsub("[[:punct:]]", "", covidText)

covidText <- gsub("http\\w+", "", covidText)

covidText <- gsub("[ |\t]{2,}", "", covidText)

covidText <- gsub("^ ", "", covidText)

covidText <- gsub(" $", "", covidText)

corp1 <- Corpus(VectorSource(covidText))

covidText.text.corpus <- tm_map(corp1, function(x)removeWords(x,stopwords()))


library(wordcloud)

wordcloud(covidText.text.corpus, min.freq = 10, colors = brewer.pal(8, "Dark2"), 
          random.color = TRUE, max.words = 1000)

mysentiment_covid <- get_nrc_sentiment((covidText))

sentimenrtscoresCovid <- data.frame(colSums(mysentiment_covid[,]))

names(sentimenrtscoresCovid) <- "Score"
sentimenrtscoresCovid <- cbind("sentiment" = rownames(sentimenrtscoresCovid), sentimenrtscoresCovid)
rownames(sentimenrtscoresCovid) <- NULL

View(sentimenrtscoresCovid)

library(ggplot2)

ggplot(data=sentimenrtscoresCovid, aes(x=sentiment, y=Score)) + geom_bar(aes(fill=sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("sentiments") + ylab("score") + ggtitle("Sentiments")