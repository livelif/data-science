setwd("/home/paulob/Documentos/dataScience/projetos/projetos/analise_de_sentimento_v2")
getwd()

install.packages("twitteR")
install.packages("httr")
library(twitteR)
library(httr)

source('utils.R')


app_name = ""
consumer_key <- ""
consumer_secret <- ""
access_token <- ""
access_secret <- ""

logarNoTwitter(app_name, consumer_key, consumer_secret, access_token, access_secret)


tema <- "BigData"
qtd_tweets <- 100
lingua <- "pt"
tweetdata = searchTwitter(tema, n = qtd_tweets, lang = lingua)

head(tweetdata)


install.packages("tm")
install.packages("SnowballC")
library(SnowballC)
library(tm)
options(warn=-1)


tweetlist <- sapply(tweetdata, function(x) x$getText())
tweetlist <- iconv(tweetlist, to = "utf-8", sub="")
tweetlist <- limpaTweets(tweetlist)
tweetcorpus <- Corpus(VectorSource(tweetlist))
tweetcorpus <- tm_map(tweetcorpus, removePunctuation)
tweetcorpus <- tm_map(tweetcorpus, content_transformer(tolower))
tweetcorpus <- tm_map(tweetcorpus, function(x)removeWords(x, stopwords()))


termo_por_documento = as.matrix(TermDocumentMatrix(tweetcorpus), control = list(stopwords = c(stopwords("portuguese"))))

head(termo_por_documento)

install.packages("RColorBrewer")
install.packages("wordcloud")
library(RColorBrewer)
library(wordcloud)


pal2 <- brewer.pal(8,"Dark2")

wordcloud(tweetcorpus, 
          min.freq = 2, 
          scale = c(5,1), 
          random.color = F, 
          max.word = 60, 
          random.order = F,
          colors = pal2)

tweettdm <- TermDocumentMatrix(tweetcorpus)
tweettdm

findFreqTerms(tweettdm, lowfreq = 11)

findAssocs(tweettdm, 'datascience', 0.60)

tweet2tdm <- removeSparseTerms(tweettdm, sparse = 0.9)


tweet2tdmscale <- scale(tweet2tdm)


tweetdist <- dist(tweet2tdmscale, method = "euclidean")

tweetfit <- hclust(tweetdist)


plot(tweetfit)


cutree(tweetfit, k = 4)

rect.hclust(tweetfit, k = 3, border = "red")




install.packages("stringr")
install.packages("plyr")
library(stringr)
library(plyr)

sentimento.score = function(sentences, pos.words, neg.words, .progress = 'none')
{
  
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   sentence = gsub("[[:punct:]]", "", sentence)
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   sentence =gsub('\\d+', '', sentence)
                   tryTolower = function(x)
                   {
                     y = NA
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     return(y)
                   }
                   
                   sentence = sapply(sentence, tryTolower)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress = .progress )
  
  scores.df = data.frame(text = sentences, score = scores)
  return(scores.df)
}

pos = get_sentiments("bing") %>% filter(sentiment == "positive") %>% select(word)
neg = get_sentiments("bing") %>% filter(sentiment == "negative") %>% select(word)

teste = c("Big Data is the future", "awesome experience",
          "analytics could not be bad", "learn to use big data")

testesentimento = sentimento.score(teste, pos, neg)
class(testesentimento)

testesentimento$score

catweets = searchTwitter("ca", n = 500, lang = "en")
usatweets = searchTwitter("usa", n = 500, lang = "en")

catxt = sapply(catweets, function(x) x$getText())
usatxt = sapply(usatweets, function(x) x$getText())

paisTweet = c(length(catxt), length(usatxt))

paises = c(catxt, usatxt)

scores = sentimento.score(paises, pos, neg, .progress = 'text')

scores$paises = factor(rep(c("ca", "usa"), paisTweet))
scores$muito.pos = as.numeric(scores$score >= 1)
scores$muito.neg = as.numeric(scores$score <= -1)


numpos = sum(scores$muito.pos)
numneg = sum(scores$muito.neg)


global_score = round( 100 * numpos / (numpos + numneg) )
head(scores)
boxplot(score ~ paises, data = scores)


install.packages("lattice")
library("lattice")
histogram(data = scores, ~score|paises, main = "Análise de Sentimentos", xlab = "", sub = "Score")



install.packages("Rstem_0.4-1.tar.gz", sep = "", repos = NULL, type = "source")
install.packages("sentiment_0.2.tar.gz",sep = "", repos = NULL, type = "source")
install.packages("ggplot2")
library(Rstem)
library(sentiment)
library(ggplot2)

tweetpt = searchTwitter("bigdata", n = 1500, lang = "pt")

tweetpt = sapply(tweetpt, function(x) x$getText())

tweetpt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweetpt)

tweetpt = gsub("@\\w+", "", tweetpt)

tweetpt = gsub("[[:punct:]]", "", tweetpt)

tweetpt = gsub("[[:digit:]]", "", tweetpt)

tweetpt = gsub("http\\w+", "", tweetpt)

tweetpt = gsub("[ \t]{2,}", "", tweetpt)
tweetpt = gsub("^\\s+|\\s+$", "", tweetpt)


try.error = function(x)
{
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}


tweetpt = sapply(tweetpt, try.error)


tweetpt = tweetpt[!is.na(tweetpt)]
names(tweetpt) = NULL

class_emo = classify_emotion(tweetpt, algorithm = "bayes", prior = 1.0)
emotion = class_emo[,7]

emotion[is.na(emotion)] = "Neutro"


class_pol = classify_polarity(tweetpt, algorithm = "bayes")
polarity = class_pol[,4]

sent_df = data.frame(text = tweetpt, emotion = emotion,
                     polarity = polarity, stringsAsFactors = FALSE)

sent_df = within(sent_df,
                 emotion <- factor(emotion, levels = names(sort(table(emotion), 
                                                                decreasing=TRUE))))



ggplot(sent_df, aes(x = emotion)) +
  geom_bar(aes(y = ..count.., fill = emotion)) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Categorias", y = "Numero de Tweets") 

# Polaridade
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x = "Categorias de Sentimento", y = "Numero de Tweets")











