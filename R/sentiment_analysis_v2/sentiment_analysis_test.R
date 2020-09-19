# Mini-Projeto 1 - Análise de Sentimentos em Redes Sociais

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
setwd("/home/paulob/Documentos/dataScience/projetos/projetos/analise_de_sentimento_v2")
getwd()

## Etapa 1 - Pacotes e Autenticação

# Instalando e Carregando o Pacote twitteR
install.packages("twitteR")
install.packages("httr")
library(twitteR)
library(httr)

# Carregando a biblioteca com funções de limpeza
source('utils.R')

# Chaves de autenticação no Twitter
app_name = ""
consumer_key <- ""
consumer_secret <- ""
access_token <- ""
access_secret <- ""

# Autenticação. Responda 1 quando perguntado sobre utilizar direct connection.
logarNoTwitter(app_name, consumer_key, consumer_secret, access_token, access_secret)

## Etapa 2 - Captura dos tweets
tweets <- searchTwitter("Data science", n =100, lang = "pt")

head(tweets)

# Capturando os tweets

## Etapa 3 - Tratamento dos dados coletados através de text mining

# Instalando o pacote para Text Mining.
install.packages("tm")
install.packages("SnowballC")
library(SnowballC)
source("utils.R")
library(tm)
options(warn=-1)

# Tratamento (limpeza, organização e transformação) dos dados coletados
tweetList <- sapply(tweets, function(x)x$getText())
tweetList <- iconv(tweetList, to = "utf-8", sub = "")
tweetList <- limpaTweets(tweetList)
tweetCorpus <- Corpus(VectorSource(tweetList))
?tm_map
tweetCorpus <- tm_map(tweetCorpus, removePunctuation)
tweetCorpus <- tm_map(tweetCorpus, content_transformer(tolower))
tweetCorpus <- tm_map(tweetCorpus, function(x) removeWords(x, stopwords()))


termo_por_documento = as.matrix(TermDocumentMatrix(tweetCorpus), control = list(stopwords = c(stopwords("portuguese"))))


install.packages("RColorBrewer")
install.packages("wordcloud")
library(RColorBrewer)
library(wordcloud)


pal2 <- brewer.pal(8, "Dark2")
wordcloud(tweetCorpus, min.freq = 2, scale = c(5,1), random.color = F, max.words = 60, random.order = F, colors = pal2)


?TermDocumentMatrix
tweetDm <- TermDocumentMatrix(tweetCorpus)
tweetDm

findFreqTerms(tweetDm, lowfreq = 11)

findAssocs(tweetDm, 'datascience', 0.60)

tweetDm2 <- removeSparseTerms(tweetDm, sparse = 0.9)


tweetDm2Scale <- scale(tweetDm2)

tweetDist <- dist(tweetDm2Scale, method = "euclidean")


tweetfit <- hclust(tweetDist)

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


library(tidytext)
library(dplyr)
pos <- get_sentiments("bing") %>% filter(sentiment == "positive") %>% select(word)
neg <- get_sentiments("bing") %>% filter(sentiment == "negative") %>% select(word)


tweetsUsa <- searchTwitter("usa", n = 500, lang = "en")
tweetsCat <- searchTwitter("ca", n = 500, lang = "en")

pegarTextosDaDaListaDeTweets <- function(lista) {
  sapply(lista, function(x) x$getText())
}

canadaText <- pegarTextosDaDaListaDeTweets(tweetsCat)
usaText <-  pegarTextosDaDaListaDeTweets(tweetsUsa)


paisTweet = c(length(canadaText), length(usaText))


paises = c(canadaText, usaText)

scores = sentimento.score(paises, pos, neg, .progress = "text")
scores


scores$paises = factor(rep(c("ca", "usa"), paisTweet))
scores$muito.pos = as.numeric(scores$score >= 1)
scores$muito.neg = as.numeric(scores$score <= -1)

scores


numpos = sum(scores$muito.pos)
numneg = sum(scores$muito.neg)

install.packages("lattice")
library("lattice")


## Usando Classificador Naive Bayes para analise de sentimento
# https://cran.r-project.org/src/contrib/Archive/Rstem/
# https://cran.r-project.org/src/contrib/Archive/sentiment/

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

ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x = "Categorias de Sentimento", y = "Numero de Tweets")
