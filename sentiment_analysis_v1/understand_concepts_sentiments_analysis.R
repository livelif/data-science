setwd("/home/paulob/Documentos/dataScience/projetos/projetos/analise_de_sentimento")
library(readr)
library(tidytext)
library(dplyr)

library(tidyverse)
library(tidytext)
library(text2vec)
library(glmnet)
library(DT)

train <- read_csv("/home/paulob/Documentos/dataScience/projetos/projetos/analise_de_sentimento/train.csv")
test <- read_csv("/home/paulob/Documentos/dataScience/projetos/projetos/analise_de_sentimento/test.csv")

View(train)

normalizadorDeTextosParaOTwitter <- function(x) {
  x$tweet <- gsub("#([a-z|A-Z|0-9|_])*","", x$tweet) # remove hashtags
  x$tweet <- gsub('@([a-z|A-Z|0-9|_])*', '', x$tweet) # remove palavras com @ (menções)
  x$tweet <- gsub('https://','', x$tweet) # removes https://
  x$tweet <- gsub('http://','', x$tweet) # removes http://
  x$tweet <- gsub('[^[:graph:]]', ' ', x$tweet) # removes graphic characters like emoticons 
  x$tweet <- gsub('[[:punct:]]', '', x$tweet) # removes punctuation 
  x$tweet <-gsub('[[:cntrl:]]', '', x$tweet) # removes control characters
  x$tweet <- gsub("\\w*[0-9]+\\w*\\s*", "", x$tweet) # removes numbers
  x$tweet <- iconv(x$tweet, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  x$tweet <- tolower(x$tweet) # caixa baixa
}

train$tweet <- normalizadorDeTextosParaOTwitter(train)
test$tweet <- normalizadorDeTextosParaOTwitter(test)

train$id <- c(1:nrow(train))
test$id <- c(1:nrow(test))

View(train)
View(test)

words <- train %>% unnest_tokens(word, tweet)

nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

nrc_joy

words %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

View(words)
words_sentiments <- words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, quantidade = id, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

View(words_sentiments)

?count

ggplot(words_sentiments, aes(x=quantidade)) + 
  geom_histogram(binwidth=1)

