setwd("/home/paulob/Documentos/dataScience/projetos/projetos/analise_de_sentimento")

library(tidyverse)
library(tidytext)
library(text2vec)
library(glmnet)
library(DT)


app_name = ""

consumer_key <- ""

consumer_secret <- ""

access_token <- ""

access_secret <- ""

?create_token
create_token(app = app_name, consumer_key = consumer_key, consumer_secret = consumer_secret,
             access_token = access_token, access_secret = access_secret)

tweetsBrasilTreino <- search_tweets("#Brasil", n = 100, include_rts = FALSE)
tweetsBrasilTest <- search_tweets("#Brasil", n = 30, include_rts = FALSE)

View(tweetsBrasilTreino)

normalizarTextoDosTweets <- function(tweets) {
  tweets$text <- gsub("#([a-z|A-Z|0-9|_])*","", tweets$text) # remove hashtags
  tweets$text <- gsub('@([a-z|A-Z|0-9|_])*', '', tweets$text) # remove palavras com @ (menções)
  tweets$text <- gsub('https://','', tweets$text) # removes https://
  tweets$text <- gsub('http://','', tweets$text) # removes http://
  tweets$text <- gsub('[^[:graph:]]', ' ', tweets$text) # removes graphic characters like emoticons 
  tweets$text <- gsub('[[:punct:]]', '', tweets$text) # removes punctuation 
  tweets$text <- gsub('[[:cntrl:]]', '', tweets$text) # removes control characters
  tweets$text <- gsub("\\w*[0-9]+\\w*\\s*", "", tweets$text) # removes numbers
  #tweets$text  <- iconv(tweets$text, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  tweets$text <- tolower(tweets$text) 
}

tweetsBrasilTreino$text <- normalizarTextoDosTweets(tweetsBrasilTreino)

df = as.data.frame(trimws(tweetsBrasilTreino$text, which = c("left")))

colnames(df)[1] <- "text"

df = df %>%
  # recode empty strings "" by NAs
  na_if("") %>%
  # remove NAs
  na.omit

View(df)

tok_fun <- word_tokenizer

tweetsBrasilTreino$id <- c(1:nrow(tweetsBrasilTreino))

df$id <- seq.int(nrow(df))

df$text = as.character(df$text)

str(df)

it_train <- itoken(df$text, 
                   tokenizer = tok_fun, 
                   ids = df$id, 
                   progressbar = FALSE)
stop_words <- read_csv("/home/paulob/Documentos/dataScience/projetos/projetos/analise_de_sentimento/stopwords.txt")

?colnames

colnames(stop_words) <- c('word')

str(stop_words)

View(stop_words)

?create_vocabulary

vocab <- create_vocabulary(it_train, stopwords = stop_words$word)

pruned_vocab <- prune_vocabulary(vocab, 
                                 term_count_min = 10, 
                                 doc_proportion_max = 0.5,
                                 doc_proportion_min = 0.001)

vectorizer <- vocab_vectorizer(pruned_vocab)
dtm_train <- create_dtm(it_train, vectorizer)

View(dtm_train)

NFOLDS = 5

?cv.glmnet

View(df)

str(dtm_train)

str(dtm_train)

matrix_df = as.matrix(df)

#https://stackoverflow.com/questions/48179423/error-error-in-lognetx-is-sparse-ix-jx-y-weights-offset-alpha-nobs
glmnet_classifier <- cv.glmnet(x = dtm_train, y = matrix_df, 
                               family = 'binomial',
                               # L1 penalty
                               alpha = 1,
                               # interested in the area under ROC curve
                               type.measure = "auc",
                               # 5-fold cross-validation
                               nfolds = NFOLDS,
                               # high value is less accurate, but has faster training
                               thresh = 1e-3,
                               # again lower number of iterations for faster training
                               maxit = 1e3)

fit = glmnet(x = dtm_train, y =  df$text)


########################################################3
it_train <- itoken(df$text, 
                   tokenizer = tok_fun, 
                   ids = df$id, 
                   progressbar = FALSE)

vocab_bigram <- create_vocabulary(it_train, ngram = c(1L, 2L))
vocab_bigram <- prune_vocabulary(vocab_bigram, term_count_min = 10, 
                                 doc_proportion_max = 0.5)

bigram_vectorizer <- vocab_vectorizer(vocab_bigram)

dtm_train_bigram <- create_dtm(it_train, bigram_vectorizer)

glmnet_classifier_bigram <- cv.glmnet(x = dtm_train_bigram, y = df[['text']], 
                                      family = 'binomial', 
                                      alpha = 1,
                                      type.measure = "auc",
                                      nfolds = NFOLDS,
                                      thresh = 1e-3,
                                      maxit = 1e3)

str(stop_words)
  
