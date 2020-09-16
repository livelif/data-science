setwd("/home/paulob/Documentos/dataScience/projetos/projetos/analise_de_sentimento")

library(rtweet)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidytext)
library(textdata)

app_name = ""

consumer_key <- ""

consumer_secret <- ""

access_token <- ""
  
access_secret <- ""

?create_token
create_token(app = app_name, consumer_key = consumer_key, consumer_secret = consumer_secret,
             access_token = access_token, access_secret = access_secret)

canada <- search_tweets("#Canada", n = 100, include_rts = FALSE)

tweets.Canada <- canada %>% select(screen_name, text)

head(tweets.Canada$text)

# remove http elements
tweets.Canada$stripped_text1 <- gsub("http\\S+", "", tweets.Canada$text)

# remove punctuation and add id for each tweet
tweets.Canada_stem <- tweets.Canada %>% select(stripped_text1) %>% unnest_tokens(word, stripped_text1)

head(tweets.Canada_stem)

cleaned_tweets.Canada <- tweets.Canada_stem %>% anti_join(stop_words)

head(cleaned_tweets.Canada)

cleaned_tweets.Canada %>% count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_classic() +
  labs(x = "Count", y = "Unique words", title = "Unique word counts found in #Canada tweets")

get_sentiments("bing") %>% filter(sentiment == "positive")

bing_canada <- 
  cleaned_tweets.Canada %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = TRUE) %>% ungroup()

bing_canada %>% group_by(sentiment) %>% 
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Tweets Canada", y = "Contribution to sentiment",
       x = NULL) + coord_flip() + theme_bw()

sentimet_bing = function(twt) {
  twt_tbl = tibble(text = twt) %>% mutate(stripped_text = gsub("http\\S+","",text)) %>%
    unnest_tokens(word, stripped_text) %>%
    anti_join(stop_words) %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup() %>%
    mutate(
      score = case_when(sentiment == "negative" ~ n*(-1),
                        sentiment == "positive" ~ n*1)
    )
  
  sent.score = case_when(
    nrow(twt_tbl)==0~0,
    nrow(twt_tbl)>0~sum(twt_tbl$score)
  )
  
  zero.type = case_when(
    nrow(twt_tbl) == 0~"Type 1",
    nrow(twt_tbl) > 0~"Type 2"
  )
  
  list(score = sent.score, type = zero.type, twt_tbl = twt_tbl)
}


canada_sent = lapply(canada$text, function(x){sentimet_bing(x)})

