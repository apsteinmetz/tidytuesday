# sentiment analysis
library(tidyverse)
library(tidytext)
library(tictoc)
library(rayshader)

load(file="data/afrisenti_translated.rdata")

tweet_word_sentiment <- afrisenti_translated %>% 
  select(tweet_num,language_iso_code, translatedText) %>% 
  separate_rows(translatedText) %>%
  rename(word = translatedText) %>% 
  anti_join(stop_words) %>% 
  left_join(get_sentiments("afinn"),multiple = "all") %>% 
  mutate(value = replace_na(value,0)) %>% 
  rename(sentiment_afinn = value) %>% 
  left_join(get_sentiments("bing"),multiple = "all") %>% 
  mutate(sentiment = replace_na(sentiment,"neutral")) %>% 
  rowwise() %>% 
  mutate(sentiment_bing = switch(sentiment,"negative" = -1,"neutral"=0,"positive"=1))


senti_vote <- Vectorize(function(label_1,label_2){
  label <- label_1
  if((label_1 == "negative") & (label_2 == "positive")) label <-  "neutral"
  if((label_2 == "negative") & (label_1 == "positive")) label <-  "neutral"
  if((label_1 == "neutral") | (label_2 == "neutral")) label <-  label_1
  if(label_1 == "neutral") label <- label_2
  return(label)
})

tic()
tweet_sentiment <- tweet_word_sentiment %>% 
  group_by(language_iso_code,tweet_num) %>% 
  summarise(sentiment_afinn = sum(sentiment_afinn),
            sentiment_bing = sum(sentiment_bing),
            .groups = "keep") %>% 
  left_join(afrisenti_translated) %>% 
  mutate(label = as.factor(label)) %>% 
  mutate(label_afinn = cut(sentiment_afinn,breaks = c( -Inf,-1,0,Inf),
                           labels=c("negative","neutral","positive"))) %>% 
  mutate(label_bing = cut(sentiment_bing,breaks = c( -Inf,-1,0,Inf),
                           labels=c("negative","neutral","positive"))) %>% 
  # majority of english tweets are labeled "neutral"
  # where the native data is evenly split.
  # where vote is split make sentiment "neutral"
  # this is mighty slow
  mutate(label_combo = as.factor(senti_vote(label_afinn,label_bing)))
toc()

# some EDA
summary(tweet_sentiment)  

tweet_sentiment %>% 
  ggplot(aes(sentiment_afinn,sentiment_bing)) + geom_point()

  
tweet_sentiment %>% 
  filter(abs(sentiment_bing)<10) %>% 
  ggplot(aes(sentiment_bing)) + 
  geom_histogram(binwidth = 1)

tweet_sentiment %>% 
  filter(abs(sentiment_afinn)<10) %>% 
  ggplot(aes(sentiment_afinn)) + 
  geom_histogram(binwidth = 1)

# see how good basic sentiment tallies or translated tweets line up with 
# actual

xt <-  xtabs(~label+label_combo, data=tweet_sentiment) %>% 
  broom::tidy() %>% 
  mutate(across(where(is.character),as.factor)) %>% 
  # group_by(label) %>% 
  mutate(prop = round(100*n/sum(n)))


gg <- xt %>% 
  ggplot(aes(label,label_combo,fill=n)) + geom_tile() +
  geom_text(aes(label = paste0(as.character(prop),"%"))) +
  labs(title = "Q: Can We Use Google Translate And Test Sentiment in English?",
       subtitle = "Not really. Translated sentiment agrees only half the time.",
       x = "Native Language Sentiment",
       y= "Google Translate Sentiment",
       caption = "source: Afrisenti Data Set")

plot_gg(gg, width = 5, height = 5, multicore = TRUE, scale = 250, 
        zoom = 0.7, theta = 10, phi = 30, windowsize = c(800, 800))

# see if a sentiment model using the actual training classifier works
# with translated data


