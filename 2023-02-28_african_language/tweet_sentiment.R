# sentiment analysis
library(tidyverse)
library(tidytext)

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

tweet_sentiment <- tweet_word_sentiment %>% 
  group_by(language_iso_code,tweet_num) %>% 
  summarise(sentiment_afinn = sum(sentiment_afinn),
            sentiment_bing = sum(sentiment_bing),
            .groups = "keep") %>% 
  left_join(afrisenti_translated)

# normalize sentiment to 10

tweet_sentiment %>% 
  ggplot(aes(sentiment_afinn,sentiment_bing)) + geom_point()
  
  
