# translate african tweets
# 111,720 tweets cost $14.87 to translate on the google cloud. Not cheap.
# some money could have been saved by omitting the tweets already in English.

library(tidyverse)
library(googleLanguageR)
library(furrr)
library(rvest)
library(tictoc)


googleLanguageR::gl_auth(Sys.getenv("googleLanguage"))

# test
tweet <- afrisenti$tweet[1]
googleLanguageR::gl_translate_detect(tweet)
googleLanguageR::gl_translate(tweet)


# batch up requests
# this cost about $15. About 10% of the tweets are already in English though
future::plan(multicore)
payload_size = 100
tic()
afrisenti_translated <- seq(0,nrow(afrisenti),by = payload_size) %>% 
  furrr::future_map_dfr(\(x) {gl_translate(afrisenti$tweet[(x+1):(x+payload_size)])},
      .progress = TRUE)
toc()
future::plan(sequential)

afrisenti_translated <- afrisenti_translated %>% 
  na.omit() %>% 
  select(-text) %>% 
  bind_cols(afrisenti) %>% 
  rowid_to_column(var = "tweet_num") %>% 
  mutate(tweet_num = as.numeric(tweet_num))
  mutate(intended_use = as_factor(intended_use)) %>%
  mutate(detectedSourceLanguage = as_factor(detectedSourceLanguage)) %>%
  mutate(language_iso_code = as_factor(language_iso_code)) %>%
  mutate(label = as.factor(label))


# detected language often disagrees with assigned language
  # from: https://arxiv.org/pdf/2201.08277.pdf 
  # 3.2. Language Detection and Data Cleaning 
  # Stopwords overlap across indigenous languages in a multilingual
  # society such as Nigeria (Caswell et al., 2020). This results in tweets being
  # collected in a language that differs from the query language. For example,
  # using the stop word "nke" to crawl tweets in Igbo produces tweets in Hausa,
  # such as "amin ya rabbi godiya nke". To mitigate this, we collected tweets
  # based on locations where a language is predominantly spoken, using the
  # location, longitude, latitude and radius parameters (25 miles) to specify a
  # circular geographic area. We also used Google CLD35 and Natural Language
  # API6 to detect the language of the collected tweets. Pidgin is not supported
  # by the API, so we used the stopword list to build an n-gram language
  # detection tool to detect Pidgin. Before annotation, we cleaned the tweets.
  # Retweets and duplicates were removed. We removed URLs and mentions, as well
  # as trailing and redundant white spaces, converted all tweets to lowercase,
  # and removed tweets with less than three words as they may contain
  # insufficient information for sentiment analysis (Yang et al., 2018).
  
# get languages from wikipedia
# iso_lang <- html_table(read_html("https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes#External_links"))[[2]]
# save(iso_lang,file="data/iso_lang.rdata")
load("data/iso_lang.rdata")

iso_lang <- iso_lang %>% 
  rename(assigned_language = `639-2/T`,
         detected_language = `639-1`,
         language = `ISO language name`) %>% 
  select(1:3)

# clean up langauge names
afrisenti_translated <- afrisenti_translated %>% 
  mutate(language_iso_code = str_replace_all(language_iso_code,"pt-MZ","por")) %>% 
  mutate(language_iso_code = str_replace_all(language_iso_code,"ary","ara")) %>% 
  mutate(language_iso_code = str_replace_all(language_iso_code,"arq","ara")) %>% 
  mutate(language_iso_code = str_replace_all(language_iso_code,"pcm","eng")) %>% 
  rename(assigned_language = language_iso_code,
         detected_language = detectedSourceLanguage) %>% 
  left_join(select(iso_lang,-assigned_language)) %>% 
  rename(detected_long = language) %>% 
  left_join(select(iso_lang,-detected_language)) %>% 
  rename(assigned_long = language)
  

save(afrisenti_translated,file="data/afrisenti_translated.rdata")
