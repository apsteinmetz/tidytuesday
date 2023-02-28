# translate african tweets

library(tidyverse)
library(googleLanguageR)
library(furrr)
library(tictoc)

googleLanguageR::gl_auth(Sys.getenv("googleLanguage"))


tweet <- afrisenti$tweet[1]
googleLanguageR::gl_translate_detect(tweet)
googleLanguageR::gl_translate(tweet)


# batch up requests
plan(multicore,workers = 4)
payload_size = 100
tic()
afrisenti_tranlated <- seq(0,nrow(afrisenti),by = payload_size) %>% 
  future_map_dfr(\(x) {gl_translate(afrisenti$tweet[(x+1):(x+payload_size)])},
      .progress = TRUE)
toc()
plan(sequential)

afrisenti_translated <- afrisenti_translated %>% 
  na.omit() %>% 
  select(-text) %>% 
  bind_cols(afrisenti) %>% 
  rowid_to_column(var = "tweet_num") %>% 
  mutate(tweet_num = as.numeric(tweet_num))
  

save(afrisenti_translated,file="data/afrisenti_translated.rdata")
