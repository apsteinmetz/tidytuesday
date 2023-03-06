# topic modeling of sentiment
library(tidyverse)
library(tidytext)
library(tidymodels)
library(tm)
library(tictoc)

setwd("2023-02-28_african_language")
load("data/afrisenti_translated.rdata")

my_stop_words = tibble(word = c("http","https","dey","de","al","url","na",
                                "t.co","rt","user","users","wey","don",
                                as.character(1:100)))

# I don't know what I'm doing but 2-letter words probably don't convey
# as much as longer words

tokens <- afrisenti_translated %>%  
  select(tweet_num,assigned_long,translatedText,label) %>% 
  unnest_tokens(word,translatedText )  |> 
  rowid_to_column(var="word_num")

# # do it with native
# tokens <- afrisenti_translated %>%  
#   select(tweet_num,assigned_long,tweet,label) %>% 
#   unnest_tokens(word,tweet )  |> 
#   rowid_to_column(var="word_num")
# 
# tokens <- tokens |> 
#   filter(str_length(word) > 2)
  
  

not_words_rows <- tokens |> 
  filter(word =="not") |> 
  mutate(word_num = word_num  + 1) |> 
  pull(word_num)

tokens <- tokens %>% 
  # create negated terms
  mutate(word = ifelse(word_num %in% not_words_rows,paste0("not_",word),word)) |> 
  anti_join(stop_words) |> 
  anti_join(my_stop_words)


# find most common words

# word list size will be critical
# full set will be wasteful and slow
# one author suggested 2000

word_count <- 2000

chosen_words <- tokens |> 
  ungroup() |> 
  select(word) |> 
  count(word) |> 
  arrange(desc(n)) |> 
  slice_max(order_by = n,n=word_count)


tweet_dtm <- tokens |> 
  inner_join(chosen_words) |> 
  group_by(tweet_num,word) |> 
  count(word) |> 
  cast_dtm(tweet_num,word,n)

temp1 <- afrisenti_translated |> 
  select(tweet_num,label) |> 
  rename(document = tweet_num)|> 
  mutate(document = as.character(document))


temp2 <- tidy(tweet_dtm) |> 
  pivot_wider(names_from = term,values_from = count) |>  
  mutate(across(everything(), ~replace_na(.x, 0)))



dtmm <- right_join(temp1,temp2) |> 
  select(-document)


# ---------------------------------------------------------
# run the models
cores <- parallel::detectCores()
cores

rf <- parsnip::rand_forest() %>% 
  set_engine("randomForest",num.threads = cores-1) %>% 
  set_mode("classification")



tic()
rf_fit <- rf_mod %>% 
  fit(label ~ ., data = dtmm)
toc()

summary(predict(rf_fit,dtmm))


