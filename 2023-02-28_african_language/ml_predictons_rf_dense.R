# modeling of sentiment
library(tidyverse)
library(tidytext)
library(tidymodels)
library(tictoc)
library(yardstick)

# setwd("2023-02-28_african_language")

# ----- SETUP ------------------------------
load("~/R Projects/tidytuesday/2023-02-28_african_language/data/stopwords_af.rdata")
load("data/afrisenti_translated.rdata")
afrisenti_translated <- afrisenti_translated %>%
  mutate(lang = as.factor(assigned_long)) %>%
  mutate(sentiment = as.factor(label)) %>% 
  select(tweet_num,intended_use,sentiment,lang,tweet,translatedText)

use_english = TRUE
if (use_english) {
  afrisenti_translated <- afrisenti_translated %>% 
    mutate(tweet = translatedText)
  }
afrisenti_translated <- afrisenti_translated %>% 
  select(-translatedText)

# do we make language a feature?
append_language =  FALSE
if (append_language) {
  afrisenti_translated <- afrisenti_translated %>% 
    mutate(tweet = paste(tweet,lang))
}

# make negation tokens
make_negations =  TRUE
if (make_negations) {
  afrisenti_translated <- afrisenti_translated %>% 
    mutate(tweet = str_replace(tweet, "not ","not_"))
}

tweet_train <- afrisenti_translated %>% 
  filter(intended_use == "train") %>% 
  select(tweet_num,sentiment,lang,tweet,tweet)

tweet_test <- afrisenti_translated %>% 
  filter(intended_use == "test") %>% 
  select(tweet_num,sentiment,lang,tweet,tweet)

tweet_dev <- afrisenti_translated %>% 
  filter(intended_use == "dev") %>% 
  select(tweet_num,sentiment,lang,tweet,tweet)

# add my stop words to defaults
my_stop_words = tibble(word = c("http","https","dey","de","al","url","na","t.co","rt","user","users","wey","don",
                                as.character(1:100)))
                           

full_stop_words <-  c(
  stop_words$word,
  my_stop_words$word,
  stopwords_af$word
)

# # turn words preceded by "not" into "not_<word>"
# # to create a negated token
# detect_negations <- function(tokens,negation_words = c("not")) {
#   # function to negate tokenized data
#   tokens <- tokens %>% rowid_to_column(var="word_num")
#   not_words_rows <- tokens |> 
#     filter(word %in% negation_words) |> 
#     mutate(word_num = word_num) |> 
#     pull(word_num)
#   tokens <- tokens %>% 
#     # create negated terms
#     filter(!(word_num %in% not_words_rows)) |> 
#     mutate(word = ifelse(word_num %in% (not_words_rows+1),paste0("not_",word),word)) |> 
#     select(-word_num)
#   return(tokens)
# }


only_top_words <- function(tokens, word_count = 1000) {
  chosen_words <- tokens |>
    ungroup() |>
    select(word) |>
    count(word) |>
    slice_max(order_by = n, n = word_count) %>% 
    select(-n)
  return(inner_join(tokens,chosen_words))
}

token_filter <- function(tokens) {
  tokens %>%
    # create negations where "not" is before a word
    # detect_negations() %>% 
    # remove English stop words
    anti_join(stop_words) %>%
    # remove African stop words by language
    anti_join(stopwords_af) %>%
    # remove my additional stop words
    anti_join(my_stop_words) %>%
    # call any word of 1 or 2 characters a stop word
    filter(str_length(word) > 2)
}


make_dfm <- function(tweet_data){ 
tweet_tokens <- tweet_data %>%
  unnest_tokens(word, tweet) %>% 
  token_filter() %>% 
  only_top_words(1000) %>% 
  count(tweet_num,word) %>% 
  bind_tf_idf(word,tweet_num,n) %>%
  select(tweet_num, word, tf_idf) %>%
  pivot_wider(names_from = word, names_prefix = "word_",
              values_from = tf_idf,values_fill = 0) %>% 
  left_join(select(tweet_data,tweet_num,sentiment)) %>% 
  select(sentiment,everything(),-tweet_num)
}

tweet_dfm_train <- make_dfm(tweet_train)
tweet_dfm_test <- make_dfm(tweet_test)

# ---------------------------------------------------------
# run the models

cores <- parallel::detectCores()
rf_mod <- parsnip::rand_forest(trees = 100) %>% 
  set_engine("ranger",num.threads = cores,importance = "impurity") %>% 
  set_mode("classification")


rf_recipe <- 
  recipe(sentiment ~ ., data = tweet_dfm_train)

rf_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rf_recipe)

translate(rf_mod)


tic()
rf_fit <- rf_workflow %>% 
  fit(tweet_dfm_train)
toc()

pred <- predict(rf_fit,tweet_dfm_test)

# Validation set assessment #1: looking at confusion matrix
predicted_for_table <- tibble(actual = tweet_dfm_train$sentiment,predicted=pred$.pred_class)

table(predicted_for_table)

test <- tibble(truth=tweet_dfm_test$sentiment,predicted=pred$.pred_class)

metrics(test,truth,predicted)

