# topic modeling of sentiment
library(tidyverse)
library(tidytext)
library(tidymodels)
library(tm)
library(tictoc)
library(butcher)
library(rayshader)
library(yardstick)

setwd("2023-02-28_african_language")
load("data/afrisenti_translated.rdata")


# ----- SETUP ------------------------------
afrisenti_translated <- afrisenti_translated %>%
  mutate(lang = as.factor(assigned_long)) %>%
  mutate(sentiment = as.factor(label))

# add my stop words to defaults
my_stop_words = tibble(word = c("http","https","dey","de","al","url","na",
                                "t.co","rt","user","users","wey","don",
                                as.character(1:100))) %>% 
  bind_rows(stop_words)

# split into words. Choose native or English
tokenize <- function(dataset, use_translated = FALSE) {
  tokens <- dataset %>%
    select(tweet_num,
           sentiment,
           lang,
           ifelse(use_translated, "translatedText", "tweet")) %>%
    unnest_tokens(word, !!(ifelse(
      use_translated, "translatedText", "tweet"
    )))  |>
    rowid_to_column(var = "word_num")
  return(tokens)
}


# turn words preceded by "not" into "not_<word>"
# to create a negated token
detect_negations <- function(tokens) {
  # this helps for english only, obviously  
  not_words_rows <- tokens |> 
    filter(word =="not") |> 
    mutate(word_num = word_num  + 1) |> 
    pull(word_num)
  tokens <- tokens %>% 
    # create negated terms
    mutate(word = ifelse(word_num %in% not_words_rows,paste0("not_",word),word))
  return(tokens)
}

# word list size will be critical
# full set will be wasteful and slow
# one author suggested 2000
# remove stop words first

get_top_words <- function(tokens, word_count = 1000) {
  chosen_words <- tokens |>
    anti_join(my_stop_words) %>% 
    ungroup() |>
    select(word) |>
    count(word) |>
    arrange(desc(n)) |>
    slice_max(order_by = n, n = word_count)
  return(chosen_words)
}

# make document term matrix including words and language. omit stop words. note
# that negation must be done before removing stop words or "not" will be stripped.
make_dtm <- function(tokens) {
  chosen_words <- get_top_words(tokens,word_count = 1000)
  tweet_dtm <- tokens |>
    inner_join(chosen_words) |>
    group_by(tweet_num, word) |>
    count(word) |>
    cast_dtm(tweet_num, word, n) %>% 
    tidy() %>% 
    mutate(count = as.integer(count))
  
  
  dtmm <- tweet_dtm |>
    pivot_wider(names_from = term, values_from = count) |>
    mutate(across(everything(), ~ replace_na(.x, 0))) %>% 
    mutate(tweet_num = as.numeric(document)) %>% 
    left_join(select(afrisenti_translated,tweet_num,sentiment,lang),by="tweet_num") %>% 
    select(sentiment,lang,everything()) %>% 
    select(-document,-tweet_num) %>% 
    
  
  return(dtmm)
}


# ----- END SETUP ------------------------------

# do it with native
# 2- letter words are a huge part of the corpus.
# I don't know what I'm doing but 2-letter words probably don't convey
# as much as longer words.

tokens_a <- tokenize(afrisenti_translated) %>% 
  filter(str_length(word) > 2)

# do it with English translations
tokens_e <- afrisenti_translated %>% 
  filter(intended_use == "train") %>% 
  tokenize(use_translated = TRUE)


# ---------------------------------------------------------
# run the models

cores <- parallel::detectCores()
rf_mod <- parsnip::rand_forest(trees = 100) %>% 
  set_engine("ranger",num.threads = cores,importance = "impurity") %>% 
  set_mode("classification")


rf_recipe <- 
  recipe(sentiment ~ ., data = dtmm) 



tic()
dtmm <- make_dtm(tokens_e)
toc()

rf_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rf_recipe)

translate(rf_mod)


rf_workflow %>% 
  fit(mtcars) %>% 
  extract_fit_parsnip() %>% 
  vip(num_features = 10)


tic()
rf_fit <- rf_workflow %>% 
  fit(dtmm)
toc()

# summary(predict(rf_fit,dtmm[-1]))

# Validation set assessment #1: looking at confusion matrix
predicted_for_table <- tibble(dtmm[,1],predict(rf_fit_native,dtmm[,-(1:2)])) %>% 
  rename(observed = label,predicted = .pred_class)

xt <- table(predicted_for_table) %>% 
  broom::tidy() %>% 
  mutate(across(where(is.character),as.factor)) %>% 
  # group_by(label) %>% 
  mutate(prop = round(100*n/sum(n)))

gg <- xt %>% 
  ggplot(aes(observed,predicted,fill=n)) + geom_tile() +
  labs(title = "African Languages Tweets\nQ: Can We Train on English Google Translations?",
       subtitle = "A: Yes. A random forest model works pretty well.",
       x = "Native Language Sentiment",
       y= "Google Translate Sentiment",
       caption = "source: Afrisenti Data Set") + 
  scale_fill_gradient(low = "#FFBF00",high = "#007000") +
  theme(text = element_text(family = "dm"),
        plot.background = element_rect(fill = "#FDECCD", color = NA),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank())

gg + geom_text(aes(label = paste0(as.character(prop),"%")))

plot_gg(gg, width = 5, height = 5, multicore = TRUE, scale = 250, 
        zoom = 0.7, theta = 10, phi = 30, windowsize = c(800, 800))
pretty_colours <- c("#F8766D","#00BA38","#619CFF")

# Validation set assessment #2: ROC curves and AUC
# Needs to import ROCR package for ROC curve plotting:
library(ROCR)
# Calculate the probability of new observations belonging to each class
predicted_for_roc_curve<- tibble(dtmm[,1:2],
                              predict(rf_fit_native,dtmm[,-1],type="prob")) %>% 
  rename(observed = label)

predicted_for_roc <- bind_cols(predicted_for_table,predicted_for_roc_curve[,2:4])

metrics(predicted_for_roc,observed,predicted)

predicted_for_roc_curve %>% 
  group_by(assigned_long) %>% 
  roc_curve(observed,.pred_negative:.pred_positive) %>% 
  autoplot()
