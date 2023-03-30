# Test

library(tidymodels)
library(tidytext)
library(textrecipes)

data("tate_text", package = "modeldata")

tate_text <- tate_text |>
  select(medium, year) |>
  mutate(year = if_else(year > 2000, "2000s", "1900s"))

tate_text

set.seed(1234)
tate_split <- initial_split(tate_text)

tate_train <- training(tate_split)
tate_test <- testing(tate_split)

rec_small <- recipe(year ~ medium, data = tate_train) |>
  step_tokenize(medium) |>
  step_tokenfilter(medium, max_tokens = 20) |>
  step_tf(medium)

rec_big <- recipe(year ~ medium, data = tate_train) |>
  step_tokenize(medium) |>
  step_tf(medium)

lr_spec <- logistic_reg()

cores <- parallel::detectCores()
rf_spec <- parsnip::rand_forest(trees = 100) %>% 
  set_engine("ranger",num.threads = cores,importance = "impurity") %>% 
  set_mode("classification")


wf_spec_big <- workflow() |>
  add_recipe(rec_big) |>
  add_model(rf_spec)

wf_spec_small <- workflow() |>
  add_recipe(rec_small) |>
  add_model(rf_spec)

results <- bench::mark(
  iterations = 1, check = FALSE,
  fit_big   <- fit(wf_spec_big,   data = tate_train),
  fit_small <- fit(wf_spec_small, data = tate_train)
)

results

wf_fit <- fit(wf_spec, data = tate_train)

tik()
predict(wf_fit, new_data = tate_train)
tok()
predict(wf_fit, new_data = tate_test)

tate_new = tibble(medium = "Finger paint on sofa", year = "2000s")

predict(wf_fit, new_data = tate_new)


# ----------------------------------------
library(tidymodels)
library(textrecipes)
library(tidytext)
library(stringr)

tate_text <- tate_text |>
  select(medium, year)

tate_nots = tibble(medium = "Etching on not canvas", year = 2000)
tate_text = bind_rows(tate_nots,tate_text)

# APPROACH 1: preproccess raw data
tate_text |> 
  mutate(medium = str_replace(medium, "not ","not_"))

# APPROACH 2: process tokenized data
detect_negations <- function(tokens,negation_words = c("not")) {
  # function to negate tokenized data
  tokens <- tokens %>% rowid_to_column(var="word_num")
  not_words_rows <- tokens |> 
    filter(word %in% negation_words) |> 
    mutate(word_num = word_num) |> 
    pull(word_num)
  tokens <- tokens %>% 
    # create negated terms
    filter(!(word_num %in% not_words_rows)) |> 
    mutate(word = ifelse(word_num %in% (not_words_rows+1),paste0("not_",word),word)) |> 
    select(-word_num)
  return(tokens)
}

unnest_tokens(tate_text,word,medium) |> 
  detect_negations()

