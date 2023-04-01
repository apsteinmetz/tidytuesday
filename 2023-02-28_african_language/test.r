library(tidyverse)
library(tidymodels)
library(tidytext)
library(textrecipes)
library(stopwords)
library(hardhat)

data("small_fine_foods")

sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")

text_rec <-
  recipe(score ~ review, data = training_data) %>%
  step_tokenize(review)  %>%
  step_stopwords(review) %>%
  step_tokenfilter(review, max_tokens = 1e3) %>%
  step_tfidf(review)

xg_model <- parsnip::boost_tree(trees = 100) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

las_model <- parsnip::logistic_reg(penalty = 0.02, mixture = 1) %>% 
  set_engine("glmnet")

rf_model <- parsnip::rand_forest(trees = 100) %>% 
  set_engine("ranger",importance = "impurity") %>% 
  set_mode("classification")

xg_wf <- 
  workflow() %>%
  add_recipe(text_rec, blueprint = sparse_bp) %>%
  add_model(xg_model)

las_wf <- 
  workflow() %>%
  add_recipe(text_rec, blueprint = sparse_bp) %>%
  add_model(las_model)

rf_wf <- 
  workflow() %>%
  add_recipe(text_rec, blueprint = sparse_bp) %>%
  add_model(rf_model)

fit_xg <- fit(xg_wf,training_data)
fit_rf <- fit(rf_wf,training_data)
fit_las <- fit(las_wf,training_data)


summary(predict(fit_xg,training_data))
summary(predict(fit_las,training_data))
summary(predict(fit_rf,training_data))
