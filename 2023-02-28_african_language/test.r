library(tidyverse)
library(tidymodels)
library(tidytext)
library(textrecipes)
library(stopwords)
library(hardhat)
library(quanteda)
library(caret)
library(glmnet)

data("small_fine_foods")

# traditional

reviews_train <- training_data %>%
  unnest_tokens(word, review) %>%
  count(product, word) %>%
  bind_tf_idf(word, product, n) %>% 
  cast_dfm(product, word, n)


docvars(reviews_train,"score") <-training_data$score

reviews_test <- testing_data %>%
  unnest_tokens(word, review) %>%
  count(product, word) %>%
  bind_tf_idf(word, product, n) %>% 
  cast_dfm(product, word, n)


docvars(reviews_test,"score") <-testing_data$score

# match all features in training set so they appear in test set
# note any new features are empty so no data leakage
reviews_test <- dfm_match(reviews_test, 
                                  features = featnames(reviews_train))



rf <- ranger::ranger(y=reviews_train$score, x = reviews_train, 
                     classification = TRUE)

lasso <- cv.glmnet(x = reviews_train,
                   y = reviews_train$score,
                   alpha = 1,
                   nfold = 5,
                   family = "binomial")

index_best <- which(lasso$lambda == lasso$lambda.min)
beta <- lasso$glmnet.fit$beta[, index_best]
head(sort(beta, decreasing = TRUE), 20)

actual <- reviews_test$score
predicted <- predict(lasso,reviews_test,type = "response")
tab <- table(actual,predicted)
tab
confusionMatrix(tab)

# -------------------------------------------------
# tidymodels
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
