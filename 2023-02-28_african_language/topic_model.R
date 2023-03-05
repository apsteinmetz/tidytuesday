# topic modeling of sentiment
library(tidyverse)
library(tidytext)
library(tidymodels)
library(tm)


load("data/afrisenti_translated.rdata")

my_stop_words = tibble(word = c("http","https","dey","de","al","url",
                                "t.co","rt","user","users",
                                as.character(1:100)))

tokens <- afrisenti_translated %>%  
  select(tweet_num,assigned_long,translatedText,label) %>% 
  unnest_tokens(word,translatedText )

not_words_rows <- tokens |> 
  rowid_to_column(var="word_num") |> 
  filter(word =="not") |> 
  mutate(word_num = word_num  + 1) |> 
  pull(word_num)





tokens %>% 
  anti_join(stop_words) |> 
  anti_join(my_stop_words)


# find most common words
chosen_words <- tokens |> 
  ungroup() |> 
  select(word) |> 
  count(word) |> 
  arrange(desc(n)) |> 
  slice_max(order_by = n,n=200)


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
  right_join(temp)

left_join(temp1,temp2)


randomForest::randomForest(label)



col_set <-   c("#952038","#F7C608","#319400")
  
# feelings about world powers
north_countries <- c("usa","europe","russia","china")
tokens |>
  filter(word %in% north_countries) |> 
  mutate(Area = str_to_title(word)) |> 
  mutate(Area = str_replace(Area,"Usa","USA")) |> 
  rename(Sentiment = label) |> 
  ggplot(aes(Area,n,fill = Sentiment)) + 
  geom_col(position = "dodge") + 
  scale_fill_manual(values =  col_set) + 
  labs(title = 'Q:How Does Africa Feel About the Global North?\nA: Not great, but Tweets mentioning Russia are mostly "neutral"',
       subtitle = "WARNING: Sample size is not large. Other caveats apply.",
       y = "Tweet Count",
       caption = "Source: Afrisenti data set") +
  coord_flip() +
  annotate("text",x = 2.5,y = 70,label = "Alternate names for\nareas are combined.\ne.g. EU = Europe") +
  theme(text = element_text(family = "dm"),
      plot.background = element_rect(fill = "#FDECCD", color = NA),
      legend.background = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_text(face = "bold"),
      plot.subtitle = element_text(face = "italic"),
      panel.background = element_blank())


