# topic modeling of sentiment
library(tidyverse)
library(tidytext)


load("data/afrisenti_translated.rdata")



tokens <- afrisenti_translated %>%  
  select(assigned_long,translatedText) %>% 
  unnest_tokens(word, translatedText ) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)

trans_dtm <- cast_dtm(tokens,word)

langs <- afrisenti_translated %>%
  select(assigned_long,detected_long) %>%
  distinct()


xt <- xtabs(~assigned_long+detected_long,afrisenti_translated) %>% 
  broom::tidy()  %>% 
  mutate(across(where(is.character),as.factor)) %>% 
  group_by(assigned_long) %>% 
  mutate(prop = round(100*n/sum(n))) %>% 
  arrange(desc(n)) %>% 
  filter(prop > 1)


gg <- xt %>% 
  ggplot(aes(assigned_long,detected_long,fill=prop)) + geom_tile() +
  labs(title = "African Languages Tweets\nQ: Who Are The Most Polyglot Tweeters",
       subtitle = "A: The Tsongan People",
       x = "Afrisenti Labeled Language Code",
       y= "Google Detected Language Code",
       caption = "source: Afrisenti Data Set") + 
  scale_fill_gradient(low = "#FFBF00",high = "#007000") +
  theme(text = element_text(family = "dm"),
        plot.background = element_rect(fill = "#FDECCD", color = NA),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank())
gg  
