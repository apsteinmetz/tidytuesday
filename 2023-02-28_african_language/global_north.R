# topic modeling of sentiment
library(tidyverse)
library(tidytext)

setwd(dir()[1])
load("data/afrisenti_translated.rdata")

langs <- afrisenti_translated %>%
  mutate(assigned_long = fct_lump_prop(as.factor(assigned_long),prop= .01)) |> 
  mutate(detected_long = fct_lump_prop(as.factor(detected_long),prop= .01)) |> 
  
  xt <- xtabs(~assigned_long+detected_long,langs) %>% 
  broom::tidy()  %>% 
  mutate(across(where(is.character),as.factor)) %>% 
  group_by(assigned_long) %>% 
  mutate(prop = round(100*n/sum(n))) %>% 
  arrange(desc(n)) %>% 
  filter(n > 0)


gg <- xt %>% 
  ggplot(aes(assigned_long,detected_long,fill=prop)) + geom_tile() +
  labs(title = "African Languages Tweets\nDiscrepencies Between Assigned and Detected Language",
       x = "Afrisenti Labeled Language",
       y= "Google Detected Language",
       fill = "% of Tweets",
       caption = "source: Afrisenti Data Set, Google") + 
  scale_fill_gradient(low = "#FFBF00",high = "#007000") +
  theme(text = element_text(family = "dm"),
        plot.background = element_rect(fill = "#FDECCD", color = NA),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle=45),
        panel.background = element_blank(),
        panel.grid = element_blank())
gg  



tokens <- afrisenti_translated %>%  
  select(assigned_long,translatedText,label) %>% 
  # can we separate "US" from "us"?
  mutate(translatedText = str_replace(translatedText," US "," usa ")) |> 
  mutate(translatedText = tolower(translatedText)) |> 
  mutate(translatedText = str_replace(translatedText,"united states","usa")) |> 
  unnest_tokens(word, translatedText ) %>% 
  mutate(word = str_replace(word,"eu$","europe")) |> 
  mutate(word = str_replace(word,"america","usa")) |> 
  anti_join(stop_words) %>% 
  count(word,label, sort = TRUE) |> 
  arrange(word)

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

