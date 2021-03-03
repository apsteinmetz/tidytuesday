#superbowl
library(tidyverse)
library(readr)

youtube <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv')


youtube %>%
  select(year,view_count,like_count,dislike_count) %>% 
  mutate(positive_pct = like_count/view_count) %>% 
  mutate(dislike_ratio = dislike_count/like_count) %>%
  # remove_missing() %>% 
  group_by(year) %>%
  summarise(count = n(),
            positive_pct = mean(positive_pct,na.rm=TRUE),
            like_ratio = 1-mean(dislike_ratio,na.rm=TRUE)) %>% 
  {.} -> temp

temp %>% 
  ggplot(aes(year,positive_pct)) + geom_line() + geom_smooth(se=FALSE,method="lm")

  
