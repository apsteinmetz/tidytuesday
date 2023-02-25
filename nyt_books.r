---
title: "NYT Books"
format:
  html:
    theme: default
---

```{r}
# crossword puzzle analysis
library(tidyverse)
library(lubridate)
library(tidytuesdayR)
library(jpeg)
library(grid)

# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

#nyt_titles <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/#2022-05-10/nyt_titles.tsv')

nyt_full <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv')

# fix titles table
nyt_titles_b <- nyt_full %>% 
  group_by(title_id,author,title) %>% 
  transmute(author,title,
            first_week = min(week),
            best_rank = min(rank),
            total_weeks=n())

nyt_titles <- nyt_full %>% 
  rename(debut_rank = rank,first_week = week) %>% 
  right_join(nyt_titles_b) %>% 
  unique() %>% 
  mutate(year = year(first_week)) %>% 
  select(title_id,title,author,year,total_weeks,first_week,debut_rank,best_rank)
  
```

```{r}
img_comm <- rasterGrob(readJPEG("images/commodore.jpg"))

nyt_titles %>% 
  filter(author == "Patrick O'Brian") %>% 
  select(title,first_week,debut_rank,best_rank,total_weeks) %>% 
  knitr::kable()
```

```{r}
nyt_full %>% 
  filter(author == "Patrick O'Brian") %>% 
  group_by(title) %>% 
  ggplot(aes(week,rank,color=title)) + geom_line() + 
  geom_point(size = 3) + 
  scale_y_reverse(limits = c(16,1),breaks=1:16) +
  annotation_custom(img_comm,xmin = as.Date("1996-04-01"),xmax = as.Date("1997-08-01"),ymax=-1,ymin = -6)


```

