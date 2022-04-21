# crossword puzzle analysis
library(tidyverse)
library(lubridate)

# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2022-04-19')
tuesdata <- tidytuesdayR::tt_load(2022, week = 16)

big_dave <- tuesdata$big_dave

# Or read in the data manually

big_dave <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/big_dave.csv')
london_times <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/times.csv')
ny_times <- read_csv("data/crossword/nyt.csv")

len_regex <- "\\([0-9]+(,[0-9]+)*\\)$" # comma delimiated string of numbers
london <- london_times %>% 
  na.omit() %>% 
  filter(str_detect(puzzle_name,"QC|Cryptic",negate = TRUE)) %>% 
#  filter(str_detect(puzzle_name,"Sunday")) %>% 
  mutate(day = wday(puzzle_date,label=TRUE),.after="clue") %>% 
  mutate(definition_length = str_length(definition),.after="clue") %>% 
  mutate(direction = ifelse(str_detect(clue_number,"a"),"Across","Down"),.after="clue") %>% 
  mutate(answer_length = str_extract(clue,len_regex),.after="clue") %>% 
  mutate(clue = str_remove(clue," \\(.+\\)$")) %>% 
  mutate(clue_length = str_length(clue),.after="clue") %>% 
  mutate(answer_length = str_remove(answer_length,"\\(")) %>% 
  mutate(answer_length = str_remove(answer_length,"\\)")) %>% 
  mutate(answer_length = str_replace_all(answer_length,",","+")) %>% 
  rowwise() %>% 
  mutate(answer_length = eval(parse(text=answer_length))) %>% 
  {.}

london %>% 
  group_by(answer) %>% 
  tally() %>% 
  ggplot(aes(n)) + geom_histogram()
  
  

