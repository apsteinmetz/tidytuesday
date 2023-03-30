# create a single file of African stop words

my_dir <- "2023-02-28_african_language/"

stop_files <- paste0(my_dir,"data/stopwords/",
                     list.files(paste0(my_dir,"data/stopwords"),pattern = "*.txt"))


read_stop <- function(fname){
  lang_iso <- str_extract(fname,"[a-z]{2}\\.txt$") %>% 
    str_remove("\\.txt")
  stop_words <- read_file(fname) %>% 
    enframe(value="word") %>% 
    separate_longer_delim("word","\n") %>% 
    mutate(lang_iso = lang_iso) %>% 
    select(lang_iso,word)
  return(stop_words)
}

stopwords_af <- map_df(stop_files,read_stop)

load(paste0(my_dir,"data/iso_lang.rdata"))

stopwords_af <- left_join(stopwords_af,rename(iso_lang,lang_iso = '639-1')) %>% 
  rename(lang = `ISO language name`) %>% 
  select(lang,word)

save(stopwords_af,file = paste0(my_dir,"data/stopwords_af.rdata"))

