# per capita tuskegee airmen by state
library(tidyverse)
library(rvest)

# I needed this for extra fonts on my windows system.  "Bahnschrift"
# is similar to the W.E.B DuBois fonts
# run once
# remotes::install_version("Rttf2pt1", version = "1.3.8")
library(extrafont)
# run once
# font_import()

loadfonts(device = "win")

# get tuskegee files
airmen <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv")


# Census.gov does not offer older census data through their api.
# I found some old published reports using the internet wayback machine.
# the population data is INCOMPLETE.  It is just for urban areas by state.
# Rural populations are omitted.  So the conclusions arrived at here
# might be wrong. Use this just for indicative purposes.
# Still, this is a great exercise is creative data wrangling!

# build a list of spreadsheets to download
page = rvest::read_html("https://web.archive.org/web/20120812191959/http://www.census.gov/population/www/documentation/twps0076/twps0076.html")
url_root <- "https://web.archive.org/web/20121018075158if_/http://www.census.gov/population/www/documentation/twps0076/"

#scrape xls urls from wayback machine
urls <- page %>% html_elements("a") %>% 
  html_attr("href") %>% 
  enframe(name=NULL,value="file") %>% 
  filter(str_detect(file,".xls")) %>% 
  mutate(url = paste0(url_root,file)) %>% 
  mutate(dest = paste0("data/airmen/",file))

# download spreadsheets from wayback machine
for (n in 2:nrow(urls)){
  download.file(urls$url[n],urls$dest[n],mode = "wb")
}

# function load spreadsheets and extract usable data
get_table <- function(n){
  xl_file = urls$dest[n]
  xl_state = substr(urls$file[n],1,2)
  pop <- readxl::read_xls(xl_file,
                           skip = 1,
                           range = cellranger::cell_limits(c(14,1),c(NA,7)),
                           na = "(NA)",
                           col_names = c("year","total","total_pct",
                                         "white","white_pct",
                                         "black","black_pct")
  ) %>% 
    select(year,total,total_pct,white,black) %>% 
    mutate(year = str_extract(year,"[0-9]{4}")) %>% 
    filter(!is.na(year)) %>% 
    mutate(across(.fns=as.numeric)) %>% 
    group_by(year) %>% 
    summarise(across(.fns=sum,na.rm=TRUE)) %>% 
    rename(city_count = total_pct) %>% 
    mutate(city_count = as.integer(city_count/100)) %>% 
    mutate(state = xl_state)
  return(pop)
  
}

# apply load, clean and combine individual state spreadsheets
state_pop <- map(1:nrow(urls),get_table) %>% 
  bind_rows() %>% 
  filter(state %in% state.abb)

state_pop_1940 <- state_pop %>% filter(year == 1940)

the_south <- c("AL","AR","DE","FL","GA","KY","LA","MD",
               "MS","NC","OK","SC","TN","TX","VA","WV","DC")

pop_adjusted <- airmen %>% 
  mutate(state = toupper(state)) %>% 
  count(state,name="airmen") %>% 
  left_join(state_pop_1940,by="state") %>% 
  filter(!is.na(year)) %>% 
  mutate(per_1000_blacks = round(airmen/black * 1000,3)) %>% 
  mutate(per_1000_all = round(airmen/total * 1000,3)) %>% 
  mutate(region = ifelse(state %in% the_south,"South","North"))
  
font_fam <- "Bahnschrift"
pop_adjusted %>% 
  filter(black > 10000) %>% 
  mutate(state = fct_reorder(state, per_1000_blacks)) %>%
  ggplot(aes(state,per_1000_blacks,fill=region)) + geom_col() +
  scale_fill_manual(values=c("darkblue","grey")) + 
  coord_flip() +
  labs(title="Tuskegee Airmen",
       subtitle="Pilot Origin per 1000 African-American People",
       x="States with More than 10,000 African-Americans in 1940", y= "",
       caption = "Source: CAF, Census.gov Population Division working paper 96, Viz: @adababbage") +
  theme(
    #titles
    plot.title = element_text(family=font_fam, hjust=0.5, size=20),
    plot.subtitle = element_text(family=font_fam, hjust=0.5, size=12),
    plot.caption = element_text(family=font_fam, hjust=0.5, size=8),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    #axes
    axis.title.x = element_text(family=font_fam, hjust=0.5, size=12),
    axis.text.x = element_text(family=font_fam, hjust=0.5, size=10),
    axis.text.y = element_text(family=font_fam, hjust=0.5, size=10),
    #background
    panel.background = element_rect(fill="#ffdea8", color=NA),
    panel.grid = element_blank(),
    plot.background = element_rect(fill="#ffdea8", color=NA),
  )


