# Tidy Tuesday 2023 Week 08
# Art Steinmetz

library(magick)
library(cluster)
library(stats)
library(scales) #just for for the show_col() function
library(tidyverse)
library(ggfortify)
library(ggpubr)
library(gridExtra)
library(tidytuesdayR)
library(tidytext)
library(furrr) # parallel processing
library(pracma) # optional timer



# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

# tuesdata <- tidytuesdayR::tt_load('2023-02-21')
# tuesdata <- tidytuesdayR::tt_load(2023, week = 8)

# bob_ross <- tuesdata$bob_ross

# bob_ross <- read_csv("data/bob_ross.csv")

# Or read in the data manually
bob_ross <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-21/bob_ross.csv')
# Save the data.
 write_csv(
   bob_ross,
   here::here("data","bob_ross.csv")
 )

# -----------------------------------------------------
# download the paintings
# This is gonna take some time
download_paintings <- function(ndx) {
  url <- bob_ross$img_src[ndx]
  # Define the local file name for the downloaded file
  local_file <-
    paste0("img/bob_ross/", bob_ross$painting_index[ndx], ".png")
  # Download the file using download.file()
  dl_result <- 
    download.file(url, local_file, mode = "wb", quiet = TRUE)
  return(tibble(img_src = url,error = dl_result))
}

print("Downloading paintings.")
# Try it in parallel
plan(multisession,workers = 4)
tic()
# error list should be all zeros
error_list <- future_map(1:nrow(bob_ross),download_paintings,.progress = TRUE) %>% 
  bind_rows()
toc()
plan(sequential)


# # Same as above in sequential download fashion so slower
# tic()
# # error list should be all zeros
# error_list <- map(1:nrow(bob_ross),download_paintings,.progress = TRUE) %>% 
#   bind_rows()
# toc()

if (sum(error_list$error)>0) {
  print("Error(s) downloading! The following URLs failed to download")
  filter(error_list, error > 0)
} else print("No Errors Downloading!")


# -----------------------------------------------------
# create RGB arrays
# image is optinally scaled down in size and color space to reduce the data size
max_colors = Inf
img_scale = "20%"

# extract RGB data from images
load_img_data <- function(ndx){
  local_file <- paste0("img/bob_ross/",bob_ross$painting_index[ndx],".png")
  img_data <- magick::image_read(local_file) %>%
    image_scale(img_scale)
  if (max_colors < Inf) img_data <- image_quantize(img_data, max = max_colors)
  img_rgb <- as.raster(img_data) %>%
    col2rgb(alpha=FALSE) %>%
    t() %>%
    as_tibble() %>%
    mutate(title = bob_ross$painting_title[ndx],
           index = bob_ross$painting_index[ndx],
           color_hex = rgb(red,green,blue,maxColorValue = 255))
  return(img_rgb)
}

# Build a data frame of RGB of all images
print("Extracting RGB arrays from paintings.")

# Parallel processing to speed it up
# This is about half the time on my machine
plan(multisession,workers = 4)
tic()
img_list <- future_map(1:nrow(bob_ross),load_img_data,.progress = TRUE) %>% 
  bind_rows()
toc()
plan(sequential)

# # Same as above if you want to do it in non-parallel fashion
# tic()
# img_list <- map(1:nrow(bob_ross),load_img_data) %>% 
#   bind_rows()
# toc()

# save(img_list,file="data/ross_img_data.rdata")
# load("data/ross_img_data.rdata") # "image_list" df

# -----------------------------------------------------
# Do the analysis
#make data tidy first
img_tidy <- img_list %>% 
  pivot_longer(cols = c(red,green,blue),names_to = "color",values_to = "level")

# plot density of each, R,G and B
ggplot(img_tidy,aes(x=level,fill=color))+
  geom_density(alpha=0.7) + 
  scale_fill_manual(values=c("blue","green","red"))

# get all paint colors

max_colors <- max(bob_ross$num_colors)

# clean up strings of colors and return as a list
items2list <- function(items) {
  items <- str_remove_all(items, "\\\\r|\\\\n")
  sep_items <- strsplit(str_remove_all(items, "\\[|\\]|\\'"), ", ")
  return(sep_items)
}

# ----------------------------------------------
# look at the paints Bob uses
# make list columns out of colors
bob_ross_2 <- bob_ross %>% 
  mutate(color_name = items2list(colors)) %>% 
  mutate(color_hex = items2list(color_hex)) %>% 
  unnest(c(color_name,color_hex)) %>% 
  select(painting_index,painting_title,num_colors,color_name,color_hex) %>% 
  mutate(color_name = ifelse(color_hex=="#000000","Black",color_name)) %>% 
  mutate(color_name = ifelse(color_hex=="#FFFFFF","White",color_name))

paint_freq <- bob_ross_2 %>% 
  group_by(color_name,color_hex) %>% 
  count() %>% 
  group_by(color_hex) %>% 
  arrange(color_hex) %>% 
  mutate(n = sum(n)) %>% 
  ungroup() %>% 
  distinct(color_hex,.keep_all = TRUE) %>% 
  arrange(n) %>% 
  mutate(color_name = as_factor(color_name))

gg1 <- paint_freq %>% 
  ggplot(aes(color_name,n)) + geom_col(fill = paint_freq$color_hex) + 
  coord_flip() + 
  theme(plot.title = element_text(family = "serif",face = "italic")) + 
  labs(title = "The Paints Bob Uses",
       x = "Paint Name", y= "Times Used")

show_col(paint_freq$color_hex,labels = TRUE)

# ----------------------------------------------
# look at the resulting colors Bob gets

# make palette using kmeans
num_colors = 32

#assign each pixel to a cluster
set.seed(123)
tic()
km <-  img_list[c("red","green","blue")] %>% 
  kmeans(centers = num_colors, iter.max = 30)
toc()

centers <- as_tibble(km$centers) %>% 
  rowid_to_column(var = "cluster") %>% 
  rowwise() %>% 
  mutate(hue = rgb2hsv(red,green,blue)[1]) %>% 
  mutate(saturation = rgb2hsv(red,green,blue)[2]) %>% 
  mutate(value = rgb2hsv(red,green,blue)[3]) %>% 
  mutate(color_hex = rgb(red,green,blue,maxColorValue = 255)) %>% 
  # if we want to sort by color instead of frequency
  # arrange(hue,saturation,value) %>% 
  identity()

img_list <- img_list %>% 
  mutate(cluster=as.factor(km$cluster)) 

pal_ross <- centers$color_hex

show_col(pal_ross)

cluster_agg <- km$cluster %>% enframe(value = "color") %>% 
  count(color,name="count") %>% 
  mutate(proportion = count/sum(count)) %>% 
  arrange(count) %>% 
  mutate(color = as_factor(as.character(color)))

# if we want to sort by color instead of frequency
# levels(cluster_agg$color) <- as.character(centers$cluster)

gg2 <- cluster_agg %>% 
  ggplot(aes(color,proportion)) + geom_col(fill = pal_ross) + 
  scale_y_continuous(labels = scales::percent) + 
  theme(axis.text.y = element_blank()) + 
  coord_flip() +
  labs(title = "The Colors Bob Gets",
       y = "Proportion of Pixels", x = "Color") +
  theme(plot.title = element_text(family = "serif",face = "italic")) + 
  annotate("text",
           x = 10,y=.03,
           hjust = 0,
           size = 3,
           label = "1) Using 'Magick' package, scale every original image to 20%.
2) Reduce every image to 32 colors. This reduces total
            color space from 1.4mm to ~11,000.
3) Perform k-means cluster analysis to generate 32 clusters
           with an 'average' color for each cluster.")
gg2
# kmeans PCA of colors


# reduce data to unique colors to make plotting quicker
# smaller numbers shorten plotting time
# lets try 40,000
pixel_count = 50000
img_list_short <-img_list %>% 
  mutate(color_hex = rgb(red,green,blue,maxColorValue = 255)) %>% 
  distinct(color_hex,.keep_all = TRUE) %>% 
  slice_sample(n = pixel_count)


img_PCA<-prcomp(img_list_short[c("red","green","blue")])

var_expl <- round(img_PCA$sdev^2/sum(img_PCA$sdev^2)*100)
# plot derived colors against pc1 and pc2



gg3 <- autoplot(img_PCA, x=1,y=2,data = img_list_short, colour = "cluster",
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 10) +
  scale_color_manual(values=rgb(km$centers,maxColorValue = 255),guide="none")+
  theme_classic() + 
  theme(plot.title = element_text(family = "serif",face = "italic")) +
  labs(title = glue::glue("{var_expl[1]}% of Color Variation in Bob Ross Paintings\n   is Due to Luminosity"),
       subtitle = "Principal Component analysis of Colors",
       x = glue::glue("PC 1, {var_expl[1]}% Variance Explained\nInterpret as Luminance"),
       y = glue::glue("PC 2, {var_expl[2]}% Variance Explained"))

gg3
# in practice pc1 is luminosity while pc2 2 and 3 get into hue
# plot derived colors against pc2 and pc3
gg4 <- autoplot(img_PCA, x=2,y=3,data = img_list_short, colour = "cluster",
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 10) +
  scale_color_manual(values=rgb(km$centers,maxColorValue = 255),guide="none")+
  theme_classic() + 
  theme(plot.title = element_text(family = "serif",face = "italic")) + 
  labs(title = "PC2 and PC3 Create an Orthogonal Color Space",
       subtitle = "Principal Component analysis of Colors",
       x = glue::glue("PC 2, {var_expl[2]}% Variance Explained"),
       y = glue::glue("PC 3, {var_expl[3]}% Variance Explained"))


gg4
ggpubr::ggarrange(gg1,gg2,gg3,gg4,nrow = 2,ncol = 2)

# ----------------------------------------------------------------------------
# Bonus code
# make Bob Ross palettes after the idea of Wes Anderson palettes

# get the most frequent words in painting names
# and paintings that contain them in a list-column
paint_words <- bob_ross %>% 
  select(painting_index,painting_title) %>% 
  separate_rows(painting_title) %>% 
  rename(word = painting_title) %>% 
  mutate(word = tolower(word)) %>% 
  anti_join(stop_words) %>% 
  mutate(word = str_remove(word,"'s")) %>% 
  mutate(word = str_replace(word,"mt.","mountain")) %>% 
  left_join(select(bob_ross,painting_index,painting_title)) %>% 
  rename(title = "painting_title") %>% 
  select(title,word) %>% 
  group_by(word) %>% 
  nest() %>% 
  rename(titles = data) %>% 
  mutate(freq = nrow(pluck(titles,1))) %>% 
  arrange(desc(freq)) %>% 
  # "oval" is always linked with a more meaningful word so skip
  filter(word != "oval") %>% 
  ungroup()

top_paint_words <-paint_words %>% 
  slice_head(n=10)

# Build a low-color data frame of RGB of all images
max_colors = 8
print("Extracting RGB arrays from paintings.")
# Parallel processing to speed it up
# This is about half the time on my machine
plan(multisession,workers = 4)
tic()
img_list <- future_map_dfr(1:nrow(bob_ross),load_img_data,.progress = TRUE)
toc()
plan(sequential)

# get the hex color data from each painting

get_color_clusters <- function(img_titles){
  num_colors <- 8
  # this takes a list of image titles
  # and returns 8 key colors from each list
  gch <- function(img_title){
    img_list %>%
      filter(title == img_title) %>% 
      select(red,green,blue)
  }
  img_titles_l <- img_titles %>% 
    pull(title)
  km <- img_titles_l %>% 
    map_df(gch) %>% 
    # group_by(color_hex) %>%
    # count() %>% 
    # ungroup() %>% 
    # slice_max(order_by = n,n=10)
    kmeans(centers = num_colors, iter.max = 30)
  sort_by_lum <- as_tibble(km$centers) %>% 
    rowwise() %>%  
    mutate(luminance = rgb2hsv(red,green,blue)[3]) %>% 
    arrange(luminance) %>% 
    mutate(color_hex =  rgb(red,green,blue,maxColorValue = 255)) %>% 
    pull(color_hex)
  
  return(sort_by_lum)
}

top_colors <- top_paint_words$titles %>%
  map(get_color_clusters) %>% 
  set_names(nm=top_paint_words$word) %>% 
  enframe(name="word",value="top_colors_hex") %>% 
  right_join(top_paint_words)


# plot one Palette
plot_one<-function(pal_name){
  tmp <- top_colors %>% 
    unnest(top_colors_hex) %>% 
    filter(word==pal_name)
  g<- ggplot(tmp,aes(top_colors_hex,fill=top_colors_hex)) + geom_bar() + 
    scale_fill_manual(values=tmp$top_colors_hex,guide=F) +
    theme_void()+ggtitle(pal_name)
  return (g)
  
}

lapply(top_colors$word,plot_one) %>% 
  grid.arrange(grobs=.)

# ----------------------------------------------
# Word Cloud!
paint_words %>% 
  select(word,freq) %>% 
wordcloud2::wordcloud2(size = 0.8)

# # Use just one image
# 
# # just use one painting number 143
# img_index = 143
# img_list_single <- img_list %>%
#   filter(title == "Quiet Mountains River")
# 
# 
# 
# local_file <- paste0("img/bob_ross/", img_index, ".png")
# img_data <- magick::image_read(local_file)
# img_rgb <- as.raster(img_data) %>%
#   col2rgb(alpha = FALSE) %>%
#   t() %>%
#   as_tibble() %>%
#   mutate(title = "Quiet Mountains River")
# 
# print(img_data)
# img_data_32 <- img_data %>% image_quantize(max = 32)
# print(img_data_32)
# 
# img_tidy <- img_rgb %>%
#   pivot_longer(
#     cols = c(red, green, blue),
#     names_to = "color",
#     values_to = "level" )
# 
# # plot density of each, R,G and B
# ggplot(img_tidy, aes(x = level, fill = color)) +
#   geom_density(alpha = 0.7) +
#   scale_fill_manual(values = c("blue", "green", "red"))
# 
