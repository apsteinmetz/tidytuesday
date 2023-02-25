library(magick)
library(cluster)
library(stats)
library(scales) #just for for the show_col() function
library(tidyverse)
library(ggfortify)
library(ggpubr)
library(tidytuesdayR)


# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

# tuesdata <- tidytuesdayR::tt_load('2023-02-21')
# tuesdata <- tidytuesdayR::tt_load(2023, week = 8)

# bob_ross <- tuesdata$bob_ross

# Save the data.
# write_csv(
#   bob_ross,
#   here::here("data","bob_ross.csv")
# )
# bob_ross <- read_csv("data/bob_ross.csv")

# Or read in the data manually
bob_ross <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-21/bob_ross.csv')

# download the paintings
# for (img_index in 1:nrow(bob_ross)) {
#   print(bob_ross$img_src[img_index])
#   url <- bob_ross$img_src[img_index]
#   
#   # Define the local file name for the downloaded file
#   local_file <- paste0("img/bob_ross/",bob_ross$painting_index[img_index],".png")
#   
#   # Download the file using download.file()
#   download.file(url, local_file, mode = "wb")
# }  

# create RGB arrays
max_colors = 32
img_scale = "20%"
img_list <- list()
for (img_index in 1:nrow(bob_ross)) {
  print(paste(img_index,"/",nrow(bob_ross),bob_ross$painting_title[img_index]))
  local_file <- paste0("img/bob_ross/",bob_ross$painting_index[img_index],".png")
  img_data <- magick::image_read(local_file) %>%
    image_scale(img_scale)
  if (max_colors < Inf) img_data <- image_quantize(img_data, max = max_colors)
  img_rgb <- as.raster(img_data) %>%
    col2rgb(alpha=FALSE) %>%
    t() %>%
    as_tibble() %>%
    mutate(title = bob_ross$painting_title[img_index])
  img_list<-bind_rows(img_rgb,img_list)
}

# save(img_list,file="data/ross_img_data.rdata")
# load("data/ross_img_data.rdata") # "image_list" df

#make data tidy first
img_tidy <- img_list %>% 
  pivot_longer(cols = c(red,green,blue),names_to = "color",values_to = "level")

# plot density of each, R,G and B
ggplot(img_tidy,aes(x=level,fill=color))+
  geom_density(alpha=0.7) + 
  scale_fill_manual(values=c("blue","green","red"))

# downsample pixels
img_list_short <-img_list[sample(1:nrow(img_list),size = 1e05),]

# get all paint colors

max_colors <- max(bob_ross$num_colors)

# clean up strings of colors and return as a list
items2list <- function(items) {
  items <- str_remove_all(items, "\\\\r|\\\\n")
  sep_items <- strsplit(str_remove_all(items, "\\[|\\]|\\'"), ", ")
  return(sep_items)
}

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

# make palette using kmeans
num_colors = 32
#assign each pixel to a cluster
set.seed(123)
km <-  img_list_short[c("red","green","blue")] %>% 
  kmeans(centers = num_colors, iter.max = 30)

pal_ross <- km$centers %>% 
  rgb(maxColorValue = 255)

show_col(pal_ross)

gg2 <- km$cluster %>% enframe(value = "color") %>% 
  count(color,name="count") %>% 
  mutate(proportion = count/sum(count)) %>% 
  bind_cols(as_tibble(t(rgb2hsv(t(km$centers))))) %>% 
  arrange(count) %>% 
  mutate(color = as_factor(as.character(color))) %>% 
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


# kmeans PCA of colors

img_PCA<-prcomp(img_list_short[c("red","green","blue")])


img_list_short <- img_list_short %>% mutate(cluster=as.factor(km$cluster))

# plot derived colors against pc1 and pc2
gg3 <- autoplot(img_PCA, x=1,y=2,data = img_list_short, colour = "cluster",
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 10) +
  scale_color_manual(values=rgb(km$centers,maxColorValue = 255),guide="none")+
  theme_classic() + 
  theme(plot.title = element_text(family = "serif",face = "italic")) + 
  labs(title = "86% of Color Variation in Bob Ross Paintings\n   is Due to Luminosity",
       subtitle = "Principal Component analysis of Colors",
       x = "PC1, 86%, Interpet as Luminosity",
       y = "PC2, 12%, Interpret as Hue")

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
       x = "PC2, 12%",
       y = "PC3, 2% of Variance Explained")


ggpubr::ggarrange(gg1,gg2,gg3,gg4,nrow = 2,ncol = 2)
# ----------------------------------------------------------------------------
# Use just one image

# just use one painting number 143
img_index = 143
img_list_single <- img_list %>%
  filter(title == "Quiet Mountains River")



local_file <- paste0("img/bob_ross/", img_index, ".png")
img_data <- magick::image_read(local_file)
img_rgb <- as.raster(img_data) %>%
  col2rgb(alpha = FALSE) %>%
  t() %>%
  as_tibble() %>%
  mutate(title = "Quiet Mountains River")

print(img_data)
img_data_32 <- img_data %>% image_quantize(max = 32)
print(img_data_32)

img_tidy <- img_rgb %>%
  pivot_longer(
    cols = c(red, green, blue),
    names_to = "color",
    values_to = "level" )

# plot density of each, R,G and B
ggplot(img_tidy, aes(x = level, fill = color)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c("blue", "green", "red"))

