#### Analysis of BBC's most highly ranked rap artists ####

## Author: Johnny Breen ##
## Date: 17 April 2020 ##

library(tidyverse)
library(broom)
library(png)
library(grid)

polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

# There's always been more males involved in rap from the get-go
rankings %>% 
  count(year, gender) %>% 
  arrange(year) %>% 
  ggplot(aes(x = year, y = n, colour = gender)) + 
  geom_line()

# It seems like there's slighly more love from US critics for the 90s than any other period of time
europe <- c("UK", "Germany")
asia <- c("Japan", "India", "China", "New Zealand")
americas <- c("US", "Canada", "Colombia")
africa <- c("South Africa", "Nigeria", "Kenya")
russia <- c("Russian Federation")

polls %>% 
  mutate(critic_country_cat = case_when(critic_country %in% europe ~ "Europe",
                                        critic_country %in% asia ~ "Asia",
                                        critic_country %in% americas ~ "Americas",
                                        critic_country %in% africa ~ "Africa",
                                        critic_country %in% russia ~ "Russia")) %>%
  ggplot(aes(y = year, x = critic_country_cat, colour = critic_country_cat)) +
  geom_jitter(alpha = 0.50, width = 0.20) +
  coord_flip()

# Certain artists are good 'bang for their buck' - for instance, 'The Pharcyde' and 'Souls of Mischief'
png_raw <- readPNG("/Users/Johnny/Desktop/Data Science/R/TidyTuesdays/PNGs/rakim-png2.png")
png_clean <- rasterGrob(png_raw, interpolate = T)

rankings %>%
  group_by(artist) %>%
  summarise(total_points = sum(points),
            total_votes = sum(n),
            total_tracks = n()) %>%
  mutate(points_per_vote = total_points / total_votes) %>%
  filter(total_votes > 5) %>%
  arrange(desc(points_per_vote)) %>%
  head(10) %>%
  mutate(artist = fct_reorder(artist, points_per_vote)) %>%
  ggplot(aes(x = artist, y = points_per_vote, size = total_tracks)) +
  annotation_custom(grob = png_clean, xmin = 6, xmax = 8, ymin = 6, ymax = 6.50) +
  geom_point(colour = "orange") +
  coord_flip() +
  theme_light() +
  scale_size(guide = "none") +
  labs(x = NULL,
       y = "Average points per vote",
       title = "Top 10 rap artists of all time",
       subtitle = "The size of each point corresponds to the number of tracks registered by each artist") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold"))
  
  
  

