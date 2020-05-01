#### Analysis of BBC's most highly ranked rap artists ####

## Author: Johnny Breen ##
## Date: 17 April 2020 ##

library(tidyverse)
library(broom)
library(png)
library(grid)
library(extrafont)
library(ggrepel)

polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

# Certain artists are good 'bang for their buck' - for instance, 'The Pharcyde' and 'Souls of Mischief'
extrafont::loadfonts()

top_10 <- rankings %>%
  group_by(artist) %>%
  summarise(total_points = sum(points),
            total_votes = sum(n),
            total_tracks = n()) %>%
  mutate(points_per_vote = total_points / total_votes) %>%
  filter(total_votes > 5) %>%
  arrange(desc(points_per_vote)) %>%
  head(10) %>%
  mutate(artist = fct_reorder(artist, points_per_vote)) 

one_hit_wonders <- top_10 %>% filter(total_tracks == 1)

one_hit_titles <- one_hit_wonders %>% inner_join(rankings, by = "artist") %>% select(artist, title, points_per_vote)

ggplot(top_10, aes(x = artist, y = points_per_vote)) +
  geom_segment(data = filter(top_10, total_tracks != 1), colour = "orange", mapping = aes(x = artist, xend = artist,
                                                 y = 6, yend = points_per_vote),
               lty = 5,
               alpha = 0.50) +
  geom_segment(data = filter(top_10, total_tracks == 1), colour = "tomato2", mapping = aes(x = artist, xend = artist,
                                                                                         y = 6, yend = points_per_vote),
               lty = 1,
               alpha = 1) +
  geom_point(mapping = aes(size = total_tracks, colour = (total_tracks == 1)), show.legend = F) +
  geom_label_repel(data = one_hit_titles, mapping = aes(label = title), size = 3, fill = "tomato2", colour = "white") +
  scale_colour_manual(values = c("orange", "tomato2")) +
  coord_flip() +
  theme_light() +
  scale_size(guide = "none") +
  labs(caption = "Source: BBC Music",
       x = NULL,
       y = "Average points per vote",
       title = "Top 10: Critics' choice",
       subtitle = "The size of each point corresponds to the number of tracks for each artist.\nLegendary one-hit classics are shown in red.") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 9),
        plot.background = element_rect(fill = "gray10"),
        panel.background = element_rect(fill = "gray40"),
        text = element_text(colour = "white", family = "Verdana"),
        axis.text = element_text(colour = "white", size = 8.5),
        axis.title.x = element_text(face = "bold"),
        plot.caption = element_text(colour = "gray60")
        )
  
  
  

