#### Analysis of data from Spotify ####

## Load required packages ##
packages <- c("tidyverse", "broom", "extrafont", "ggwordcloud")
for (pkg in packages) {
  if (!pkg %in% installed.packages()) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

## Read data ##
spotify_raw <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

## Explore data ##

# Relationship between the duration of the song and its popularity - not much in this...
spotify_raw %>% 
  ggplot(aes(duration_ms, track_popularity)) + geom_point(alpha = 0.20) +
  facet_wrap(~playlist_genre)
