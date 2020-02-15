#### Analysis of data from Spotify ####

## Load required packages ##
packages <- c("tidyverse", "broom", "extrafont", "ggwordcloud", "tidytext")
for (pkg in packages) {
  if (!pkg %in% installed.packages()) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

## Read data ##
spotify_raw <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

## Pre-process data ##

# Let's look at what range of time we're working with
spotify_clean <- spotify_raw %>%
  separate(col = track_album_release_date, into = c("release_year", "release_month", "release_day"), sep = "-", remove = FALSE) %>%
  mutate(duration_mins = duration_ms / (1000 * 60),
         remix = ifelse(str_detect(track_name, "Remix"), 1, 0))

spotify_2019 <- spotify_clean %>% 
  filter(release_year == 2019)
  
## Explore data ##

# Distribution of songs by year - most of the data resides in 2019
spotify_clean %>% 
  count(release_year) %>% 
  arrange(desc(n))

# Is there a trend in duration over time? Songs seem to be increasing in duration overall although there has been a 
# sharp drop-off in recent years
spotify_clean %>%
  group_by(release_year) %>%
  summarise(avg_duration = mean(duration_mins, na.rm = TRUE), count = n()) %>%
  ggplot(aes(x = as.integer(release_year), y = avg_duration)) +
  geom_point(alpha = 0.50, aes(size = count)) +
  geom_line()

# Do we have any seasonality present? Songs seem to get a bit more popular towards the end of the year...
spotify_clean %>%
  group_by(release_month) %>%
  summarise(avg_popularity = mean(track_popularity, na.rm = TRUE), count = n()) %>%
  ggplot(aes(x = as.numeric(release_month), y = avg_popularity)) +
  geom_point(alpha = 0.50, aes(size = count)) + 
  geom_line()

## Principal Components Analysis (PCA) ##

# Let's quickly pre-process the data
spotify_numerics <- spotify_clean %>%
  select_if(is.numeric) %>%
  select(-track_popularity, -remix)

spotify_pca_data <- spotify_numerics %>%
  as.matrix() %>%
  scale() %>% # centers and scales each respective covariate
  as.data.frame()

# Now let's apply PCA then summarise the results
spotify_pca <- prcomp(spotify_pca_data)
summary(spotify_pca)
