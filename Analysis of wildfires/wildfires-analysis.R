

library(maps)
library(tidyverse)

tables <- read_html("https://en.wikipedia.org/wiki/List_of_wildfires") %>% 
  html_nodes("table") %>% 
  html_table(fill = TRUE)

theme_set(theme_light(base_size = 10, base_family = "Arial"))

wildfires_clean <- wildfires_raw %>% 
  mutate(area = str_extract(Size, pattern = ".+\\s(?=acres)"),
         area = as.numeric(str_remove_all(area, ",")),
         killed = str_extract(Notes, pattern = "(\\d|,)+(?=\\speople)"),
         killed = as.numeric(str_remove_all(killed, ","))) %>%
  select(-Size) %>%
  as_tibble()

wildfires_clean %>%
  ggplot(aes(x = Area, y = Year)) + 
  geom_jitter(aes(size = area), alpha = 0.50) + 
  coord_flip() +
  theme()

wildfires_clean %>%
  ggplot(aes(x = Year, y = area)) +
  geom_point()

wildfires_coords <- map_data("state")

wildfires_clean %>%
  mutate(Area = str_to_lower(Area)) %>%
  left_join(wildfires_coords, by = c("Area" = "region")) %>%
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(group = Area), colour = "gray50") +
  geom_point(aes(size = area))
  
