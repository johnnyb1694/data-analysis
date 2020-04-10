#### Analysis of Roman emperors ####

#### Preliminaries ####

## Input parameters ##

required_packages <- c("tidyverse", "ggsci", "extrafont", "lubridate")
data_url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv"

## Load packages ##

for (pkg in required_packages) {
  if (!pkg %in% installed.packages()) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

rm(pkg)

#### Import data ####

emp_raw <- readr::read_csv(file = data_url)

#### Clean data ####

# 1. Don't think we need 'name_full' (as 'name' suffices), 'notes' or 'verif_who'
# 2. We can engineer new features: 'lifespan' and 'length_of_reign'

emp_clean <- emp_raw %>%
  select(-c("name_full", "notes", "verif_who")) %>%
  mutate(birth_period = ifelse(death < birth, "BC", "AD"),
         reign_period = ifelse(reign_end < reign_start, "BC", "AD"),
         length_of_life = ifelse(birth_period == "BC", 
                           (birth - ymd("0000-01-01")) + (death - ymd("0000-01-01")),
                           (death - birth)) / 365.25,
         length_of_reign = ifelse(reign_period == "BC",(reign_start - ymd("0000-01-01")) + (reign_end - ymd("0000-01-01")),
                                  (reign_end - reign_start)) / 365.25)

#### Explore data ####

extrafont::loadfonts()

theme_set(theme_light(base_size = 10, base_family = "Arial"))

## Explore counts ##

# Each emperor lived longer than the average for the time
emp_clean %>% 
  ggplot(aes(x = length_of_life)) + 
  geom_histogram(bins = 30) + 
  expand_limits(x = c(0, 100)) + 
  geom_vline(xintercept = 35, linetype = "dashed") # 35 years of age was the average at the time

# Political upheaval was more common throughout the Gordian dynasty
library(ggridges)

emp_clean %>%
  add_count(dynasty) %>%
  filter(n > 1) %>%
  ggplot(aes(x = length_of_reign, y = dynasty, fill = dynasty)) +
  geom_density_ridges(aes(alpha = n), show.legend = FALSE) +
  scale_color_nejm() +
  theme(panel.grid = element_blank())

emp_clean %>%
  add_count(dynasty) %>%
  filter(n > 1) %>%
  ggplot(aes(x = dynasty, y = length_of_life, colour = dynasty)) +
  geom_point(alpha = 0.25, show.legend = FALSE) +
  stat_summary(fun.y = mean, geom = "point", size = 5, show.legend = FALSE) +
  scale_color_nejm() +
  coord_flip() +
  labs(y = "Length of life (years)",
       x = NULL) +
  theme(panel.grid = element_blank())



#### Archived code..#####
emp_clean %>%
  nest(-killer) %>%
  mutate(n_obs = map_int(data, ~ nrow(.))) %>%
  filter(n_obs > 1) %>%
  mutate(t_test = map(data, ~ t.test(.$length_of_reign))) %>%
  mutate(t_test_tidy = map(t_test, ~ broom::tidy(.))) %>%
  unnest(t_test_tidy) %>%
  mutate(conf.low = ifelse(conf.low < 0, 0, conf.low)) %>%
  ggplot() +
  geom_point(aes(x = estimate, y = killer)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, y = killer))

emp_clean %>%
  group_by(killer) %>%
  summarise(avg_reign = mean(length_of_reign, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(x = avg_reign, y = killer))

emp_clean %>% 
  ggplot(aes(x = length_of_life, y = length_of_reign)) + 
  geom_point(aes(colour = rise), show.legend = FALSE) + 
  facet_wrap(~rise)
  


