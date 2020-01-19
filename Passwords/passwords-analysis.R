#### Analysis of 'bad passwords' ####

## Background ##

# The following script aims to analyse a dataset containing a series of so-called 'bad passwords'.

# The data is originally sourced from 'Information is Beautiful':
# https://docs.google.com/spreadsheets/d/1cz7TDhm0ebVpySqbTvrHrD3WpxeyE4hLZtifWSnoNTQ/edit#gid=21

## Load required packages ##
packages <- c("tidyverse", "broom")
for (pkg in packages) {
  if (!pkg %in% installed.packages()) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

## Read data ##
passwords_url <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv'
common_words_url <- 'https://raw.githubusercontent.com/first20hours/google-10000-english/master/google-10000-english-no-swears.txt'
passwords_raw <- read_csv(file = passwords_url)
common_words_raw <- read_csv(file = common_words_url, col_names = FALSE)

## Pre-process data ##
common_words <- pull(common_words_raw)

check_if_common <- function(x) {
  if (x %in% common_words) {
    return(1)
  } else {
    return(0)
  }
}

passwords_clean <- passwords_raw %>%
  na.omit() %>% # there are 7 rows in which every single field value is entirely 'NA'
  mutate(length = str_length(password),
         online_crack_sec = case_when(time_unit == "years" ~ value * 365 * 24 * 60 * 60,
                                      time_unit == "months" ~ value * 30 * 24 * 60 * 60,
                                      time_unit == "weeks" ~ value * 7 * 24 * 60 * 60,
                                      time_unit == "days" ~ value * 24 * 60 * 60,
                                      time_unit == "hours" ~ value * 60 * 60,
                                      time_unit == "minutes" ~ value * 60,
                                      TRUE ~ value),
         # I'm using the alphanumeric character system as my basis
         entropy = ifelse(str_detect(password, "\\d"), 
                          length * log(36, base = 2),
                          length * log(26, base = 2)),
         common = map_dbl(password, ~ check_if_common(.))) %>% 
  modify_at(c("category"), ~ as.factor(.)) %>%
  select(rank, password, category, common, online_crack_sec, offline_crack_sec, length, entropy)

## Analyse data ##

passwords_stats <- passwords_clean %>%
  group_by(category) %>%
  summarise(mean_crack_sec = mean(offline_crack_sec, na.rm = TRUE),
            median_crack_sec = median(offline_crack_sec, na.rm = TRUE),
            var_crack_sec = var(offline_crack_sec, na.rm = TRUE),
            count = n(),
            common_count = sum(common, na.rm = TRUE),
            mean_length = mean(as.numeric(length), na.rm = TRUE))

# Finding: length matters, a lot!
passwords_clean %>%
  mutate(length_bracket = ifelse(length <= 7, "<= 7 characters", "> 7 characters")) %>%
  group_by(length_bracket) %>%
  summarise(mean_secs = mean(offline_crack_sec, na.rm = T),
            count = n()) %>%
  ggplot(aes(x = length_bracket, y = mean_secs, size = count)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL,
       y = "Seconds to crack")

# Finding: the number of common words appearing in your password matters too - in this case, the difference
#          in the mean cracking time is about 0.70 seconds, or about 4.4 times as long to crack
passwords_clean %>%
  group_by(common) %>%
  summarise(mean_secs = mean(offline_crack_sec, na.rm = T))

# Finding: you might assume that password-related passwords would be the worst. Indeed, there
#          were some funny password-related passwords cropping up in this dataset. Just take a look!


# Finding: nonetheless, it doesn't really matter what category of password you choose. It's more important to
#          exclude common words from your password, irrespective of password category
