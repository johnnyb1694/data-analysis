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
  modify_at(c("category", "length"), ~ as.factor(.)) %>%
  select(rank, password, category, common, online_crack_sec, offline_crack_sec, length, entropy)

## Analyse data ##
passwords_conf <- passwords_clean %>% 
  nest(-category) %>% 
  mutate(t_test = map(data, ~ t.test(.$offline_crack_sec)), tidy_results = map(t_test, ~ tidy(.))) %>% 
  unnest(tidy_results) %>% 
  select(category, conf_low = conf.low, conf_mid = estimate, conf_high = conf.high) %>% 
  arrange(desc(conf_mid))

passwords_stats <- passwords_clean %>%
  group_by(category) %>%
  summarise(mean_crack_sec = mean(offline_crack_sec, na.rm = TRUE),
            var_crack_sec = var(offline_crack_sec, na.rm = TRUE),
            common_count = sum(common, na.rm = TRUE),
            mean_length = mean(as.numeric(length), na.rm = TRUE))

# Finding: whilst 'password-related' passwords are seemingly the best of all 'bad passwords', there 
#            is considerable uncertainty in this. In fact, the uncertainty appears to decrease 
#            directly in line with the average number of seconds required to crack a given password category
passwords_stats %>% 
  mutate(category = fct_reorder(category, mean_crack_sec)) %>% 
  ggplot(aes(mean_crack_sec, category)) + 
  geom_point(aes(alpha = 1 / var_crack_sec, colour = common_count), size = 4.5, show.legend = FALSE) + 
  labs(x = "Average seconds to crack", y = NULL) + 
  scale_colour_gradient(low = "slateblue2", high = "tomato2")

# Finding: so-called 'fluffy' passwords are the 'worst of the worst' passwords! This is primarily
#          because these passwords have a considerably smaller average length than those presented
#          in the other category of passwords
passwords_stats %>%
  arrange(mean_length)

# Finding: 
