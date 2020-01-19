#### Analysis of 'bad passwords' ####

## Background ##

# The following script aims to analyse a dataset containing a series of so-called 'bad passwords'.

# The data is originally sourced from 'Information is Beautiful':
# https://docs.google.com/spreadsheets/d/1cz7TDhm0ebVpySqbTvrHrD3WpxeyE4hLZtifWSnoNTQ/edit#gid=21

## Load required packages ##
packages <- c("tidyverse", "broom", "extrafont", "reshape2")
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
  mutate(common = ifelse(common == 1, "Common", "Uncommon")) %>%
  modify_at(c("category", "length"), ~ as.factor(.)) %>%
  select(rank, password, category, common, online_crack_sec, offline_crack_sec, length, entropy)

## Analyse data ##

# Load fonts
extrafont::loadfonts()

# Set theme
theme_set(theme_light(base_size = 11, base_family = "Arial"))

entropy_avg <- mean(passwords_clean$entropy, na.rm = TRUE)
entropy_min <- min(passwords_clean$entropy, na.rm = TRUE)
entropy_by_commonality <- passwords_clean %>% 
  group_by(common) %>%
  summarise(mean_entropy = mean(entropy, na.rm = TRUE))

# Finding: the data provides information on the most popular 'worst passwords' in each password category.
#          They are shown below in the following plot - marvel at your fellow human beings and their creativity
most_popular_pwd_posns <- passwords_clean %>% 
  group_by(category) %>% 
  summarise(most_popular_posn = min(rank, na.rm = T))
most_popular_pwds <- passwords_clean %>%
  inner_join(most_popular_pwd_posns, by = c("rank" = "most_popular_posn"))

most_popular_pwds %>% 
  mutate(category.x = str_replace(category.x, "simple-", "")) %>%
  ggplot(aes(colour = category.x)) +
  geom_text(mapping = aes(x = 2.5, y = 2.5, label = password)) +
  facet_wrap(~category.x) +
  labs(x = NULL,
       y = NULL,
       title = "The worst (of the worst) passwords split by category",
       subtitle = "Marvel at the creativity of your fellow human beings...",
       caption = "Source: 'Information is Beautiful'") +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "grey20"),
        strip.text = element_text(face = "bold"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 8, colour = "gray50"))

# Finding: the number of common words appearing in your password does matter - in this case, the difference
#          in the mean cracking time is about 0.70 seconds, or about 4.4 times as long to crack. However
#          it's important not to overstate this fact. The difference here is being driven by outliers
#          which happen to consist of 'uncommon' words!
common_stats <- passwords_clean %>%
  group_by(common) %>%
  summarise(mean_entropy = mean(entropy, na.rm = T))

passwords_clean %>% 
  ggplot(aes(x = common, y = entropy, colour = common)) + 
  geom_jitter(alpha = 0.10, show.legend = F) +
  stat_summary(fun.y = mean, geom = "point", size = 5) +
  geom_segment(data = entropy_by_commonality, mapping = aes(x = common, xend = common, y = entropy_min, yend = mean_entropy), linetype = "dashed") +
  coord_flip() +
  labs(x = NULL,
       y = "Password strength ('Entropy')",
       title = "The effect of word commonality on password strength",
       subtitle = "Including uncommon words in your password can provide\nyou with an advantage",
       caption = "Source: 'Information is Beautiful'") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 8, colour = "gray50"),
        panel.grid = element_blank())

# Finding: nonetheless, it doesn't really matter what category of password you choose. It's slightly more important to
#          exclude common words from your password, irrespective of password category. But, 
#          the length of the password trumps everything

passwords_clean %>%
  modify_at("length", ~ as.numeric(as.character(.))) %>%
  mutate(length_bracket = ifelse(length <= 7, "<= 7 characters", "> 7 characters")) %>%
  group_by(length_bracket) %>%
  summarise(mean_secs = mean(offline_crack_sec, na.rm = T),
            count = n()) %>%
  ggplot(aes(x = length_bracket, y = mean_secs, size = count)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL,
       y = "Seconds to crack",
       size = "Count")
