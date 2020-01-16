#### Analysis of 'bad passwords' ####

## Background ##

# The following script aims to analyse a dataset containing a series of so-called 'bad passwords'.

# The data is originally sourced from 'Information is Beautiful':
# https://docs.google.com/spreadsheets/d/1cz7TDhm0ebVpySqbTvrHrD3WpxeyE4hLZtifWSnoNTQ/edit#gid=21

## Load required packages ##
packages <- c("tidyverse")
for (pkg in packages) {
  if (!pkg %in% installed.packages()) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

## Read data ##
data_url <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv'
passwords_raw <- read_csv(file = data_url)

## Pre-process data ##
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
         entropy = length * log(36, base = 2)) %>% # I'm using the alphanumeric character system as my basis
  select(rank, password, category, online_crack_sec, offline_crack_sec, length, entropy)

## Analyse data ##

