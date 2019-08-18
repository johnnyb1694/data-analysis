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
         lifespan = ifelse(birth_period == "BC", 
                           (birth - ymd("0000-01-01")) + (death - ymd("0000-01-01")),
                           (death - birth)) / 365.25)

#### Explore data ####

# 1. What 
