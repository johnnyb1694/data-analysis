#### Analysis of Roman emperors ####

#### Preliminaries ####

## Input parameters ##

required_packages <- c("tidyverse", "ggsci", "extrafont")
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

#### Explore data ####

