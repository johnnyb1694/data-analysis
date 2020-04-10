#### Primer in multi-level modelling ####

## Data source: https://www.apa.org/science/about/psa/2017/01/multilevel-modelling 

## The data above contains repeated measurements on three individuals. They participate in an experiment 
## in which they are presented with names of different colours in non-matching ink (e.g. 'red' printed in the colour 'green'). 
## The candidates are instructed to respond with the name of the ink colour rather than the name printed on 
## the card. 

## We are interested in whether the language of instruction affects the response time (e.g. Spanish vs English)

#### Load packages ####
library(tidyverse)
library(rvest)
library(xml2)
library(magrittr)
 
#### Read in data ####
url <- "https://www.apa.org/science/about/psa/2017/01/multilevel-modelling"

reaction_time_df <- read_html(url) %>%
  html_table() %>%
  .[[1]]

#### Clean data ####

column_names <- reaction_time_df[1,,drop = TRUE] %>%
  unlist(use.names = FALSE)

names(reaction_time_df) <- column_names

reaction_time_df %<>%
  .[-1,]

reaction_time_clean <- reaction_time_df %>%
  transmute(Subject,
            Language = `Response language`,
            Response = as.integer(`Time (in ms)`)) %>%
  as_tibble() %>%
  group_by(Subject, Language) %>%
  mutate(Measurement = as.factor(row_number()))

#### Analyse data ####
reaction_time_clean %>%
  ggplot(aes(x = Measurement, y = Response)) +
  geom_point(aes(colour = Language, shape = Language)) +
  facet_wrap(~Subject) +
  theme_light() +
  labs(x = "Measurement",
       y = "Response (ms)")

## Let's fit a simplistic linear regression model
reaction_lm <- lm(Response~Language, data = reaction_time_clean)

## Note that this model creates an intercept of ~ 622 and a slope of ~ -25
summary(reaction_lm)

reaction_coef <- reaction_lm$coefficients

## Let's plot the resultant coefficients on our graph
intercept_coef <- reaction_coef[1]
slope_coef <- reaction_coef[2]

reaction_time_clean %>%
  ggplot(aes(x = Measurement, y = Response)) +
  geom_point(aes(colour = Language, shape = Language)) +
  geom_abline(intercept = intercept_coef, slope = slope_coef, linetype = "dashed") + 
  facet_wrap(~Subject) +
  theme_light() +
  labs(x = "Measurement",
       y = "Response (ms)")
  
  
  


