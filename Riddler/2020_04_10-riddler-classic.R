## Riddler Classic ##

## Date: 2020-04-10 ##
## Headline: how many spammers should I expect on my posts? ##

## Notes ##

# - Note that the variable we are measuring in this context is the number of 'spam' comments per day
# - Given that spam comments could occur at *any* point in time, within a given day, the Poisson distribution is the most appropriate choice of model
# - We are informed that spam comments occur at a rate of 1 per day. Thus, we know the average time between events but we do not know the exact timing of events
# - Therefore, the number of spam comments registered per day follows a Poisson process

## Background ##

# Poisson processes: https://www.probabilitycourse.com/chapter11/11_1_2_basic_concepts_of_the_poisson_process.php

# Preliminaries ----
library(tidyverse)

theme_set(theme_light(base_family = "Arial", base_size = 10))

# Calculations ----

# Input parameters
l <- 1
t <- 3
shape <- l*t
sims <- 1

# Basic plot of Poisson distribution (on a daily basis, according to a rate of 1 spam comment per day)
tibble(x = 0:10, p_x = dpois(x, l)) %>%
  ggplot(aes(x, p_x)) +
  geom_point() +
  geom_line()

# Generation of one simulation
set.seed(123)

rpois(n = sims, lambda = shape) %>%
  rpois(n = ., lambda = shape)
