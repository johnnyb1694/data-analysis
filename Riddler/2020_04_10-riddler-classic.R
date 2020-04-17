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
sims <- 1

# Basic plot of Poisson distribution (on a daily basis, according to a rate of 1 spam comment per day)
tibble(x = 0:10, p_x = dpois(x, 1)) %>%
  ggplot(aes(x, p_x)) +
  geom_point() +
  geom_line()

# Generation of simulations
set.seed(123)

simulate_trial <- function(l = 1, t = 3) {
  rexp(n = 100, rate = 1 / (l*t)) %>% 
    cumsum() %>% 
    keep(~ .x < t)
}

recurse <- function(trial_results, n) {
  
  if (n == 0) {
    
    tmp_results <- simulate_trial()
    recurse(trial_results = tmp_results, n = 1)
    
  } else {
    
    results_length <- length(trial_results) # e.g. we spotted 2 instances of spam comments
    
    for (i in seq_len(results_length)) {
      simulate_trial(l = 1, t = 3 - trial_results[[i]])
    }
  }

}




