#####################
## FiveThirtyEight ##
#####################

## Riddler Classic ##
## 'Flipping your way to victory' ##

# Hypothesis: since the expected value of flipping coin A equates to that of coin B at any point in time, the optimal strategy is to flip coin B 
#             until one reaches a point at which continually flipping coin A will guarantee a win

## Date: 21 February 2020 ##

#### Preliminaries ----

library(tidyverse)

#### Input parameters ----

prob_A <- 0.5
prob_B <- 0.5

amount_A <- 1
amount_B <- 2

#### Functions ----


