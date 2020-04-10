#### An analysis of data on the Tour De France ####

## Author: Johnny Breen ##

#### Background ----

# The data for this script comes from Alastair Rushworth's brilliant 'tdf' package. All credit goes to Alastair for making this data available.

# The details on how to source this data can be found here: https://github.com/alastairrushworth/tdf

#### Preliminaries ----
library(tidytuesdayR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(lubridate)
library(repurrrsive)
library(scales)
library(stringr)

theme_set(theme_light(base_size = 11, base_family = "Arial"))

#### Data read ----

tuesdata <- tidytuesdayR::tt_load('2020-04-07')

winners <- tuesdata$tdf_winners
stages <- tuesdata$tdf_stages
stage_results <- tuesdata$stage_data

#### Analysis of winners ----

# The majority of riders have been of French nationality
winners %>%
  count(nationality, sort = T)

# The total distance travelled over time has decreased
winners %>%
  mutate(winning_year = year(start_date)) %>%
  ggplot(aes(winning_year, distance)) +
  geom_point(alpha = 0.60) +
  geom_smooth() +
  labs(x = "Edition year",
       y = "Distance travelled (km)",
       title = "Total distance travelled versus edition year",
       subtitle = "Riders are travelling shorter and shorter distances") +
  theme(plot.title = element_text(face = "bold"))

# However, the margin of winning has also got closer over time, indicating that races are becoming tighter
winners %>%
  mutate(winning_year = year(start_date)) %>%
  ggplot(aes(winning_year, time_margin)) +
  geom_point(alpha = 0.60) +
  geom_smooth(colour = "tomato2") +
  labs(x = "Edition year",
       y = "Time margin",
       title = "Margin of win versus edition year",
       subtitle = "Races are getting closer and closer") +
  theme(plot.title = element_text(face = "bold"))

# Contrary to expectations, one can win the Tour De France without winning all of the stages
winners %>%
  count(stage_wins, sort = T) %>%
  mutate(stage_wins = fct_reorder(as.factor(stage_wins), n)) %>%
  ggplot(aes(n, stage_wins)) +
  geom_point(aes(colour = n), alpha = 0.60, show.legend = F, size = 5) +
  geom_segment(aes(x = 0, xend = n, y = stage_wins, yend = stage_wins), lty = "dashed", colour = "darkgray") +
  scale_x_continuous(breaks = seq(0, 25, 5)) +
  scale_colour_continuous(low = "slateblue2", high = "tomato2") +
  labs(y = "Number of stages won",
       x = "Number of winners ",
       title = "Distribution of stages won across all Tour De France winners",
       subtitle = "Most winners of the coveted yellow jersey only win 1 to 4 stages",
       caption = "Source: 'tdf' package") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "darkgray"))

#### Analysis of stage results ----

# The average age of participants hasn't changed much over time
stage_results %>%
  group_by(year) %>%
  summarise(mean_age = mean(age, na.rm = T),
            count = n()) %>%
  ggplot(aes(year, mean_age)) + 
  geom_point(aes(alpha = count)) +
  geom_line() +
  expand_limits(y = 0)

#### Analysis of stages ----

# We can see from the below that most stages are 'Plain stages'
stages %>%
  count(Type, sort = T)

# The most difficult mountain stages are shown below
mountain_stages <- stages %>%
  filter(Type == "Stage with mountain(s)") %>%
  arrange(desc(Distance)) %>%
  transmute(stage_results_id = str_c("stage-", Stage),
            year = year(Date),
            distance = Distance,
            origin = Origin,
            destination = Destination) %>%
  slice(1:20)



  



