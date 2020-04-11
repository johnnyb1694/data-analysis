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
library(ggrepel)

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

# The most difficult mountain stages are shown below in descending order of difficulty
mountain_stages <- stages %>%
  filter(str_detect(Type, regex("Mountain", ignore_case = T))) %>%
  arrange(desc(Distance)) %>%
  transmute(stage_results_id = str_c("stage-", Stage),
            year = year(Date),
            distance = Distance,
            journey = str_c(Origin, " to ", Destination, ", ", year, " (", distance, "km)"))

most_difficult_by_year <- mountain_stages %>%
  group_by(year) %>%
  summarise(max_distance = max(distance, na.rm = T))

most_difficult_by_year_all <- mountain_stages %>%
  left_join(most_difficult_by_year, by = "year") %>%
  mutate(most_difficult = ifelse(distance == max_distance, T, F)) %>%
  filter(most_difficult) %>%
  select(-max_distance, -most_difficult)

top_10 <- most_difficult_by_year_all %>%
  arrange(desc(distance)) %>%
  slice(1:10)

# This following plot is quite striking: there were some very tricky mountain stages in the early 1900s
# Of course, we've also seen that the overall distance covered across the whole tour was also very high back then
# All of this means that the Tour De France has always been an extremely difficult event to compete in
mountain_plot_data <- most_difficult_by_year_all %>%
  mutate(rank = ifelse(journey %in% top_10$journey, "Inside top 10", "Outside top 10")) 

ggplot(data = mountain_plot_data, mapping = aes(year, distance)) +
  geom_hline(yintercept = min(top_10$distance), colour = "red", lty = "dashed", alpha = 0.50) +
  geom_point(aes(colour = rank, alpha = distance)) +
  geom_label_repel(aes(label = journey), data = mountain_plot_data %>% filter(rank == "Inside top 10") %>% arrange(desc(distance)) %>% slice(1:2),
                   size = 3) +
  labs(title = "The most difficult mountain stages (shown in red), split by year",
       subtitle = "Back in the early 1900s, riders dealt with extraordinarily difficult mountain stages",
       x = "Year",
       y = "Distance (km)",
       caption = "Source: 'tdf' package") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "darkgray"),
        legend.position = "none")

# Next, we shall investigate the average speed sustained by the winners of these difficult mountain stages

# Also, let's look at how the number of stages has increased over time
stages %>% 
  mutate(year = year(Date)) %>% 
  count(year, sort = T) %>%
  ggplot(aes(x = year, y = n)) +
  geom_point()


  



