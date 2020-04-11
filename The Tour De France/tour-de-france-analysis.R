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
library(gridExtra)

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
dist_plot <- winners %>%
  mutate(winning_year = year(start_date)) %>%
  ggplot(aes(winning_year, distance)) +
  geom_point(alpha = 0.60) +
  geom_smooth(colour = "orange") +
  labs(x = "Year",
       y = "Distance travelled (km)",
       title = "Change in the total distance travelled each year",
       subtitle = "Riders are travelling shorter and shorter distances overall",
       caption = "Source: 'tdf' package") +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "darkgray"),
        panel.grid = element_blank())

# However, the margin of winning has also got closer over time, indicating that races are becoming tighter
margin_plot <- winners %>%
  mutate(winning_year = year(start_date)) %>%
  ggplot(aes(winning_year, time_margin)) +
  geom_point(alpha = 0.60) +
  geom_smooth(colour = "tomato2") +
  labs(x = "Year",
       y = "Time margin",
       title = "Change in the margin of win each year",
       subtitle = "Races are getting closer and closer",
       caption = "Source: 'tdf' package") +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "darkgray"),
        panel.grid = element_blank()) 

# Aggregate plot of both the above plots on the same page
grid.arrange(dist_plot, margin_plot)

# Contrary to expectations, one can win the Tour De France without winning all of the stages
central <- count(winners, stage_wins, sort = T) %>% 
  summarise(n_median = median(n)) %>%
  pull()

arrows <- tibble(x_start = 5,
                 y_start = 1.75,
                 x_end = 7.5,
                 y_end = 0)

winners %>%
  count(stage_wins, sort = T) %>%
  mutate(above_median = ifelse(n > central, "Above average", "Below average")) %>%
  ggplot(aes(n, stage_wins)) +
  geom_point(aes(colour = above_median), show.legend = F, size = 5) +
  geom_segment(aes(x = central, xend = n, y = stage_wins, yend = stage_wins), colour = "darkgray", lty = "dotted") +
  scale_x_continuous(breaks = seq(0, 25, 5)) +
  scale_y_continuous(breaks = 0:8) +
  geom_vline(xintercept = central) +
  geom_curve(data = arrows, aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
             arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
             colour = "gray50", curvature = 0.3) +
  labs(y = "Number of stages won",
       x = "Number of winners",
       title = "Distribution of the number of stages won by each Tour De France winner",
       subtitle = "Most winners of the Tour De France only win 1 to 4 stages (in red)",
       caption = "Source: 'tdf' package") +
  annotate("text", x = 5, y = 2, size = 3, colour = "gray20", label = "Some winners don't even win a single stage!") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "darkgray")) +
  scale_colour_manual(values = c("tomato2", "orange"))

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
            winner = Winner,
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
  geom_hline(yintercept = min(top_10$distance), colour = "tomato2", lty = "dashed") +
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
        legend.position = "none") +
  scale_colour_manual(values = c("tomato2", "orange"))

# One thing that winners have in common is that they do well on the mountain stages
winner_stage_types <- stages %>% 
  inner_join(winners, by = c("Winner" = "winner_name")) %>% 
  count(Type, sort = T, name = "winner_count") %>%
  mutate(`Tour De France winner` = winner_count / sum(winner_count)) %>%
  select(-winner_count)

all_stage_types <- stages %>% 
  anti_join(winners, by = c("Winner" = "winner_name")) %>%
  count(Type, sort = T, name = "regular_count") %>%
  mutate(`Regular participant` = regular_count / sum(regular_count)) %>%
  select(-regular_count)

all_stage_types %>%
  left_join(winner_stage_types, by = "Type") %>%
  mutate(type_class = case_when(str_detect(Type, regex("mountain", ignore_case = T)) ~ "Mountain-based stage",
                                str_detect(Type, regex("time trial", ignore_case = T)) ~ "Time trial",
                                TRUE ~ "Other stage")) %>%
  group_by(type_class) %>%
  summarise(`Tour De France winner` = sum(`Tour De France winner`, na.rm = T),
            `Regular participant` = sum(`Regular participant`, na.rm = T)) %>%
  gather(key = "prop_type", value = "prop", 2:3) %>%
  mutate(type_class = fct_reorder(type_class, -prop)) %>%
  ggplot(aes(x = type_class, y = prop, fill = prop_type)) +
  geom_col(position = "dodge", width = 0.3) +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "",
       y = "Representation (%) in stage type",
       caption = "Source: 'tdf' package",
       fill = "Classification",
       title = "Distribution of participants across stage types",
       subtitle = "Tour De France winners tend to win stage types that rely on their individual strength") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = "darkgray")) +
  scale_fill_manual(values = c("orange", "tomato2"))
  



  



