library(tidyverse)
library(magrittr)

root_path <- "/Users/Johnny/Desktop/Data Science/R/Data Analyses in R/Insurance Data Analysis"
severity_threshold <- 15000

ins_raw <- read_csv(file = paste(root_path, "insurance.csv", sep = "/"))

ins_by_region <- ins_raw %>%
  mutate(severity = ifelse(charges > severity_threshold, 1, 0)) %>%
  group_by(region) %>%
  summarise(n_severe = sum(severity, na.rm = TRUE), n_region = n()) %>%
  ungroup() 
    
compute_ll <- function(alpha, beta) {
  k <- ins_by_region$n_severe
  n <- ins_by_region$n_region
  -sum(VGAM::dbetabinom.ab(x = k, size = n, shape1 = alpha, shape2 = beta, log = TRUE))
}

ll_estimate <- stats4::mle(minuslogl = compute_ll, 
                           start = list(alpha = 1, beta = 1), 
                           method = "L-BFGS-B",
                           lower = c(0.01, 0.01))

ab <- VGAM::coef(ll_estimate)

alpha0 <- ab[[1]]
beta0 <- ab[[2]]

ins_by_region_wstats <- ins_by_region %>%
  bind_rows(tibble(region = "central", n_severe = 10, n_region = 20)) %>% # fictitious line to illustrate the point
  mutate(avg_severity = n_severe / n_region,
         avg_severity_eb = (n_severe + alpha0) / (n_region + alpha0 + beta0),
         alpha1 = alpha0 + n_severe,
         beta1 = beta0 + n_region - n_severe,
         eb_low = qbeta(p = 0.025, shape1 = alpha1, shape2 = beta1),
         eb_high = qbeta(p = 0.975, shape1 = alpha1, shape2 = beta1))

regional_data <- crossing(x = seq(0.10, 0.40, 0.0001),
                          region = c("northeast", "northwest", "southeast", "southwest", "central"))

theme_set(theme_light(base_size = 10, base_family = "Arial"))

ins_by_region_wstats %>%
  left_join(regional_data, by = c("region" = "region")) %>%
  mutate(density = dbeta(x, alpha1, beta1),
         region = str_to_title(region)) %>%
  ggplot(aes(x = x, y = density, colour = region)) +
  geom_line(alpha = 0.75) +
  stat_function(fun = function (xx) dbeta(xx, alpha0, beta0), 
                lty = 2, 
                colour = "black") +
  scale_x_continuous(breaks = seq(0.10, 0.40, 0.10),
                     labels = scales::percent_format()) +
  labs(x = "Average incidence of large loss claims",
       y = "Density",
       colour = "Region",
       title = "Bayesian analysis of large loss claims",
       subtitle = "The dotted curve indicates the prior distribution of the incidence of large loss claims",
       caption = "Source: Kaggle") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 9),
        plot.caption = element_text(colour = "gray50"))
  



