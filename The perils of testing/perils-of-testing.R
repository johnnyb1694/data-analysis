#### Why 'testing' requires 'testing' ####

# The problem ----

# You've been feeling under the weather for more than a couple of days and things don't seem to be improving. 
# So, you pluck up the courage to visit your local doctor. He tells you that it's probably just an innocuous
# virus but he decides to run a series of tests on you regardless, 'just to be safe'. 

# The next day you receive your results: your doctor informs you that you have tested positive
# for a very serious disease that affects only 5% of the global population.

# "The test", your doctor informs you, "is 99% accurate and it's only wrong 5 times out of 100 on otherwise healthy patients."

# You fall to your knees: "99% accurate? It's got to be right, surely?"

# ... Or is it?

# Preliminaries ----
req_pkgs <- c("tidyverse", "scales")

for (pkg in req_pkgs) {
  if (!pkg %in% installed.packages()){
    install.packages(pkg, dependencies = T)
  }
  library(pkg, character.only = T)
}

theme_set(theme_light())

# Input parameters ----

# Under the following assumptions, the posterior probability of being positive is only around ~ 51.03%
disease_coverage <- 0.05
test_accuracy <- 0.99
false_positive_rate <- 0.05

# Calculation ----

compute_posterior <- function(p_A, p_BA, p_BAc) {
  p_Ac <- 1 - p_A
  
  (p_A * p_BA) / ((p_A * p_BA) + (p_BAc * p_Ac))
}

compute_posterior(p_A = disease_coverage, p_BA = test_accuracy, p_BAc = false_positive_rate)

# Visual explanation ----

# Imagine that our population consists of 10,000 individuals. This means that there are 500 individuals carrying 
# the fictitious disease in our population. We apply our test to our entire population.

# Given the parameters of the test, we end up with 495 individuals being correctly identified as carrying the virus.
# In addition, we end up falsely identifying 500 individuals as positive. 

# As a result, we end up with 995 individuals who have all tested positive, yet only 495 of these individuals are
# indeed positive. It should be clearer to you now as to why our actual probabilty of being positive is lower
# than we first imagined.

# Sensitivity analysis ----

# In fact, the posterior probability is incredibly sensitive to the false positive rate (which is directly related
# to the 'specificity' of the test i.e. the probability of accurately identifying negative patients)
sensitivity_fp_rate <- tibble(false_positive_sequence = seq(from = 0.01, to = 0.15, by = 0.01), 
                        posterior_prob = compute_posterior(p_A = disease_coverage,
                                                           p_BA = test_accuracy,
                                                           p_BAc = false_positive_sequence))

sensitivity_fp_rate %>%
  ggplot(aes(x = false_positive_sequence,
             y = posterior_prob)) +
  geom_point(aes(colour = posterior_prob, size = posterior_prob), show.legend = F) +
  geom_line(alpha = 0.75, show.legend = F) +
  scale_colour_continuous(low = "tomato2", high = "slateblue1") +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(0, 1.0, 0.20)) +
  scale_x_continuous(labels = scales::percent_format(),
                     breaks = seq(0.01, 0.15, 0.02)) +
  expand_limits(y = 0) +
  labs(x = "False positive rate",
       y = "Probability of being disease carrier",
       title = "Probability of carrying disease versus false positive rate",
       subtitle = "Tests with a low specificity tend to misdiagnose many healthy cases")

# However, another important factor is how prevalent the underlying disease is in the population.
# The more common it is, the less chance we have of identifying false positive cases.
sensitivity_coverage <- tibble(coverage_sequence = seq(from = 0.01, to = 0.15, by = 0.01), 
                              posterior_prob = compute_posterior(p_A = coverage_sequence,
                                                                 p_BA = test_accuracy,
                                                                 p_BAc = false_positive_rate))

sensitivity_coverage %>%
  ggplot(aes(x = coverage_sequence,
             y = posterior_prob)) +
  geom_point(aes(colour = posterior_prob, size = posterior_prob), show.legend = F) +
  geom_line(alpha = 0.75, show.legend = F) +
  scale_colour_continuous(low = "tomato2", high = "slateblue1") +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(0, 1.0, 0.20)) +
  scale_x_continuous(labels = scales::percent_format(),
                     breaks = seq(0.01, 0.15, 0.02)) +
  expand_limits(y = 0) +
  labs(x = "Prevalence of disease (amongst population)",
       y = "Probability of being disease carrier",
       title = "Probability of carrying disease versus disease prevalence",
       subtitle = "Tests aimed at identifying common diseases are generally more reliable")
