#### Analysis of medical costs dataset ####

# The data relied upon here is broadly in line with what can be found at this link:
# https://www.kaggle.com/mirichoi0218/insurance

# Nonetheless, I have adapted the dataset for the purposes of illustrating issues that crop up in 
# data science within an actuarial (and also some general) contexts.

#### Load libraries ####
library(tidyverse) # we mainly rely on dplyr and ggplot2 here
library(glmnet)
library(magrittr)
library(broom)

#### Load data ####
data_root <- "/Users/Johnny/Desktop/Data Science/R/Data Analyses in R/Medical Cost Analysis"
ins_raw <- read_csv(paste(data_root, "insurance.csv", sep = "/"))

#### Pre-process data ####

## Task 1: replace the inconvenient variable names to allow for easier EDA
old_names <- c("IS_SMOKER_OR_NOT", "IS_GENDER_MALE_OR_FEMALE")
new_names <- c("smoker", "gender")

colnames(ins_raw)[old_names] <- new_names


## Task 2: convert any factors which have been interpreted as characters to factors
toFactor <- function (df, cols_to_fct) {
  df[cols_to_fct] <- lapply(df[cols_to_fct], function(xx) factor(xx)) %>%
    as.tibble()
  return(df)
}
ins_df <- toFactor(ins_df, c("sex", "smoker", "region"))

theme_set(theme_light())

# plot of smoker distribution
ggplot(ins_df, aes(smoker, fill = smoker)) +
  geom_bar(width = 0.25, show.legend = FALSE) +
  labs(x = "Smoker Status",
       y = "Count",
       title = "Smoker Plot",
       subtitle = "Expected class imbalance")

# plot of charge density for smokers / non-smokers
ggplot(ins_df, aes(charges, colour = smoker)) +
  geom_density() +
  facet_wrap(~smoker) +
  labs(title = "Smoker vs non-smoker charge distribution",
       subtitle = "Smokers, as expected, receive overall higher charges",
       x = "Charge",
       y = "Density")

# plot of charge density for different numbers of children
ggplot(ins_df, aes(charges, colour = as.factor(children))) +
  geom_density() +
  facet_wrap(~children) +
  labs(title = "Number of children charge distribution",
       subtitle = "No clear differentiation between different children classes",
       x = "Charge",
       y = "Density", 
       colour = "Child No.")

# plot of age against charges including facets: region and bmi
ggplot(ins_df, aes(bmi, charges, colour = region)) +
  geom_point(position = "jitter") +
  facet_wrap(~region)

# basic modelling
ins_df %<>%
  mutate(ID = row_number())

# Set up training and test data
train_df <- ins_df %>%
  sample_frac(0.80)
test_df <- ins_df %>%
  filter(!(ID %in% train_df$ID))

# Train a simple LASSO model on the training data
glm_x <- train_df %>%
  select(-charges, -ID) %>%
  data.matrix()
glm_y <- train_df %>%
  pull(charges)
lasso_fit <- cv.glmnet(x = glm_x, y = log(glm_y))
plot(lasso_fit)

# Experiment with results
x_test <- test_df %>%
  select(-charges, -ID) %>%
  data.matrix()
y_test <- test_df %>%
  pull(charges) %>%
  log()
y_pred <- predict(lasso_fit, x_test)
glm_results <- test_df %>%
  mutate(charges_pred = exp(y_pred)) %>%
  mutate(residual = abs(charges - charges_pred))

# Plot actual versus predicted and iterate over model
glm_results %>%
  ggplot(aes(charges, charges_pred, colour = residual)) +
  geom_point(position = "jitter") +
  geom_text(data = filter(glm_results, residual > 15000), 
            mapping = aes(label = ID), 
            size = 2.5,
            vjust = -1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_colour_gradient(low = "blue", high = "red") +
  labs(x = "Actual",
       y = "Predicted", 
       title = "Plot of actual versus predicted",
       subtitle = "Look for the red-coloured IDs in the data - these are mis-estimated",
       colour = "Absolute Error")
