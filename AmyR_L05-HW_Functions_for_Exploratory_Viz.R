# Data Analysis Essentials
# Lesson 5 - Creation Functions for Exploratory Data Analysis
# Date: 12/08/2019

# Load Packages ----------------------------------------------------------------
library(stringr) # stringr
library(tidyverse) # tidyverse

# Write Function to create consistent plots ------------------------------------

# Define input variables and default parameters 
gg_explore <- function(data, x_col, y_col, 
                       param_title = str_c(y_col, " by ", x_col),
                       param_subtitle = str_c("Row count: ", data %>% nrow()),
                       param_alpha = 0.5,
                       param_method = "lm",
                       param_se = FALSE, 
                       param_dot_color = "blue",
                       param_line_color = "orange") {
  # Generate an exploratory plot and save it to the environment as "plt"
  plt <- data %>% ggplot(mapping = aes_string(x = x_col, y = y_col)) +
    geom_jitter(alpha = param_alpha, color = param_dot_color) + 
    geom_smooth(method = param_method, se = param_se, color = param_line_color) +
    ggtitle(param_title, subtitle = param_subtitle) 
  # Display the Plot
  plt %>% print()
}

# Test the function using the mtcars dataset
gg_explore(mtcars, "disp", "mpg")
