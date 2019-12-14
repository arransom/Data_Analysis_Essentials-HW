# Data Analysis Essentials
# Lesson 5 - Creation Functions for Exploratory Data Analysis
# Date: 12/08/2019

# Load Packages ----------------------------------------------------------------
library(stringr) # stringr
library(tidyverse) # tidyverse

# Write functions to create and save consistent plots --------------------------

# Write a function to name and save plot
plot_save <- function(filename) {
  filename %>% ggsave()
}

# Write a function to create consistant plots
# Define input variables and default parameters 
gg_explore <- function(data, x_col, y_col, z_col,
                       param_title = str_c(y_col, " by ", x_col),
                       param_subtitle = str_c("Row count: ", data %>% nrow()),
                       param_alpha = 0.5,
                       param_method = "lm",
                       param_se = FALSE) {
  
  # Generate an exploratory plot and save it to the environment as "plt"
  plt <- data %>%
    ggplot(mapping = aes_string(x = x_col, y = y_col, color = z_col)) +
    geom_jitter(alpha = param_alpha) + 
    geom_smooth(method = param_method, se = param_se) +
    ggtitle(param_title, subtitle = param_subtitle) 
  
  # Display the Plot
  plt %>% print()
  
  # save plot to file
  str_c(x_col, "vs", y_col, ".png") %>% plot_save()
}
  
# Test the function using the mtcars dataset
gg_explore(df_mtcars, "disp", "mpg", "cyl")
glimpse(mtcars)
df_mtcars <- mtcars %>%
  mutate(cyl = as.factor(cyl),
         gear = as.factor(gear))


# Explore the Data -------------------------------------------------------------

# glimpse()
storms %>% glimpse()

# head() & tail()
head(storms, 10)
tail(storms, 10)

# Display help on dataset
?storms


# Process the data -------------------------------------------------------------

# Save the dataframe as df_storms, removing columns that will not be used
# and performing data type conversions 
df_storms <- storms %>%
  select(-(12:13)) %>%
  mutate(year = as.integer(year),
         month = as.integer(month),
         hour = as.integer(hour))
