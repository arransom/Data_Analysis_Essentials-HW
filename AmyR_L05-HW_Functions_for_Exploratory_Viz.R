# Data Analysis Essentials
# Lesson 5 - Creation Functions for Exploratory Data Analysis
# Date: 12/08/2019

# Set Working Directory --------------------------------------------------------
setwd("C:/Users/arran/Dropbox/Data Analytics/Data Analysis Essentials/Data_Analysis_Essentials-HW_Output")

# Load Packages ----------------------------------------------------------------
library(stringr) # stringr
library(tidyverse) # tidyverse

# Write functions to create and save consistent plots --------------------------

# plot_save: function to name and save plots
plot_save <- function(filename) {
  filename %>% ggsave()
}

# gg_explore: create consistant scatterplots with variables mapped to the x and 
# y axes


# Define input variables and default parameters 
gg_explore <- function(data, x_col, y_col,
                       param_title = str_c(y_col, " by ", x_col),
                       param_subtitle = str_c("Row count: ", data %>% nrow()),
                       param_alpha = 0.5,
                       param_method = "lm",
                       param_se = FALSE,
                       param_line_color = "purple",
                       param_dot_color = "blue") {
  
  # Generate an exploratory plot and save it to the environment as "plt"
  plt <- data %>%
    ggplot(mapping = aes_string(x = x_col, y = y_col)) +
    geom_jitter(alpha = param_alpha, color = param_dot_color) + 
    geom_smooth(method = param_method, se = param_se, color = param_line_color) +
    ggtitle(param_title, subtitle = param_subtitle) 
  
  # Display the Plot
  plt %>% print()
  
  # save plot to file
  str_c(x_col, "vs", y_col, ".png") %>% plot_save()
}


# gg_explore_color: create consistant scatterplots with variables mapped to the
# x and y axes and to color

# Define input variables and default parameters 
gg_explore_color <- function(data, x_col, y_col, z_col,
                       param_title = str_c(y_col, " by ", x_col, 
                                           "categorized by ", z_col),
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
  str_c(x_col, "vs", y_col, "cat_by ", z_col, ".png") %>% plot_save()
}
  

# Test the functions using the mtcars dataset ----------------------------------

df_mtcars <- mtcars %>%
  mutate(cyl = as.factor(cyl),
         gear = as.factor(gear))

gg_explore(df_mtcars, "disp", "mpg")
gg_explore_color(df_mtcars, "disp", "mpg", "cyl")


# Explore the Data -------------------------------------------------------------

# glimpse()
storms %>% glimpse()
?storms

# head() & tail()
head(storms, 10)
tail(storms, 10)

# Display help on dataset
?storms


# Process the data -------------------------------------------------------------

# Save the dataframe as df_storms, removing columns that will not be used,
# coercing data types.  Filter to include only hurricanes

df_hurricane <- storms %>%
  select(-(12:13)) %>%
  filter(status == "hurricane") %>%
  mutate(year = as.integer(year),
         month = as.integer(month),
         hour = as.integer(hour))

# collapse the date & time variables into a single column
df_hurricane <- df_hurricane %>% 
  mutate(obs_date = str_c(month, "-", day, "-", year),
         # workaround: convert the hour variable to string type and join to 
         # ":00" so that lubridate will recognize as a time
         obs_time = str_c(as.character(hour), ":00"),
         obs_datetime = lubridate::mdy_hm(str_c(obs_date, "-", obs_time)),
         # convert max_cat into an integer variable, subtracting 2
         # so that the integer value matches the official category rather than
         # its ordinal position
         category = as.integer(category),
         category = category - 2) %>%
  select(name, obs_datetime, year, month, lat, long, category)


# group_by and summarize -------------------------------------------------------

# group by year and name, creating a single observation for each hurricane
# Note: Storms that occured over Dec 31/Jan 1 will have two observations,
# one in each year

df_hurricane_summary <- df_hurricane %>%
  group_by(year, name) %>%
  summarize(start = min(obs_datetime), end = max(obs_datetime),
            n_most_lat = max(lat), s_most_lat = min(lat), 
            max_cat = max(category),
            min_cat = min(category)) %>%
  mutate(length = (end - start)/lubridate::ddays(1),
         start_mo = lubridate::month(start, label = TRUE),
         end_mo = lubridate::month(end, label = TRUE))
         
# explore the summary table for df_hurricane_summary
df_hurricane_summary %>% 
  arrange(max_cat) %>%
  glimpse() %>%
  head(10)

df_hurricane_summary %>% 
  arrange(max_cat) %>%
  tail(10)

df_hurricane_summary %>% 
  arrange(min_cat) %>%
  glimpse() %>%
  head(10)

# Use walk() to iterate over the summary dataset and create visualizations------

# define x and y axis columns 

x_col <- "year"

y_cols <- c("n_most_lat", "s_most_lat", "length", "max_cat", "min_cat", 
            "start_mo", "end_mo")


# Create a functional call to iterate over the objects, calling gg_explore with 
# df_hurricane_summary 
y_cols %>% walk(gg_explore, data = df_hurricane_summary, x_col = x_col)


# Chart 1 - Length (in days) of Hurricanes, by Year ----------------------------

df_hurricane_summary %>% gg_explore("year", "length")

# This linear model shows the mean length (in days) of hurricanes increasing 
# over time. This observation is limited to the length of time that a given 
# weather event is classified as a hurricane - it does not account for the 
# length of time that the same weather event is observable as a tropical 
# depression or tropical storm.


# Chart 2 - Highest Category Reached of Hurricanes, by Year --------------------

df_hurricane_summary %>% ggplot(mapping = aes(x = year, y = max_cat)) +
  geom_jitter(alpha = 0.5, color = "blue") + 
  geom_smooth(method = "lm", se = FALSE, color = "purple") +
  labs(title = "Hurricanes: Highest Category Reached, by Year of Occurance",
       subtitle = "1975 - 2015")

# This scatterplot shows the higest category at which each hurricane was 
# observed. Note that I converted "category" from an ordinal to an integer 
# variable for the purpose of this analysis. Specifically, the linear model 
# shows that the highest cateogry reached increases over time.

# Analysis and Conclusions -----------------------------------------------------

# Viewing the charts above, I observe that by at least two measures, hurricanes 
# are becoming "worse" over time. Specifically, the charts show that the average
# length (in days) of hurricanes has increased from 1975 to 2015. The average 
# category reached - an indicator of wind speed and thus the strength of a 
# hurricane - has increased as well.

# Note that this analysis does not consider the average number of hurricanes 
# per year, nor the same storms as they build up to hurricane status, or as 
# they weaken from it. For instance, a single storm can be categorized as a 
# tropical depression, tropical storm, and hurricane at different points in 
# time, depending on its wind speed.
