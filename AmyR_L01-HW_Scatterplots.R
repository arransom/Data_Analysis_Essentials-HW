# Data Analysis Essentials
# Lesson 1 - Scatterplots
# Date: 9/10/2019

# Load Packages ----------------------------------------------------------------
library(tidyverse)

# class() ----------------------------------------------------------------------

# Let's check out the class of the data
class(diamonds) 

# glimpse() --------------------------------------------------------------------

# Use glimpse to display the variable names and sample data
glimpse(diamonds)

# help -------------------------------------------------------------------------

# Use help '?' to get a description of the data
?diamonds

# ggplot() ---------------------------------------------------------------------

# Create a scatterplot of diamonds 
# price on the y-axis and carat on the x-axis
ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price))

# labs() -----------------------------------------------------------------------

# Execute help on labs()
?labs

# Create a scatterplot of diamonds 
# price on the y-axis and carat on the x-axis
# Add a title label 'Diamonds carat vs price'
# Add a subtitle 'Banding at 1/2 carat increments'
ggplot(data = diamonds) + 
   geom_point(mapping = aes(x = carat, y = price)) +
   labs(title="Diamonds carat vs price",
        subtitle="Banding at 1/2 carat increments")
