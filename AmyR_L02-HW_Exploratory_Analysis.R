# Data Analysis Essentials
# Lesson 2 - Scatterplots
# Date: 9/26/2019

# Load Packages ----------------------------------------------------------------
library(tidyverse)

# Data processing --------------------------------------------------------------
# Rename columns x, y, and z.  Save in a new dataframe:  df_diamonds
df_diamonds<-diamonds %>% 
  rename(mm_length = x, mm_width = y, mm_depth = z) %>%
  select(-c(cut, color, table))

# View the new dataframe
glimpse(df_diamonds)

# Chart 1: Diamond Price vs. Carat ---------------------------------------------
# This scatterplot shows the price of diamonds as compared to their weight in 
# carats. Note the banding at the whole and half carat (and to a smaller degree 
# at what may be the quarter carat).

ggplot(data = df_diamonds, mapping = aes(x = carat, y = price)) + 
  geom_point(alpha = 0.1) +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Price Trends in Diamonds: Price vs. Carat",
       subtitle = "Points alpha-blended at 0.1")

# Chart 2: Diamond Price vs. Carat, By Clarity Grade ---------------------------
# This scatterplot shows the price of diamonds as compared to their weight in 
# carats, by clarity grade. The clarity grade is indicated by color.

ggplot(data = df_diamonds, 
       mapping = aes(x = carat, y = price, color = clarity)) + 
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Price Trends in Diamonds: Price vs. Carat, by Clarity Grades",
       subtitle = "Clarity level indicated by color, 
       points jittered, alpha-blended")

# Chart 3: Diamond Price vs. Carat, (Facets by Clarity) 
# This set of scatterplots scatterplot shows the price of diamonds as compared 
# to their weight in carats, by clarity grade. Each clarity grade is shown in a 
# separate scatterplot, and in a different color.

ggplot(data = df_diamonds, 
       mapping = aes(x = carat, y = price, color = clarity)) + 
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = lm, se = FALSE) +
  facet_grid(clarity ~ .) +
  labs(title = "Price Trends in Diamonds: Price vs. Carat, by Clarity Grades",
       subtitle = "Clarity level indicated by color, 
       points jittered, alpha-blended")

# Analysis and Conclusions -----------------------------------------------------

# The first scatter plot shows the price of diamonds as compared to their weight 
# in carats. Banding is clearly visible at the whole and half carat levels (and 
# to a smaller degree atapproximately the quarter carat). This may indicate 
# that reported weights may have been rounded for data entry. Alternately, it 
# could indicate that jewelers have diamonds cut to specific weights. Given the 
# increased variation in diamond price at the bands, I belive rounding is more 
# likely.

# The second scatterplot disaggregates the dataset by clarity grade. Note that 
# the trendlines are steepest at the highest clarity grades (IF/Flawless, VVS1 
# and VVS2/Very Very Slightly Included) than at lower grades (I1/Included)(This 
# dataset does not include observations of diamonds at the I2 or I3 grades). 
# This indicates that a diamond's size may have a greater impact on its price
# at higher clarity grades. It also appears that among diamonds with the 
# highest clarity grades, a greater proportion are of lower weight. However, it
# is difficult to tell if this is due to overplotting.

# The second scatterplot disaggregates the dataset by clarity grade, and 
# displays a separaate plot for each grade. It does appear that there is a 
# relationship between the clarity grade of diamonds and the proportion at 
# higher or lower weights. At higher clarity grades, a greater proportion are 
# of lower weight. As the clarity grade decreases, the number of observations at
# higher weights appears to increase. More analysis is necessary to confirm and 
# quantify this relationship and propose explanations.