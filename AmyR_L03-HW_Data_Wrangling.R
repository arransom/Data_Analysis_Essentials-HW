# Data Analysis Essentials
# Lesson 3 - Data Acquisition
# Date: 10/01/2019

# Load Packages ----------------------------------------------------------------
library(tidyverse)

# Explore Data Structure -------------------------------------------------------

# Data: house_listings
load("house_listings.RData")


# Use glimpse() to explore the dataframe
house_listings %>% glimpse()

# Use summary() to explore the resulting dataframe
house_listings %>% summary()

n_distinct(house_listings$city)

# Data Processing --------------------------------------------------------------

# Create a snapshot of the data before making any changes
house_listings_snap1 <- house_listings

# Create a new data frame with the appropriate data types & data cleaning 
df_house_listings <- house_listings %>%
  mutate(city = as.factor(city),        # Perform data type conversions
         year = as.integer(year),
         month = as.integer(month),
         sales = as.integer(sales),
         volume = as.integer(volume),
         median = as.integer(median),
         listings = as.integer(listings),
         inventory = as.double(inventory)) %>%
  select(-volume) %>%           # Remove unused columns 
  drop_na(median)               # Drop rows where median home price is missing

# Use glimpse() to explore the resulting dataframe
df_house_listings %>% glimpse()

# Use summary() to explore the resulting dataframe
df_house_listings %>% summary()


# Chart 1: Number of Active House Listings -------------------------------------

# This histogram shows the distribution of values for number of house listings.

# Looking at summary statistics for the cleaned dataframe, I noticed that the 
# maximum value for the variable "listings" (showing the number of active house 
# listings) was significantly higher than the 3rd quartile, and that the mean 
# was much higher than the median. I used a histogram to better understand this 
# distribution: Was there a single outlier, or were there several observations 
# with listings in the higher end of the range?
  
# Histogram - Number of Active Listings
ggplot(df_house_listings) +
  geom_histogram(aes(x = listings), bins = 25) +
  labs(title = "Histogram: Number of Active Listings",
       subtitle = "Number of Bins: 25")

# The histogram clearly showed that there are multiple observations with 
# listings at the higher end of the range. I wonder if one or two cities are 
# responsible for these observations?


# More Data Processing ---------------------------------------------------------

# Group by City and Calculate Summary Values for # of Active Listings by City
df_house_listings_city <- df_house_listings %>%
  group_by(city) %>% 
  summarize(
    min_listings = min(listings, na.rm = TRUE),
    median_listings = median(listings, na.rm = TRUE),
    mean_listings = mean(listings, na.rm = TRUE),
    max_listings = max(listings, na.rm = TRUE))%>%
  print(n=Inf)

# Filter the Data to include observations for Dallas and Houston, only 
df_dallas_houston <- df_house_listings %>%
  filter(city %in% c("Dallas", "Houston")) %>%
  mutate(city = fct_drop(city))


# Chart 2: Median Home Price vs. Number of Active House Listings ---------------

# After additional data wrangling, I found that two cities, Dallas and Houston, 
# were responsible for all of the observations with the number of active 
# listings above 15,000.

# This chart shows the relationship between the median home price and the number
# of active listings in Dallas and Houston.

# Scatterplot & Trend Lines: Median Home Price vs. House Listings
ggplot(df_dallas_houston, aes(x = listings, y = median, color = city)) + 
  geom_point() +
  geom_smooth(method = lm)+
  labs(title = "Median House Price vs. Active Listings: Dallas and Houston",
       subtitle = "Notice wide standard error, especially for Houston")


# Chart 3: Median House Price vs. Number of Sales ------------------------------

# This scatterplot shows the relationship between the median house price and 
# the number of house sales in Dallas and Houston.

# Scatterplot & Trend Lines: Median House Price vs. Number of Sales
ggplot(df_dallas_houston, aes(x = sales, y = median, color = city)) + 
  geom_point() +
  geom_smooth(method = lm)+
  labs(title = "Median House Price vs. Number of House Sales: 
       Dallas and Houston",
       subtitle = "Notice wide standard error for Dallas")


# Analysis and Conclusions -----------------------------------------------------

# As described above, the first chart shows the distribution of values for 
# number of house listings. Using the histogram, it's easy to see that the most 
# frequent values for the number of active house listings were between 0 and 
# 5,000. A much smaller number of observations have values between 5,000 and 
# ~15,000, and a handful of observations have values from ~15,000 to more than 
# 40,000 active house listings in a given month.

# The second chart shows the relationship between the median home price and the 
# number of active listings on the market in Dallas and Houston. In Dallas, the 
# median home price is negatively correlatied with the number of houses listed. 
# One possible explanation is that a large 'supply' of houses on the market 
# pushes prices down, as potential home buyers have multiple options to choose 
# from and may have additional negotiating power. However, in Houston, the 
# median home price is positively correlated with the number of houses listed. 
# As the number of observations considered is relatively small (21 observations 
# for Dallas, 23 observations for Houston) and the standard error for both 
# cities is quite high, it would be irresponsible to draw any conclusions based 
# on this data.

# The third chart shows the relationship between the median house price and the 
# number of house sales in Dallas and Houston. The two variables are positively 
# correlated - the median house price and the number of house sales rise 
# together.

# Opportunities for Future Research --------------------------------------------
# The dataset includes information about home sales in a number of different 
# cities in Texas and across several months and years. I would be very 
# interested in looking at changes over time in the number of home sales, the 
# median sales price, and the relationship between these and other factors 
# (including the economy, natural disasters, and other variables that may 
# influence home sales
