# Data Analysis Essentials
# Lesson 4 - Data Transformations
# Date: 10/24/2019

# Load Packages ----------------------------------------------------------------
library(odbc) # odbc
library(DBI) # DBI
library(lubridate)
library(tidyverse) # tidyverse

# Connect to the Database-------------------------------------------------------

# Connection string info
driver_name <- "xxxxx"
server_name <- "xxxxx"
database_name <- "xxxxx" 
user_id <- "xxxxx"
password <- "xxxxx"

# Store connection in variable: conn
conn <- dbConnect(odbc::odbc(), 
                  driver = driver_name, 
                  server = server_name, 
                  database = database_name,
                  uid = user_id,
                  pwd = password)

# Import Tables from Server ----------------------------------------------------

# Import Data from Sales.SalesOrderHeader, 
# Keeping only the variables I plan to analyze
sql_select <- "SELECT * FROM Sales.SalesOrderHeader"
df_sales_order_header <- conn %>% 
  dbGetQuery(sql_select) %>%
  select(SalesOrderID:TerritoryID) 

# Import Data from Sales.SalesOrderDetail
# Remove rowguid and ModifiedDate
sql_select <- "SELECT * FROM Sales.SalesOrderDetail"
df_sales_order_detail <- conn %>% 
  dbGetQuery(sql_select) %>%
  select(-rowguid, -ModifiedDate) 

# Import Data from Production.Product
# Keeping only the variables I plan to analyze
sql_select <- "SELECT * FROM Production.Product"
df_product<- conn %>% 
  dbGetQuery(sql_select) %>%
  select(ProductID:ProductNumber) %>%
  rename(ProductName = Name)

# Import Data from Sales.SalesTerritory
# Remove rowguid and ModifiedDate
sql_select <- "SELECT * FROM Sales.SalesTerritory"
df_sales_territory <- conn %>% 
  dbGetQuery(sql_select) %>%
  select(-rowguid, -ModifiedDate) %>%
  rename(TerritoryName = Name)

# Review Primary Key and Foreign Key prior to inner join -----------------------

# Compare the overall row count and the distinct row count for 
# df_sales_order_header to confirm that SalesOrderID is the primary key

#df_sales_order_header
df_sales_order_header %>% 
  nrow()

df_sales_order_header %>% 
  distinct(SalesOrderID) %>% 
  nrow()

# Confirm that the number of distinct values in df_sales_order_detail for the 
# foreign key (SalesOrderID) it to df_sales_order_header matches 
# the total number of rows in df_sales_order_header, a sign of referential integrity

df_sales_order_detail %>% 
  distinct(SalesOrderID) %>% 
  nrow()


# Join Tables ------------------------------------------------------------------

# Join the first two tables (df_sales_order_header and df_sales_order_detail)
df_sales_orders <- df_sales_order_header %>%
  inner_join(df_sales_order_detail, by = "SalesOrderID")

# Join df_product and df_sales_territory to df_sales_orders
df_sales_all <- df_sales_orders %>%
  left_join(df_product, by = "ProductID") %>%
  left_join(df_sales_territory, by = "TerritoryID")

# Use semi_join() with an inline filter to limit the dataframe to sales in 
# North America
df_sales_north_america <- df_sales_all %>%
  semi_join(df_sales_territory %>% filter(Group == "North America"))

# Explore the Data -------------------------------------------------------------

# Glimpse results

# glimpse(df_sales_order_header)
# glimpse(df_sales_order_detail)
# glimpse(df_product)
# glimpse(df_sales_territory)
# glimpse(df_sales_orders)
# glimpse(df_sales_all)
# glimpse(df_sales_north_america)


# Disconnect from the database -------------------------------------------------

dbDisconnect(conn)

# Data Transformations ---------------------------------------------------------

# Look at the number of distinct values in variable CountryRegionCode,
# primarily to look for data entry errors or alternate spellings/abbreviations 
df_sales_north_america %>% 
  distinct(CountryRegionCode)

# Create new columns
df_sales_north_america <- df_sales_north_america %>%
  mutate(Country = str_replace_all(CountryRegionCode, c("US" = "UnitedStates", "CA"= "Canada")),
         Year = year(OrderDate),
         Month = month(OrderDate),
         Year_Month = make_date(Year, Month))

# Review the range of dates included in the data

distinct(df_sales_north_america, Year_Month)

# Limit the dataset to years for which you there is complete data:
# 2012 and 2013; drop the variable used to review range of dates

df_sales_2012_2013 <- df_sales_north_america %>%
  filter(Year == 2012 | Year == 2013) %>%
  select(-Year_Month)

glimpse(df_sales_2012_2013)

# Chart 1 - Number of Sales per Week:  United States and Canada ----------------

# The following chart shows the number of sales per week in the United States 
# and Canada from 2012 to 2013. Note that the number of sales per week follows 
# a similar pattern in both countries, with a large spike for one week each 
# month.  Also notice that the number of sales in "off" weeks shows a consistant 
# increase beginning in July 2013.  This increase is clearly noticable for both 
# countries. 

df_sales_2012_2013 %>% 
  group_by(Country) %>%     
  mutate(Week = floor_date(OrderDate, "week")) %>%
  count(Week, name = "WeeklySales") %>%
ggplot(aes(Week, WeeklySales, color = Country)) +
  geom_line() + 
  labs(title = "Sales per Week: 2012 - 2013",
       subtitle = "United States and Canada")


# Chart 2: Number of Sales per Month: United States -------------------------------------

# In the previous chart, it appeared that the increase in sales during "off" 
# weeks was due to an overall increase in sales, rather than a shift in the 
# timing of sales. To confirm this, I charted the sales per month for the same 
# time period (2012-2013). I also limited the dataset to sales occuring in the 
# United States.

df_sales_2012_2013 %>% 
  filter(Country == "UnitedStates") %>%     
  mutate(Month = floor_date(OrderDate, "month")) %>%
  count(Month, name = "MonthlySales") %>%
ggplot(aes(Month, MonthlySales)) +
  geom_col(fill = "turquoise3") + 
  labs(title = "Sales per Month: 2012 - 2013",
       subtitle = "United States")


# Chart 3 - Number and Value of Sales Per Month: United States -----------------

# Next, I needed to confirm that the value of sales per month had increased 
# along with the number of sales per month. Another possibility is that 
# companies are ordering the same amount of goods, but placing more (and more 
# frequent) orders.

df_sales_2012_2013 %>% 
  filter(Country == "UnitedStates") %>%     
  mutate(Month = floor_date(OrderDate, "month")) %>%
  group_by(Month) %>%
  summarize(MonthlySales = n(), MonthlySalesValue = sum(LineTotal)) %>%
  mutate(MonthlySalesValueThousands = MonthlySalesValue/1000) %>%
ggplot() +
  geom_col(aes(Month, MonthlySales), fill = "turquoise3") +
  geom_line(aes(Month, MonthlySalesValueThousands))
  labs(title = "Number and Value of Sales per Month: 2012 - 2013",
     subtitle = "United States")

# Analysis and Conclusions -----------------------------------------------------

# This chart shows that the value of sales went up along with the total number 
# of sales per month in the United States. However, this does not necessarily 
# mean that the company's profits increased during that time. For instance, 
# production costs may have increased, the specific products sold may have 
# changed (for instance, from products with a higher profit-margin to those with
# a lower profit-margin), or overhead costs may have increased. One possibility:
# Perhaps the company hired an additional sales person to focus on increasing 
# sales during "off" weeks. Clearly sales have increased during those periods - 
# but have they increased enough to offset the their employment costs?