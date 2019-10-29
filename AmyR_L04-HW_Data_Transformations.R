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


# Data Transformations ---------------------------------------------------------

# Look at the number of distinct values in variable CountryRegionCode,
# primarily to look for data entry errors or alternate spellings/abbreviations 
df_sales_north_america %>% 
  distinct(CountryRegionCode)

# Create new column, Country, with the names of the countries spelled out
df_sales_north_america <- df_sales_north_america %>%
  mutate(Country = str_replace_all(CountryRegionCode, c("US" = "UnitedStates", "CA"= "Canada")))




# Disconnect from the database -------------------------------------------------

dbDisconnect(conn)