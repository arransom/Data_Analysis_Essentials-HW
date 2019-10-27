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
df_sales_all <- df_sales_all %>%
  semi_join(df_sales_territory %>% filter(Group == "North America"))

# Explore the Data -------------------------------------------------------------

# Glimpse results

glimpse(df_sales_order_header)
glimpse(df_sales_order_detail)
glimpse(df_product)
glimpse(df_sales_territory)
glimpse(df_sales_orders)
glimpse(df_sales_all)