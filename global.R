library(tidyverse)
library(ggpubr)
library(scales)
library(glue)
library(plotly)
library(lubridate)
library(dplyr)
library(scales)
library(zoo)
library(leaflet)
library(leaflet.extras)
options(scipen=123)
library(DT)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)


#Read data
superstore_main <- read.csv("Superstore.csv")

#change the data
superstore_data <-  superstore_main %>% 
  select(-c(Postal.Code)) %>% 
  mutate(
    Order.ID = as.factor(Order.ID),
    Order.Date = mdy(Order.Date),
    Ship.Date = mdy(Ship.Date),
    Ship.Mode = as.factor(Ship.Mode),
    Customer.ID = as.factor(Customer.ID),
    Customer.Name = as.factor(Customer.Name),
    Region = as.factor(Region),
    Product.ID = as.factor(Product.ID),
    Category = as.factor(Category),
    Order.Year = year(Order.Date),
    Ship.Year = year(Order.Date),
    Order.YQ = as.yearqtr(Order.Date,format = "%Y -%m -%d")
  ) 

#Leaflet Data
sales_area <- superstore_data %>% 
  group_by(Lattitude,Longitude,State) %>% 
  summarise(Profit_A = sum(round(Profit),4) ,Cust_Num = length(unique(Customer.ID))) %>% 
  mutate(Profit_A = prettyNum(Profit_A, big.mark = ",")) %>% 
  select(Lattitude,Longitude,State, Profit_A,Cust_Num) %>% 
  ungroup() 

color_area <- colorNumeric(palette = "31D6D1", domain = sales_area$Cust_Num )

store_icon <- awesomeIcons(icon = "briefcase", iconColor = "yellow", markerColor = "black",library = "fa" )


