######SETUP#######

#load packages
library(ggplot2) #for plots
library(dplyr) #for working with dataframes
library(tidyr) #for working with datasets

#read csv
bee <- read.csv("00_rawdata/bee-data.csv") #bee data
site_data <- read.csv("00_rawdata/site-info.csv") #site data

#add column to round data for site type (either grass or bare) by joining 
#rodent dataframe to site dataframe
bee1 <- bee %>%
  left_join(site_data, by = c("site_id" = "name")) %>%
  rename (field.type = grass) #column listing field condition (either bare or 
#grass)  to "field.type" 

#####PLOTS#####

#plot number of bees found in each field type
ggplot(bee1, aes(x=field.type))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)

#plot number of bee found in each field type, grouped by area
ggplot(bee1, aes(x=area, fill = field.type))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)

