#load packages
library(ggplot2)
library(dplyr)
library(tidyr)

#read csv
round_data <- read.csv("00_rawdata/data-entry-checklist.csv")
site_data <- read.csv("00_rawdata/site-info.csv")

#add column to round data for site type (either grass or bare)
round_data1 <- round_data %>%
  left_join(site_data, by = c("Site.name" = "name")) %>%
  rename(julien.date = Date) %>% #change date column name to "julien.date" 
  rename (field.type = grass) #change date column name to "julien.date" 


#plot date vs field type 

ggplot(round_data1, aes(x="field.type"))+ geom_histogram(stat = "count")

round_data1$field.type

