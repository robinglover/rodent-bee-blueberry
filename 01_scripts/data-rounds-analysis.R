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

#change fieldtype to a factor
round_data1$field.type <- as.factor(round_data1$field.type)

round_data1$julien.date <- as.Date(round_data1$julien.date)

#plot date vs field type 
ggplot(round_data1, aes(y=field.type, x=julien.date, colour = field.type))+ geom_point()




