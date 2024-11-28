#####SETUP#####

#load packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate) #package used to generate julian date from calendar date

#read csv
round_data <- read.csv("00_rawdata/data-entry-checklist(checklist).csv")
site_data <- read.csv("00_rawdata/site-info.csv")

#add column to round data for site type (either grass or bare)
round_data1 <- round_data %>%
  left_join(site_data, by = c("Site.name" = "name")) %>%
  rename(date = Date) %>% #change date column name to "julien.date" 
  rename (field.type = grass) #change date column name to "julien.date" 

#####FIXING UP DATAFRAME######

#change field type to a factor
round_data1$field.type <- as.factor(round_data1$field.type)

#change area to a factor
round_data1$area <- as.factor(round_data1$area)

#change date to a date
round_data1$date <- as.Date(round_data1$date)

#add column for julian date
round_data1$julian.date <- yday(round_data1$date)

#####CREATE FILTERED DATAFRAMES#####

#filter to only vegetation surveys
veg_round_data <- filter(round_data1, Survey.type == "vegetation (rodent)" | Survey.type == "vegetation (bee)" | Survey.type == "vegetation (bee and rodent)")

#filter to only bee surveys
bee_round_data <- filter(round_data1, Survey.type == "bombus")

#filter to only rodent surveys
rodent_round_data <- filter(round_data1, Survey.type == "rodent (holes only)" | Survey.type == "rodent (powder)")

#####PLOTS#####

##VEG

#plot date vs field type for veg data
ggplot(veg_round_data, aes(y=field.type, x=julian.date))+ geom_boxplot()

#plot date vs field type for veg data, grouped by area
ggplot(veg_round_data, aes(y=area, x=julian.date, colour = field.type))+ geom_boxplot()

##BEE

#plot date vs field type for bee data
ggplot(bee_round_data, aes(y=field.type, x=julian.date))+ geom_boxplot()

#plot date vs field type for bee data, grouped by area
ggplot(bee_round_data, aes(y=area, x=julian.date, colour = field.type))+ geom_boxplot()

##RODENT

#plot date vs field type for rodent data
ggplot(rodent_round_data, aes(y=field.type, x=julian.date))+ geom_boxplot()

#plot date vs field type for rodent data, grouped by area
ggplot(rodent_round_data, aes(y=area, x=julian.date, colour = field.type))+ geom_boxplot()

####STATS ANALYSIS####

#Attempting
z <- lm(julian.date ~ field.type + (1|area), data = round_data1)
