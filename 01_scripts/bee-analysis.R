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
  theme_classic(base_size = 15)+
  xlab("Field Type")+
  ylab("Bee abundance (all behaviours)")

#plot number of bee found in each field type, grouped by area
ggplot(bee1, aes(x=area, fill = field.type))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Area")+
  ylab("Bee abundance (all behaviours)")

#plot number of bees found in each field type, grouped by behaviour
ggplot(bee1, aes(fill=field.type, x=behaviour))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Behaviour")+
  ylab("Bee abundance")

####FILTER#####

#filter to bees not flying
bees_not_flying <- filter(bee1, behaviour != "flying")

####PLOT#####

#plot number of bees (not flying) found in each field type
ggplot(bees_not_flying, aes(x=field.type))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Field Type")+
  ylab("Bee abundance (not flying)")

#plot number of bee (not flying) found in each field type, grouped by area
ggplot(bees_not_flying, aes(x=area, fill = field.type))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Area")+
  ylab("Bee abundance (not flying)")

####FILTER#####

#filter to bees foraging
bees_foraging <- filter(bee1, behaviour == "foraging")

####PLOT#####

#plot number of bees (foraging only) found in each field type
ggplot(bees_foraging, aes(x=field.type))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Field Type")+
  ylab("Bee abundance (foraging only)")

#plot number of bee (not flying) found in each field type, grouped by area
ggplot(bees_foraging, aes(x=area, fill = field.type))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Area")+
  ylab("Bee abundance (foraging only)")

####FILTER#####
View(bee1)
#filter to bees nest searching
bees_nestsearching <- filter(bee1, behaviour == "nest_searching")

####PLOT#####

#plot number of bees (foraging only) found in each field type
ggplot(bees_nestsearching, aes(x=field.type))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Field Type")+
  ylab("Bee abundance (nest searching only)")

#plot number of bee (not flying) found in each field type, grouped by area
ggplot(bees_nestsearching, aes(x=area, fill = field.type))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Area")+
  ylab("Bee abundance (nest searching only)")

