#load packages
library(ggplot2) #for plots
library(dplyr) #for working with dataframes
library(tidyr) #for working with datasets
library(lubridate) #package used to generate julian date from calendar date


#read csv
flower <- read.csv("00_rawdata/flowers-data.csv") #floral data
site_data <- read.csv("00_rawdata/site-info.csv") #site data

#add column to round data for site type (either grass or bare) by joining 
#rodent dataframe to site dataframe
flower1 <- flower %>%
  left_join(site_data, by = c("site_id" = "name")) %>%
  rename (field.type = grass) #column listing field condition (either bare or 
#grass)  to "field.type" 

#add new column for location (field for p1 and p2 or ditch)
flower1 <- mutate(flower1, location = if_else(plot_id == "p1" | plot_id == "p2",
                                        "field", 
                                        if_else(plot_id == "ditch", 
                                                "ditch", 
                                                NA)))

#change date to a date
flower1$date <- as.Date(flower1$date)

#add column for julian date
flower1$julian.date <- yday(flower1$date)


head(flower1)
ggplot(flower1, aes(x=date))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Date")+
  ylab("# floral species") 

ggplot(flower1, aes(fill=field.type, x=factor(round, level=c("NS", "1", "2", "3", "NA"))))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Date")+
  ylab("# floral species")
