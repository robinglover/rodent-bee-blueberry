#load packages
library(ggplot2)
library(dplyr)
library(tidyr)

#read csv
rodent <- read.csv("00_rawdata/rodent-data.csv")
site_data <- read.csv("00_rawdata/site-info.csv")

#add column to round data for site type (either grass or bare)
rodent1 <- rodent %>%
  left_join(site_data, by = c("site_id" = "name")) %>%
  rename (field.type = grass) #change date column name to "julien.date" 


#filter to only holes with certain IDs
rodent_certain <- filter(rodent1, species_guess_final == "dm" | species_guess_final == "v")

#plot hole diameter vs species guess
ggplot(rodent_certain, aes(y=diameter_cm, x=species_guess_final))+ geom_boxplot()

#filter to only holes with certain IDs for activity
rodent_certain2 <- filter(rodent1, powder_active_final == "y" | powder_active_final == "n")

#plot hole diameter vs species guess
ggplot(rodent_certain2, aes(x=field.type, fill=powder_active_final))+ geom_bar(position="dodge", stat="count")

#plot hole diameter vs species guess
ggplot(rodent_certain, aes(x=field.type, fill=species_guess_final))+ geom_bar(position="dodge", stat="count")

#plot hole diameter vs species guess
ggplot(rodent1, aes(x=field.type, fill=species_guess_final))+ geom_bar(position="dodge", stat="count")
