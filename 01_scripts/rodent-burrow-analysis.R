######SETUP#######

#load packages
library(ggplot2) #for plots
library(dplyr) #for working with dataframes
library(tidyr) #for working with datasets

#read csv
rodent <- read.csv("00_rawdata/rodent-data.csv") #rodent burrow data
site_data <- read.csv("00_rawdata/site-info.csv") #site data

#add column to round data for site type (either grass or bare) by joining 
#rodent dataframe to site dataframe
rodent1 <- rodent %>%
  left_join(site_data, by = c("site_id" = "name")) %>%
  rename (field.type = grass) #column listing field condition (either bare or 
                              #grass)  to "field.type" 

#####FILTERED DATAFRAMES########

#filter dataframe to only contain holes with certain IDs (either vole or dm)
rodent_certain_species <- filter(rodent1, species_guess_final == "dm" | species_guess_final == "v")

#filter dataframe to only include holes with certain IDs for activity (either yes or no)
rodent_certain_activity <- filter(rodent1, powder_active_final == "y" | powder_active_final == "n")

#####PLOTS######

#plot hole diameter vs species guess (for certain species guesses)
ggplot(rodent_certain_species, aes(y=diameter_cm, x=species_guess_final))+ 
  geom_boxplot() +
  theme_classic(base_size = 15)

#plot number of active and inactive burrows in each field type
ggplot(rodent_certain_activity, aes(x=field.type, fill=powder_active_final))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)

#plot number of active burrows of each species (v or dm) in each field type
ggplot(rodent_certain_species, aes(x=field.type, fill=species_guess_final))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)

#plot number of burrows of all species guesses (v,dm, unknown) in each field type
ggplot(rodent1, aes(x=field.type, fill=species_guess_final))+ geom_bar(position="dodge", stat="count")

#plot total number of burrows in each field type
ggplot(rodent1, aes(x=field.type))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)


