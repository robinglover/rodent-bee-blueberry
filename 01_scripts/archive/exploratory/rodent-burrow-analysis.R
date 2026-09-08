######SETUP#######

#load packages
library(ggplot2) #for plots
library(dplyr) #for working with dataframes
library(tidyr) #for working with datasets

#read csv
rodent <- read.csv("00_rawdata/rodent tracks ID(rodents).csv") #rodent burrow data
site_data <- read.csv("00_rawdata/site-info.csv") #site data

#add column to round data for site type (either grass or bare) by joining 
#rodent dataframe to site dataframe
rodent1 <- rodent %>%
  left_join(site_data, by = c("site_id" = "name")) %>%
  rename (field.type = grass) #column listing field condition (either bare or 
                              #grass)  to "field.type" 

#####FILTERED DATAFRAMES########

#filter dataframe to only contain holes with certain IDs (either vole or dm)
rodent_certain_species <- filter(rodent1, powder_species_guess_photo2 == "A" | powder_species_guess_photo2 == "B")

#filter dataframe to only include holes with certain IDs for activity (either yes or no)
rodent_certain_activity <- filter(rodent1, powder_active_photo2 == "y" | powder_active_photo2 == "n")

#####PLOTS######

#plot hole diameter vs species guess (for certain species guesses)
ggplot(rodent_certain_species, aes(y=diameter_cm, x=powder_species_guess_photo2))+ 
  geom_boxplot() +
  theme_classic(base_size = 15)+
  xlab("RG Species Guess")+
  ylab("Diameter (cm)")

#plot number of active and inactive burrows in each field type
ggplot(rodent_certain_activity, aes(x=field.type, fill=powder_active_photo2))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Field Type")+
  ylab("Rodent Burrow Abundance (count)")

#plot number of active burrows of each species (v or dm) in each field type
ggplot(rodent_certain_species, aes(x=field.type, fill=powder_species_guess_photo2))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Field Type")+
  ylab("Rodent Burrow Abundance (count)")

#plot number of burrows of all species guesses (v,dm, unknown) in each field type
ggplot(rodent1, aes(x=field.type, fill=species_guess_final))+ 
  geom_bar(position="dodge", stat="count")+
  xlab("Field Type")+
  ylab("Rodent Burrow Abundance (count)")

#plot total number of burrows in each field type
ggplot(rodent1, aes(x=field.type))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Field Type")+
  ylab("Rodent Burrow Abundance (count)")

#plot total number of burrows at each site
ggplot(rodent1, aes(x=site_id))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Field Type")+
  ylab("Rodent Burrow Abundance (count)")

#plot number of rodent burrow found in each field type, grouped by area
ggplot(rodent1, aes(x=area, fill = field.type))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Area")+
  ylab("Rodent Burrow Abundance (count)")

###planning rodent fieldwork 2025

#how many burrows were found at each plot at bates farm?
##filter to just bates farm
bates57_rodent <- filter(rodent1, site_id == "bates_57b")
bates57_rodent_filtered <- filter(rodent1, site_id == "bates_57b" & plot_id == "p2")

ggplot(bates57_rodent_filtered, aes(x=transect_id, fill = powder_species_guess_photo2))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Species Guess")+
  ylab("Rodent Burrow Abundance on Bates Farm (count)")

summary_data3 <- bates57_rodent_filtered %>%
  group_by(transect_id) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Display the summarized data as a table
kable(summary_data3, col.names = c("Transect", "Rodent Burrow Abundance (Count)"), 
      caption = "Rodent Burrow Abundance Summary")

# Enhanced table appearance
summary_data2 %>%
  kable("html", col.names = c("Activity Guess", "Rodent Burrow Abundance (Count)"), 
        caption = "Rodent Burrow Abundance Summary Overall") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))




ggplot(bates57_rodent, aes(x=powder_species_guess_photo2))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Species Guess")+
  ylab("Rodent Burrow Abundance on Bates Farm (count)")

ggplot(rodent1, aes(x=powder_species_guess_photo2))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Species Guess")+
  ylab("Rodent Burrow Abundance Overall (count)")


summary_data <- rodent1 %>%
  group_by(powder_species_guess_photo2) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

library(knitr)

# Display the summarized data as a table
kable(summary_data, col.names = c("Species Guess", "Rodent Burrow Abundance (Count)"), 
      caption = "Rodent Burrow Abundance Summary")


library(kableExtra)

# Enhanced table appearance
summary_data %>%
  kable("html", col.names = c("Species Guess", "Rodent Burrow Abundance (Count)"), 
        caption = "Rodent Burrow Abundance Summary Overall") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


summary_data2 <- rodent1 %>%
  group_by(powder_active_photo2) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Display the summarized data as a table
kable(summary_data2, col.names = c("Activity Guess", "Rodent Burrow Abundance (Count)"), 
      caption = "Rodent Burrow Abundance Summary")

# Enhanced table appearance
summary_data2 %>%
  kable("html", col.names = c("Activity Guess", "Rodent Burrow Abundance (Count)"), 
        caption = "Rodent Burrow Abundance Summary Overall") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


#filter to rodent active only

active <- filter(rodent1, powder_active_photo2 == "y")

summary_data4 <- active %>%
  group_by(powder_species_guess_photo2) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Display the summarized data as a table
kable(summary_data4, col.names = c("Species Guess", "Rodent Burrow Abundance (Count)"), 
      caption = "Rodent Burrow Abundance Summary")

# Enhanced table appearance
summary_data4 %>%
  kable("html", col.names = c("Activity Guess", "Rodent Burrow Abundance (Count)"), 
        caption = "Rodent Burrow Abundance Summary Overall") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


