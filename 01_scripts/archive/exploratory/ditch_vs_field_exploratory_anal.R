######SETUP#######

#load packages
library(ggplot2) #for plots
library(dplyr) #for working with dataframes
library(tidyr) #for working with datasets
library(lme4) #for mixed effects models
library(ggpubr) #for adding significance asterisks to graph

#read csv
bee <- read.csv("00_rawdata/bee-data.csv") #bee data
site_data <- read.csv("00_rawdata/site-info.csv") #site data

#add column to round data for site type (either grass or bare) by joining 
#rodent dataframe to site dataframe
bee1 <- bee %>%
  left_join(site_data, by = c("site_id" = "name")) %>%
  rename (field.type = grass) #column listing field condition (either bare or 
#grass)  to "field.type" 

#add new column for location (field for p1 and p2 or ditch)
bee1 <- mutate(bee1, location = if_else(plot_id == "p1" | plot_id == "p2",
                                        "field", 
                                        if_else(plot_id == "ditch", 
                                                "ditch", 
                                                NA)))
#create datasheet with site, round #, plot, sample, behaviour, location, and count number of each behaviour
bee2 <- bee1 %>% 
  mutate(effort_correction = if_else(location == "field", 0.5, 1)) %>% 
  count(site_id, round, plot_id, sample_code, behaviour, location) %>%
  left_join(
    bee1 %>% mutate(effort_correction = if_else(location == "field", 0.5, 1)) %>% 
      select(site_id, round, plot_id, sample_code, effort_correction) %>% distinct(),
    by = c("site_id", "round", "plot_id", "sample_code")
  ) %>%
  mutate(corrected_n = n * effort_correction)

#BOX PLOT: number of bees found in each location (ditch vs field), grouped by behaviour
ggplot(bee2, aes(fill=location, x=behaviour, y = corrected_n))+ 
  geom_boxplot() +
  theme_classic(base_size = 15)+
  xlab("Behaviour")+
  ylab("Bee abundance")+
  stat_compare_means(
    aes(group = location),  # group by location
    method = "t.test",  # or "wilcox.test" if you're doing a non-parametric test
    label = "p.format",  # Display p-value 
    size = 3.7,  # Adjust this value to make the asterisks larger
    )

#run separate t-tests for each behaviour
##flying
onlyFlying <- filter(bee2, behaviour == "flying")
t.test(corrected_n ~ location, data = onlyFlying, var.equal = FALSE)

##foraging
onlyForaging <- filter(bee2, behaviour == "foraging")
t.test(corrected_n ~ location, data = onlyForaging, var.equal = FALSE)

##nest_searching
onlyNestSearching <- filter(bee2, behaviour == "nest_searching")
t.test(corrected_n ~ location, data = onlyNestSearching, var.equal = FALSE)

##resting
onlyResting <- filter(bee2, behaviour == "resting")
t.test(corrected_n ~ location, data = onlyResting, var.equal = FALSE)
