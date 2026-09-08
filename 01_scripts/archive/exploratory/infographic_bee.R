######SETUP#######

#load packages
library(ggplot2) #for plots
library(dplyr) #for working with dataframes
library(tidyr) #for working with datasets
library(lme4) #for mixed effects models
library(ggpubr) #for adding significance asterisks to graph
library(tidyr) #make adjustments to dataframes


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



###for loop####
site_id_list = c("chong_96", "chong_68", "bates_57b", "bks_56", "bks_68", "bks_96", "sandhu_ladnertrunk", "lbf_ladnertrunk", "avtar_168", "surjit_168", "gwb_neaves", "bks_neaves")

combined_table <- list()

combined_table2 <- list()

behaviours = c("nest_searching", "foraging", "resting", "flying", "unknown")

# Loop over each site
for (site in site_id_list) {
  
  # Filter data for current farm
  bee1_site <- bee1 %>% filter(site_id == site)
  
  # Calculate number of bees per location, adjusting for sampling effort
  bees_in_location <- bee1_site %>%
    group_by(location) %>%
    summarise(bees_in_location = n()) %>%
    mutate(bees_in_location_adjusted = ifelse(location == "ditch", bees_in_location * 2, bees_in_location)) %>%
    ungroup()  # Ungroup the data after summarizing
  
  # Store the result in combined_table (you can append or store as a list)
  combined_table[[site]] <- bees_in_location
  
  # Initialize an empty list for behaviour data
  behaviour_data_list <- list()
  
  # Loop through each behaviour and calculate the corresponding data
  for (behaviour in behaviours) {
    # Filter and process the data for the current behaviour
    behaviour_data <- bee1_site %>%
      filter(behaviour == behaviour) %>%
      group_by(location) %>%
      summarise(count = n()) %>%
      mutate(behaviour_bees = ifelse(location == "ditch", count * 2, count)) %>%
      rename(!!paste0(behaviour, "_bees") := behaviour_bees) %>%
      ungroup()  # Ungroup the data after summarizing
    
    # Store the result in the list with the behaviour name
    behaviour_data_list[[behaviour]] <- behaviour_data
  }
  
  # Join all behaviour data in the behaviour_data_list
  combined_table2[[site]] <- bees_in_location %>%
    left_join(behaviour_data_list$foraging, by = "location") %>%
    left_join(behaviour_data_list$nest_searching, by = "location") %>%
    left_join(behaviour_data_list$flying, by = "location") %>%
    left_join(behaviour_data_list$resting, by = "location") %>%
    left_join(behaviour_data_list$unknown, by = "location") %>%
    mutate(
      # Ensure numeric types before performing division
      nest_searching_bees = as.numeric(nest_searching_bees),
      foraging_bees = as.numeric(foraging_bees),
      flying_bees = as.numeric(flying_bees),
      resting_bees = as.numeric(resting_bees),
      unknown_bees = as.numeric(unknown_bees),
      bees_in_location_adjusted = as.numeric(bees_in_location_adjusted),
      
      # Calculate percentages for each behaviour
      perc_nest_searching = (nest_searching_bees / bees_in_location_adjusted) * 100,
      perc_foraging = (foraging_bees / bees_in_location_adjusted) * 100, 
      perc_flying = (flying_bees / bees_in_location_adjusted) * 100,
      perc_resting = (resting_bees / bees_in_location_adjusted) * 100,
      perc_unknown = (unknown_bees / bees_in_location_adjusted) * 100
    ) %>%
    # Select only the columns we need
    select(location, bees_in_location_adjusted, perc_nest_searching, perc_foraging, perc_flying, perc_resting, perc_unknown)
  
  # Print the combined table for the current site
  print(combined_table2[[site]])
}







for (site in site_id_list) {
  # Filter data for current farm
  bee1_site <- bee1 %>% filter(site_id == site)
  
  #Calculate number of bees per location, adjusting for sampling effort
  bees_in_location <- bee1_site %>%
    group_by(location) %>%
    summarise(bees_in_location = n()) %>%
    mutate(bees_in_location_adjusted = ifelse(location == "ditch", bees_in_location * 2, bees_in_location)) 
  
  # Store the result in combined_table (you can append or store as a list)
  combined_table[[site]] <- bees_in_location
  
  # Initialize an empty list for behaviour data
  behaviour_data_list <- list()
  
  # Loop through each behaviour and calculate the corresponding data
  for (behaviour in behaviours) {
    # Filter and process the data for the current behaviour
    behaviour_data <- bee1_site %>%
      filter(behaviour == behaviour) %>%
      group_by(location) %>%
      summarise(count = n()) %>%
      mutate(behaviour_bees = ifelse(location == "ditch", count * 2, count)) 
    
    # Store the result in the list with the behaviour name
    behaviour_data_list[[behaviour]] <- behaviour_data
  }
  
  #Join all behaviour data in the behaviour_data_list
  combined_table2[[site]] <- bees_in_location %>%
    left_join(behaviour_data_list$foraging, by = "location") %>%
    left_join(behaviour_data_list$nest_searching, by = "location") %>%
    left_join(behaviour_data_list$flying, by = "location") %>%
    left_join(behaviour_data_list$resting, by = "location") %>%
    left_join(behaviour_data_list$unknown, by = "location") %>%
    mutate(
      # Ensure numeric types before performing division
      nest_searching_bees = as.numeric(nest_searching_bees),
      foraging_bees = as.numeric(foraging_bees),
      flying_bees = as.numeric(flying_bees),
      resting_bees = as.numeric(resting_bees),
      unknown_bees = as.numeric(unknown_bees),
      bees_in_location_adjusted = as.numeric(bees_in_location_adjusted),
      
      # Calculate percentages
      perc_nest_searching = (nest_searching_bees / bees_in_location_adjusted) * 100,
      perc_foraging = (foraging_bees / bees_in_location_adjusted) * 100, 
      perc_flying = (flying_bees / bees_in_location_adjusted) * 100,
      perc_resting = (resting_bees / bees_in_location_adjusted) * 100,
      perc_unknown = (unknown_bees / bees_in_location_adjusted) * 100
    ) %>%
    # Select only the columns we need
    select(location, bees_in_location_adjusted, perc_nest_searching, perc_foraging, perc_flying, perc_resting, perc_unknown)
  
  # Print the combined table
  print(combined_table2[[site]])
}























#####extra####

# Calculate the number of bees per location, adjusting for sampling effort
bees_in_location <- bee1_avatar %>%
  group_by(location) %>%
  summarise(bees_in_location = n()) %>%
  mutate(bees_in_location = ifelse(location == "ditch", bees_in_location * 2, bees_in_location))

#####for loop#####
behaviours = c("nest_searching", "foraging", "resting", "flying", "unknown")

bee_results <- list()
# Loop through each behaviour and calculate the corresponding data
for (behaviour in behaviours) {
  # Filter and process the data for the current behaviour
  behaviour_data <- bee1_avatar %>%
    filter(behaviour == behaviour) %>%
    group_by(location) %>%
    summarise(count = n()) 
  
  # Store the result in the list with the behaviour name
  bee_results[[behaviour]] <- behaviour_data
  
  print(bee_results[[behaviour]])
}

# Calculate the number of nest-searching bees 
nest_searching_bees <- bee1_avatar %>%
  filter(behaviour == "nest_searching") %>%
  group_by(location) %>%
  summarise(nest_searching = n()) %>%
  mutate(nest_searching_bees = ifelse(location == "ditch", nest_searching * 2, nest_searching))

# Calculate the number of foraging bees 
foraging_bees <- bee1_avatar %>%
  filter(behaviour == "foraging") %>%
  group_by(location) %>%
  summarise(foraging = n()) %>%
  mutate(foraging_bees = ifelse(location == "ditch", foraging * 2, foraging))

# Calculate the number of flying bees 
flying_bees <- bee1_avatar %>%
  filter(behaviour == "flying") %>%
  group_by(location) %>%
  summarise(flying = n()) %>%
  mutate(flying_bees = ifelse(location == "ditch", flying * 2, flying))

# Calculate the number of resting bees 
resting_bees <- bee1_avatar %>%
  filter(behaviour == "resting") %>%
  group_by(location) %>%
  summarise(resting = n()) %>%
  mutate(resting_bees = ifelse(location == "ditch", resting * 2, resting))

# Calculate the number of unknown behaviour bees 
unknown_bees <- bee1_avatar %>%
  filter(behaviour == "unknown") %>%
  group_by(location) %>%
  summarise(unknown = n()) %>%
  mutate(unknown_bees = ifelse(location == "ditch", unknown * 2, unknown))

combined_table <- bees_in_location_adjusted %>%
  left_join(foraging_bees_adjusted, by = "location") %>%
  left_join(nest_searching_bees_adjusted, by = "location") %>%
  left_join(flying_bees_adjusted, by = "location") %>%
  left_join(resting_bees_adjusted, by = "location") %>%
  left_join(unknown_bees_adjusted, by = "location") %>%
  mutate(
    # Calculate percentage of nest-searching bees
    perc_nest_searching = (nest_searching_bees_adjusted / bees_in_location_adjusted) * 100,
    
    # Calculate percentage of foraging bees
    perc_foraging = (foraging_bees_adjusted / bees_in_location_adjusted) * 100, 
    # Calculate percentage of nest-searching bees
    perc_flying = (flying_bees_adjusted / bees_in_location_adjusted) * 100,
    
    # Calculate percentage of foraging bees
    perc_resting = (resting_bees_adjusted / bees_in_location_adjusted) * 100,
    # Calculate percentage of unknown behaviour bees
    perc_unknown = (unknown_bees_adjusted / bees_in_location_adjusted) * 100
  ) %>%
  # Select only the columns we need
  select(location, bees_in_location_adjusted, perc_nest_searching, perc_foraging, perc_flying, perc_unknown)

# Print the combined table
print(combined_table)




######create table for each farm#####

site_id_list = c("chong_96", "chong_68", "bates_57b", "bks_56", "bks_68", "bks_96", "sandhu_ladnertrunk", "lbf_ladnertrunk", "avtar_168", "surjit_168", "gwb_neaves", "bks_neaves")
combined_table <- list()

for (i in site_id_list) {
  # Filter data for Farm
  bee1_i <- bee1 %>% filter(site_id == i)
  
  # Calculate the number of bees per location, adjusting for sampling effort
  bees_in_location <- bee1_i %>%
    group_by(location) %>%
    summarise(bees_in_location = n()) 
  
  # Adjust the number of bees in the field by halving the count, as there was twice the sampling effort in the field
  bees_in_location_adjusted <- bees_in_location %>%
    mutate(bees_in_location_adjusted = ifelse(location == "ditch", bees_in_location * 2, bees_in_location))
  
  # Calculate the number of nest-searching bees 
  nest_searching_bees <- bee1_i %>%
    filter(behaviour == "nest_searching") %>%
    group_by(location) %>%
    summarise(nest_searching = n()) 
  
  # Adjust the number of bees in the ditch by doubling the count, as there was twice the sampling effort in the field
  nest_searching_bees_adjusted <- nest_searching_bees %>%
    mutate(nest_searching_bees_adjusted = ifelse(location == "ditch", nest_searching * 2, nest_searching))
  
  # Calculate the number of foraging bees 
  foraging_bees <- bee1_i %>%
    filter(behaviour == "foraging") %>%
    group_by(location) %>%
    summarise(foraging = n())
  
  # Adjust the number of bees in the ditch by doubling the count, as there was twice the sampling effort in the field
  foraging_bees_adjusted <- foraging_bees %>%
    mutate(foraging_bees_adjusted = ifelse(location == "ditch", foraging * 2, foraging))
  
  # Calculate the number of flying bees 
  flying_bees <- bee1_i %>%
    filter(behaviour == "flying") %>%
    group_by(location) %>%
    summarise(flying = n()) 
  
  # Adjust the number of bees in the ditch by doubling the count, as there was twice the sampling effort in the field
  flying_bees_adjusted <- flying_bees %>%
    mutate(flying_bees_adjusted = ifelse(location == "ditch", flying * 2, flying))
  
  # Calculate the number of resting bees 
  resting_bees <- bee1_i %>%
    filter(behaviour == "resting") %>%
    group_by(location) %>%
    summarise(resting = n())
  
  # Adjust the number of bees in the ditch by doubling the count, as there was twice the sampling effort in the field
  resting_bees_adjusted <- resting_bees %>%
    mutate(resting_bees_adjusted = ifelse(location == "ditch", resting * 2, resting))
  
  # Calculate the number of unknown behaviour bees 
  unknown_bees <- bee1_i %>%
    filter(behaviour == "unknown") %>%
    group_by(location) %>%
    summarise(unknown = n())
  
  # Adjust the number of bees in the ditch by doubling the count, as there was twice the sampling effort in the field
  unknown_bees_adjusted <- unknown_bees %>%
    mutate(unknown_bees_adjusted = ifelse(location == "ditch", unknown * 2, unknown))
  
  combined_table[i] <- bees_in_location_adjusted %>%
    left_join(foraging_bees_adjusted, by = "location") %>%
    left_join(nest_searching_bees_adjusted, by = "location") %>%
    left_join(flying_bees_adjusted, by = "location") %>%
    left_join(resting_bees_adjusted, by = "location") %>%
    left_join(unknown_bees_adjusted, by = "location") %>%
    mutate(
      # Calculate percentage of nest-searching bees
      perc_nest_searching = (nest_searching_bees_adjusted / bees_in_location_adjusted) * 100,
      
      # Calculate percentage of foraging bees
      perc_foraging = (foraging_bees_adjusted / bees_in_location_adjusted) * 100, 
      # Calculate percentage of nest-searching bees
      perc_flying = (flying_bees_adjusted / bees_in_location_adjusted) * 100,
      
      # Calculate percentage of foraging bees
      perc_resting = (resting_bees_adjusted / bees_in_location_adjusted) * 100,
      # Calculate percentage of unknown behaviour bees
      perc_unknown = (unknown_bees_adjusted / bees_in_location_adjusted) * 100
    ) %>%
    # Select only the columns we need
    select(location, bees_in_location_adjusted, perc_nest_searching, perc_foraging, perc_flying, perc_unknown)
  
  # Print the combined table
  print(combined_table[i])
}
