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

#add new column for location (field for p1 and p2 or ditch)
bee <- mutate(bee, location = if_else(plot_id == "p1" | plot_id == "p2",
                                        "field", 
                                        if_else(plot_id == "ditch", 
                                                "ditch", 
                                                NA)))


####just for avtar####
# Filter data for current farm
bee_avtar<- bee %>% filter(site_id == "avtar_168")

# Calculate number of bees per location, adjusting for sampling effort
bees_in_location <- bee_avtar %>%
  group_by(location) %>%
  summarise(bees_in_location = n()) %>%
  mutate(bees_in_location_adjusted = ifelse(location == "ditch", bees_in_location * 2, bees_in_location))%>%
  ungroup()

## Filter and process the data for the current behaviour
foraging_data <- bee_avtar %>%
  filter(behaviour == "foraging") %>%
  group_by(location) %>%
  summarise(count = n()) %>%
  mutate(foraging_bees = ifelse(location == "ditch", count * 2, count),
         perc_foraging = (foraging_bees / bees_in_location$bees_in_location_adjusted) * 100)%>%
  ungroup()

nest_searching_data <- bee_avtar %>%
  filter(behaviour == "nest_searching") %>%
  group_by(location) %>%
  summarise(count = n()) %>%
  mutate(ns_bees = ifelse(location == "ditch", count * 2, count),
         perc_ns = (ns_bees / bees_in_location$bees_in_location_adjusted) * 100)%>%
  ungroup()

resting_data <- bee_avtar %>%
  filter(behaviour == "resting") %>%
  group_by(location) %>%
  summarise(count = n()) %>%
  mutate(resting_bees = ifelse(location == "ditch", count * 2, count))%>%
  left_join(bees_in_location, by = "location") %>%
  mutate(perc_resting = (resting_bees / bees_in_location_adjusted) * 100) %>%
  ungroup()

flying_data <- bee_avtar %>%
  filter(behaviour == "flying") %>%
  group_by(location) %>%
  summarise(count = n()) %>%
  mutate(flying_bees = ifelse(location == "ditch", count * 2, count),
         perc_flying = (flying_bees /bees_in_location$bees_in_location_adjusted) * 100)%>%
  ungroup()

unknown_data <- bee_avtar %>%
  filter(behaviour == "unknown") %>%
  group_by(location) %>%
  summarise(count = n()) %>%
  mutate(unknown_bees = ifelse(location == "ditch", count * 2, count)) %>%
  left_join(bees_in_location, by = "location") %>%
  mutate(perc_unknown = (unknown_bees /bees_in_location_adjusted) * 100)%>%
  ungroup()

combined_table <- bees_in_location %>%
  left_join(foraging_data, by = "location") %>%
  left_join(nest_searching_data, by = "location") %>%
  left_join(resting_data, by = "location") %>%
  left_join(flying_data, by = "location") %>%
  left_join(unknown_data, by = "location") %>%
  # Select only the columns we need
  select(location, bees_in_location_adjusted, perc_foraging, perc_ns, perc_resting, perc_flying, perc_unknown)

# Print the combined table for the current site
print(combined_table)

#####for loop for all sites#####

#list of site IDs
site_ids = c("chong_96", "chong_68", "bates_57b", "bks_56", "bks_68", "bks_96", "sandhu_ladnertrunk", "lbf_ladnertrunk", "avtar_168", "surjit_168", "gwb_neaves", "bks_neaves")

#empty list to store results from for loop
all_sites_results <- list()

# Loop through each site
for (site in site_ids) {
  
  # Filter data for the current site
  bee_site <- bee %>% filter(site_id == site)
  
  # Calculate adjusted total bees per location
  bees_in_location <- bee_site %>%
    group_by(location) %>%
    summarise(bees_in_location = n()) %>%
    mutate(bees_in_location_adjusted = ifelse(location == "ditch", bees_in_location * 2, bees_in_location)) %>%
    ungroup()
  
  # Define a helper function to calculate adjusted counts and percentages for each behaviour
  process_behaviour <- function(behaviour_label, new_col_name, perc_col_name) {
    bee_site %>%
      filter(behaviour == behaviour_label) %>%
      group_by(location) %>%
      summarise(count = n()) %>%
      mutate(!!new_col_name := ifelse(location == "ditch", count * 2, count)) %>%
      left_join(bees_in_location, by = "location") %>%
      mutate(!!perc_col_name := (!!sym(new_col_name) / bees_in_location_adjusted) * 100) %>%
      select(location, !!perc_col_name)
  }
  
  # Process each behaviour
  foraging_data <- process_behaviour("foraging", "foraging_bees", "perc_foraging")
  nest_searching_data <- process_behaviour("nest_searching", "nest_searching_bees", "perc_nest_searching")
  resting_data <- process_behaviour("resting", "resting_bees", "perc_resting")
  flying_data <- process_behaviour("flying", "flying_bees", "perc_flying")
  unknown_data <- process_behaviour("unknown", "unknown_bees", "perc_unknown")
  
  # Combine all into one table
  combined_table <- bees_in_location %>%
    left_join(foraging_data, by = "location") %>%
    left_join(nest_searching_data, by = "location") %>%
    left_join(resting_data, by = "location") %>%
    left_join(flying_data, by = "location") %>%
    left_join(unknown_data, by = "location") %>%
    mutate(site_id = site) %>%
    select(site_id, location, bees_in_location_adjusted,
           perc_foraging, perc_nest_searching, perc_resting, perc_flying, perc_unknown)
  
  # Save the result in the list, using site_id as the name
  all_sites_results[[site]] <- combined_table
}

# Combine all into one dataframe
final_results <- bind_rows(all_sites_results)

# Print or view the final result
print(final_results)

#write output as csv
write.csv(final_results, "bee_behavior_summary_by_site.csv", row.names = FALSE)


#####create pie chart for each row####

# Define consistent colors for behaviours
behaviour_colors <- c(
  foraging = "steelblue",
  nest_searching = "darkolivegreen3",
  resting = "grey26",
  flying = "darkgoldenrod2",
  unknown = "orangered3"
)

#for loop
for (i in 1:nrow(final_results)) {
  
  row_data <- final_results[i, ]
  
  # Pivot to long format
  pie_data <- row_data %>%
    select(site_id, location, perc_foraging, perc_nest_searching, perc_resting, perc_flying, perc_unknown) %>%
    pivot_longer(
      cols = starts_with("perc_"),
      names_to = "behaviour",
      values_to = "percentage"
    ) %>%
    mutate(
      behaviour = gsub("perc_", "", behaviour),
      percentage = round(percentage, 1)
    ) %>%
    arrange(desc(behaviour)) %>%
    filter(percentage > 0)  # remove 0% values to clean up small slices
  
  # Calculate positions for labels
  pie_data <- pie_data %>%
    mutate(
      ypos = cumsum(percentage) - 0.5 * percentage,
      label = paste0(percentage, "%")
    )
  
  # Create pie chart
  pie_plot <- ggplot(pie_data, aes(x = "", y = percentage, fill = behaviour)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    geom_text(aes(y = ypos, label = label), color = "white", size = 5, fontface = "bold") +
    labs(
      title = paste("Site:", row_data$site_id, "| Location:", row_data$location),
      fill = "Behaviour"
    ) +
    scale_fill_manual(values = behaviour_colors) +  # consistent colours 
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
    )
  
  print(pie_plot)
  
  #Save to file
  ggsave(paste0("pie_", row_data$site_id, "_", row_data$location, ".png"), pie_plot)
}


#####create csv containing the following info for each site: list of bee species, #####
#number of individuals of each species

#load bee data
bee2 <- read.csv("00_rawdata/bee-data-with-extra-sightings-added.csv") #bee data

# Example site_id_list (you should have this in your data)
site_id_list <- c("chong_96", "chong_68", "bates_57b", "bks_56", "bks_68", "bks_96", "sandhu_ladnertrunk", "lbf_ladnertrunk", "avtar_168", "surjit_168", "gwb_neaves", "bks_neaves")

# Placeholder for the final result (list of data frames for each site)
combined_results <- list()

# Loop through each site
for (site in site_id_list) {
  
  # Filter the data for the current site (replace 'your_data' with your actual dataset)
  site_data <- bee2 %>% filter(site_id == site)
  
  # Summarize bee species and number of individuals (count of bees for each species)
  bee_summary <- site_data %>%
    group_by(bee_species) %>%
    summarise(num_individuals = n()) %>%
    mutate(site_id = site) %>% # Add the site_id to each row
    ungroup()
  
  # Store the result for the current site
  combined_results[[site]] <- bee_summary
}

# Combine all results into a single data frame
final_combined_data <- bind_rows(combined_results)

# Write the combined data to a CSV file
write.csv(final_combined_data, "site_bee_summary.csv", row.names = FALSE)

#####create csv containing the following info for each site: list of floral species, #####
#number of individuals of each species

#load bee data
bee2 <- read.csv("00_rawdata/bee-data-with-extra-sightings-added.csv") #bee data

# Example site_id_list (you should have this in your data)
site_id_list <- c("chong_96", "chong_68", "bates_57b", "bks_56", "bks_68", "bks_96", "sandhu_ladnertrunk", "lbf_ladnertrunk", "avtar_168", "surjit_168", "gwb_neaves", "bks_neaves")

# Placeholder for the final result (list of data frames for each site)
combined_results2 <- list()

# Loop through each site
for (site in site_id_list) {
  
  # Filter the data for the current site (replace 'your_data' with your actual dataset)
  site_data <- bee2 %>% filter(site_id == site)
  
  # Summarize bee species and number of individuals (count of bees for each species)
  flower_summary <- site_data %>%
    group_by(flower_species) %>%
    summarise(num_individuals = n()) %>%
    mutate(site_id = site) %>% # Add the site_id to each row
    ungroup()
  
  # Store the result for the current site
  combined_results2[[site]] <- flower_summary
}

# Combine all results into a single data frame
final_combined_floral_data <- bind_rows(combined_results2)

# Write the combined data to a CSV file
write.csv(final_combined_floral_data, "site_floral_summary.csv", row.names = FALSE)
