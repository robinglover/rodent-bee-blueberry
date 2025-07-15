library(tidyverse)
library(stringr)

#load Jenna's floral data
jenna_flowers <- read.csv("06_cleandata/floral_interaction_data/bombusflowers_Jenna.csv")

#load Duncan's and Carly's data
DC_flowers <- read.csv("06_cleandata/floral_interaction_data/bombus_flower_Duncan_Carly.csv")
DC_flowers <- rename(DC_flowers, V1 = plant_sp)

# Filter rows in DC_only where V1 is not already in jenna_flowers
new_rows <- DC_flowers%>%
  filter(!V1 %in% jenna_flowers$V1)

# Bind those rows in DC_only to jenna_flowers
JDC_flowers <- bind_rows(jenna_flowers, new_rows)

#create new column with proper species name format 
JDC_flowers <- JDC_flowers %>%
  mutate(flower_species = str_replace_all(tolower(V1), " ", "_"))

#create new column for species code
JDC_flowers <- JDC_flowers %>%
  mutate(
    flower_code = str_to_lower(
      str_c(
        str_sub(word(V1, 1), 1, 2),
        str_sub(word(V1, 2), 1, 2)
      )
    )
  )

#only save useful columns
JDC_flowers <- JDC_flowers %>%
  select(flower_species, flower_code)

#write to new csv
write_csv(JDC_flowers, file = file.path("06_cleandata","useful_flowers.csv"))

#all floral genera that are generally used by bees (include all species under these genera in analyses)
##rhododendron_spp.
##rorippa_spp.
##rubus_spp.
##rosa_sp
##calystegia_sp
##prunus_sp
##phacelia_sp