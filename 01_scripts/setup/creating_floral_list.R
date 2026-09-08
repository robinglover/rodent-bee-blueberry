library(tidyverse)
library(stringr)

#load Jenna's floral data
jenna_flowers <- read.csv("06_cleandata/floral_interaction_data/bombusflowers_Jenna.csv")
jenna_flowers <-jenna_flowers%>%
  mutate(flower_species = str_replace_all(tolower(V1), " ", "_"))

#load Duncan's and Carly's data
DC_flowers <- read.csv("06_cleandata/floral_interaction_data/bombus_flower_Duncan_Carly.csv")
DC_flowers <- rename(DC_flowers, V1 = plant_sp)
DC_flowers <-DC_flowers%>%
  mutate(flower_species = str_replace_all(tolower(V1), " ", "_"))

#create list of flowers that bees visited in my study
robin_floral <- read.csv("06_cleandata/bees_CLEAN.csv")
robin_flower_species <- unique(robin_floral$flower_species)
#trim whitespace
robin_flower_species <- trimws(robin_flower_species)
#remove blanks
robin_flower_species <- robin_flower_species[robin_flower_species != ""]
#Convert to data frame first
robin_flowers <- data.frame(flower_species = robin_flower_species)

# Filter rows in DC_flowers where flower_species is not already in jenna_flowers
new_rows_DC <- DC_flowers%>%
  filter(!flower_species %in% jenna_flowers$flower_species)

# Bind those rows to jenna_flowers
JDC_flowers <- bind_rows(jenna_flowers, new_rows_DC)

# Filter rows in robin_flowers where flower_species is not already in JDC_flowers
new_rows_rg <- robin_flowers%>%
  filter(!flower_species %in% JDC_flowers$flower_species)

#bind rows to JDC_flowers
JDCR_flowers <- bind_rows(JDC_flowers, new_rows_rg)

#replace spaces with _ if needed
JDCR_flowers <- JDCR_flowers %>%
  mutate(flower_species = str_replace_all(flower_species, " ", "_"))

#change brassicaceae to brassica_sp
JDCR_flowers <- JDCR_flowers %>%
  mutate(flower_species = if_else(flower_species == "brassicaceae", "brassica_sp", flower_species))

#create new column for species code
JDCR_flowers <- JDCR_flowers %>%
  mutate(
    flower_code = str_to_lower(
      str_c(
        str_sub(word(flower_species, 1, sep = "_"), 1, 2),
        str_sub(word(flower_species, 2, sep = "_"), 1, 2)
      )
    )
  )

#only save useful columns
JDCR_flowers <- JDCR_flowers %>%
  select(flower_species, flower_code)

#remove "ask tyler" row
JDCR_flowers <- JDCR_flowers %>%
  filter(flower_species != "ask_tyler")

#remove duplicate rows
JDCR_flowers <- JDCR_flowers %>%
  distinct(flower_species, .keep_all = TRUE)

#change spp. to sp in flower species
JDCR_flowers <- JDCR_flowers %>%
  mutate(flower_species = str_replace_all(flower_species, "_spp\\.?$", "_sp"))

#write to new csv
write_csv(JDCR_flowers, file = file.path("06_cleandata","useful_flowers.csv"))

#find list of all floral genera (sp. or spp.)
problematic_species <- JDCR_flowers %>%
  filter(str_detect(flower_code, "sp"))

#floral genera that are generally used by bees (include all species under these genera in analyses)
##brassica_sp
##rhododendron_sp
##rorippa_sp
##rubus_sp
##rosa_sp
##calystegia_sp
##prunus_sp
##phacelia_sp