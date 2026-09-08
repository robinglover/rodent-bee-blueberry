#load packages
library(tidyverse)
library(stringr)
library(lubridate)

##bee
#read in data
bee <- read.csv("06_cleandata/bees_CLEAN.csv")

#check values in each useful column
unique(bee$site_id) ##good

unique(bee$round) ##needs to be fixed:
bee$date <- mdy(bee$date)
bee <- bee %>%
  mutate(
    round = case_when(
      date <= ymd("2024-05-01") ~ "2024_r1",
      date >= ymd("2024-05-14") & date <= ymd("2024-06-06") ~ "2024_r2",
      date >= ymd("2024-06-18") & date <= ymd("2024-07-03") ~ "2024_r3",
      date >= ymd("2025-04-03") & date <= ymd("2025-04-19") ~ "2025_r1",
      date >= ymd("2025-04-27") & date <= ymd("2025-05-13") ~ "2025_r2",
      date > ymd("2025-05-13") ~ "2025_r3"
    ))
unique(bee$round) ##good

unique(bee$plot_id) ##remove trailing spaces
bee$plot_id <- trimws(bee$plot_id)
unique(bee$plot_id)##good

unique(bee$behaviour) ##remove spaces
bee$behaviour <- trimws(bee$behaviour)
unique(bee$behaviour) ##good

unique(bee$caste) ##remove spaces
bee$caste <- trimws(bee$caste)
unique(bee$caste) ##good

unique(bee$flower_code) ##remove spaces
bee$flower_code <- trimws(bee$flower_code)
unique(bee$flower_code) ##good

unique(bee$flower_species) ##remove spaces
bee$flower_species <- trimws(bee$flower_species)
unique(bee$flower_species) ##good

unique(bee$bee_code) ##remove spaces
bee$bee_code <- trimws(bee$bee_code)
unique(bee$bee_code) ##good

unique(bee$bee_species) ##remove spaces
bee$bee_species <- trimws(bee$bee_species)
unique(bee$bee_species) ##good

write_csv(bee, file = file.path("06_cleandata","bee_clean.csv"))

##floral
floral <- read.csv("06_cleandata/flowers_CLEAN.csv")

#correct date format
floral <- floral %>%
  mutate(date = mdy(date))

#remove white space from all character columns
bee <- bee %>%
  mutate(across(where(is.character), trimws))

#check each column
unique(floral$site_id)
unique(floral$round) #need to assign proper rounds
floral <- floral %>%
  mutate(
    round = case_when(
      date < ymd("2024-05-01") ~ "2024_r1",
      date >= ymd("2024-05-14") & date <= ymd("2024-06-06") ~ "2024_r2",
      date >= ymd("2024-06-12") & date <= ymd("2024-07-03") ~ "2024_r3",
      date >= ymd("2024-07-10") & date <= ymd("2024-08-14") ~ "2024_r4",
      date >= ymd("2025-04-03") & date <= ymd("2025-04-24") ~ "2025_r1",
      date >= ymd("2025-04-27") & date <= ymd("2025-05-30") ~ "2025_r2",
      date > ymd("2025-05-30") ~ "2025_r3"
    ))
unique(floral$round) ##good
unique(floral$plot_id) ##good
unique(floral$flower_code) ##good
unique(floral$flower_species) ##replace "ask tyler" with blank and fix formatting of species names
floral <- floral %>%
  mutate(flower_species = if_else(flower_species == "ask tyler", "", flower_species)) %>%
  mutate(flower_species = if_else(flower_species == "myosotis stricta ", "myosotis_stricta", flower_species)) %>%
  mutate(flower_species = if_else(flower_species == "arabidopsis _thaliana", "arabidopsis_thaliana", flower_species))
unique(floral$flower_species)##good
unique(floral$abundance) ##good

write_csv(floral, file = file.path("06_cleandata","floral_clean.csv"))

##rodent
rodent <- read.csv("06_cleandata/rodents_CLEAN.csv")

#correct date format
rodent$date_day <- dmy(rodent$date_day)

#remove white space from all character columns
rodent <- rodent %>%
  mutate(across(where(is.character), trimws))

#check each column
unique(rodent$site_id)
unique(rodent$round) ##assign rounds
rodent <- rodent %>%
  mutate(
    round = case_when(
      date_day < ymd("2024-06-08") ~ "2024_r1",
      date_day >= ymd("2024-06-09") & date_day <= ymd("2024-10-01") ~ "2024_r4"))
unique(rodent$round) ##good
unique(rodent$plot_id) ##good
unique(rodent$diameter_cm) ##good
unique(rodent$powder_active_FINAL) ##good
unique(rodent$powder_species_guess_FINAL) ##good

write_csv(rodent, file = file.path("06_cleandata","rodent_clean.csv"))


##veg
veg <- read.csv("06_cleandata/vegetation_CLEAN.csv")


#correct date format
veg$date <- dmy(veg$date) 

#remove white space from all character columns
veg <- veg %>%
  mutate(across(where(is.character), trimws))

#check each character column
unique(veg$site_id) ##good
unique(veg$round) ##assign rounds
veg <- veg %>%
  mutate(
    round = case_when(
      date < ymd("2024-05-01") ~ "2024_r1",
      date >= ymd("2024-05-14") & date <= ymd("2024-06-06") ~ "2024_r2",
      date >= ymd("2024-06-12") & date <= ymd("2024-07-03") ~ "2024_r3",
      date >= ymd("2024-07-10") & date <= ymd("2024-08-14") ~ "2024_r4",
      date >= ymd("2025-04-03") & date <= ymd("2025-04-24") ~ "2025_r1",
      date >= ymd("2025-04-27") & date <= ymd("2025-05-30") ~ "2025_r2",
      date > ymd("2025-05-30") ~ "2025_r3"
    ))
unique(veg$round) ##good
unique(veg$plot_id) ##good

#check structure of rest of dataset
str(veg) ##change grass height to numeric
veg <- veg %>%
  mutate(grass_height = as.numeric(grass_height))
str(veg) ##good

write_csv(veg, file = file.path("06_cleandata","veg_clean.csv"))

##site info
site_info <- read.csv("00_rawdata/exploratory/site-info.csv")

#remove white space from all character columns
site_info <- site_info %>%
  mutate(across(where(is.character), trimws))

#check each column
unique(site_info$name) ##good
unique(site_info$area) ##good
unique(site_info$grass) ##good

#rename columns to match other datasets
site_info <- site_info %>%
  rename(site_id = name)
site_info <- site_info %>%
  rename(cover_type = grass)

write_csv(site_info, file = file.path("06_cleandata","site_info_clean.csv"))

