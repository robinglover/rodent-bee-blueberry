#load packages
library(dplyr)
library(geosphere)

#read in data
data <- read.csv("06_cleandata/site_info_clean.csv")

mean_within_area_distance <- function(df) {
  
  coords <- df[, c("x", "y")]
  
  dmat <- distm(coords, fun = distHaversine)
  
  mean(dmat[upper.tri(dmat)])
}

# Step 1: compute mean distance per area
area_stats <- data %>%
  group_by(area) %>%
  summarise(
    mean_dist_km = mean_within_area_distance(pick(x, y))
  ) %>%
  ungroup()

# Step 2: compute overall mean + standard error
summary_stats <- area_stats %>%
  summarise(
    overall_mean = mean(mean_dist_km, na.rm = TRUE),
    se = sd(mean_dist_km, na.rm = TRUE) / sqrt(sum(!is.na(mean_dist_km)))
  )

summary_stats
