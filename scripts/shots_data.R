# Load the tidyverse library to be able to use %>% and dplyr to wrangle
library(tidyverse)

# Load the artificial shot data
shots <- readRDS(file = "data/shots.rds")

# Display the variable types and the general structure of the data
str(shots) # variable type
summary(shots) # Summary statistics

# Display the first few rows
head(shots)

# Add a few variables and clean others
shots <- shots %>%
  # Convert shots to a tibble format
  tibble() %>%
  # Add Columns
  mutate(
    # convert players to a factor
    player =  factor(
      player, 
      # Re-level P1, P2, ..., P18
      levels = paste("Player", 1:length(unique(shots$player)))
    ),
    # Create  a factor binary variable for whether the shot was made or not
    shot_made_factor = recode_factor(factor(shot_made_numeric),
                                     "0" = "Miss", 
                                     "1" = "Make"
    )
  )

# Display the first few rows
head(shots)

# Define FIBA court width and y-coordinate of hoop center in meters
width <- 15 
hoop_center_y <- 1.575

# Calculate the shot distances
shots <- shots %>%
  # Add Columns
  mutate(
    dist_meters = sqrt((loc_x-width/2)^2 + (loc_y-hoop_center_y)^2),
    dist_feet = dist_meters * 3.28084
  )

# Display the first few rows
head(shots)

# Calculate the shot angles
shots <- shots %>%
  # Add Columns
  mutate(
    theta_rad = case_when(
      # Quadrant 1: Shots from left side higher than the rim
      loc_x > width/2 & loc_y > hoop_center_y ~ atan((loc_x-width/2)/(loc_y-hoop_center_y)),
      # Quadrant 2: Shots from right side higher than the rim
      loc_x < width/2 & loc_y > hoop_center_y ~ atan((width/2-loc_x)/(loc_y-hoop_center_y)),
      # Quadrant 3: Shots from right side lower than the rim
      loc_x < width/2 & loc_y < hoop_center_y ~ atan((hoop_center_y-loc_y)/(width/2-loc_x))+(pi/2),
      # Quadrant 4: Shots from left side lower than the rim
      loc_x > width/2 & loc_y < hoop_center_y ~ atan((hoop_center_y-loc_y)/(loc_x-width/2))+(pi/2),
      # Special Cases
      loc_x == width/2  & loc_y >= hoop_center_y ~ 0, # Directly centered front
      loc_x == width/2  & loc_y < hoop_center_y ~ pi, # Directly centered back
      loc_y == hoop_center_y ~ pi/2, # Directly parallel to hoop center
    ),
    # Make the angle negative if the shot is on the left-side
    theta_rad = ifelse(loc_x > width/2, -theta_rad, theta_rad),
    # Convert the angle from radians to degrees
    theta_deg = theta_rad * (180/pi)
  )

# Display the first few rows
head(shots)

# Save the augmented data
saveRDS(shots, file = "data/shots_augmented.rds")