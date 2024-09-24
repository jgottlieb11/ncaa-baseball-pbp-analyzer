# Load required libraries
library(dplyr)

# -------------------------------------------------
# Unique Game IDs and Starting Pitchers
# -------------------------------------------------

# Step 1: Extract unique game IDs
# This step retrieves the unique game identifiers for the dataset.
get_unique_game_ids <- function(data) {
  unique(data$GAME_ID)
}

# Step 2: Find the starting pitchers
# Starting pitchers are identified by filtering for the first inning and the leadoff flag.
find_starting_pitchers <- function(data) {
  data %>%
    filter(INN_CT == 1, LEADOFF_FL == 'TRUE') %>%
    select(GAME_ID, PIT_ID)
}

# Step 3: Get starting pitchers for a specific game
get_starting_pitchers_for_game <- function(starting_pitchers, game_id) {
  starting_pitchers %>%
    filter(GAME_ID == game_id)
}

# -------------------------------------------------
# Process Games and Assign Reliever (RP) Labels
# -------------------------------------------------

# Step 4: Add reliever flag to a specific game
# This function adds a column to indicate whether the pitcher is a reliever.
add_reliever_flag <- function(data, game_id, starting_pitchers) {
  specific_sp <- get_starting_pitchers_for_game(starting_pitchers, game_id)
  
  data %>%
    filter(GAME_ID == game_id) %>%
    mutate(
      RP = case_when(
        PIT_ID %in% specific_sp$PIT_ID ~ 0,  # If pitcher is a starter, RP = 0
        TRUE ~ 1  # Otherwise, RP = 1 (reliever)
      )
    )
}

# Step 5: Process all games and append results
# This function loops through all games and appends the RP labels to the entire dataset.
process_all_games <- function(data, game_ids, starting_pitchers) {
  # Initialize with the first game
  game_data <- add_reliever_flag(data, game_ids[1], starting_pitchers)
  
  # Loop through the remaining games and bind the results
  for (i in 2:length(game_ids)) {
    game_result <- add_reliever_flag(data, game_ids[i], starting_pitchers)
    game_data <- rbind(game_data, game_result)
  }
  
  return(game_data)
}

# -------------------------------------------------
# Main Execution Flow
# -------------------------------------------------

# Main function to process the dataset
process_dataset_with_rp <- function(data) {
  # Extract unique game IDs
  game_ids <- get_unique_game_ids(data)
  
  # Find starting pitchers for all games
  starting_pitchers <- find_starting_pitchers(data)
  
  # Process all games and return the final dataset with reliever flag
  final_data <- process_all_games(data, game_ids, starting_pitchers)
  
  return(final_data)
}

# Example usage:
# final_dataset <- process_dataset_with_rp(d2017)
