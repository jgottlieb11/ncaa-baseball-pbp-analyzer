# Required libraries
library(dplyr)
library(stringr)

# -----------------------------------------------
# Utility Functions
# -----------------------------------------------

# Function to remove leading and trailing whitespace
# This utility ensures that any surrounding whitespace is removed.
clean_whitespace <- function(text) {
  gsub("\\s*$", "", gsub("^\\s*", "", text))
}

# Function to remove trailing punctuation
# Specifically designed to remove periods at the end of a string.
clean_punctuation <- function(text) {
  text <- clean_whitespace(text)
  ifelse(str_sub(text, -1) == '.', gsub("\\.", "", text), text)
}

# Helper function to detect inning change
# Checks if the current inning is different from the next one.
is_inning_changed <- function(current_inning, next_inning) {
  if (current_inning != next_inning) {
    return(1)
  } else {
    return(0)
  }
}

# Helper function to detect game change
# Compares the current game ID with the previous one to detect game transitions.
is_game_changed <- function(current_game, previous_game) {
  if (current_game != previous_game) {
    return(1)
  } else {
    return(0)
  }
}

# -----------------------------------------------
# Parsing Functions for Game Events
# -----------------------------------------------

# Function to determine the end of an inning
# Loops through the innings and marks where inning changes occur.
end_inning <- function(top_half) {
  n <- length(top_half)
  inning_end <- integer(n)
  
  # Loop through all but the last inning
  for (i in 1:(n - 1)) {
    inning_end[i] <- is_inning_changed(top_half[i], top_half[i + 1])
  }
  
  # Mark the final inning as the end of the inning
  inning_end[n] <- 1
  return(inning_end)
}

# Function to determine the end of a game
# Loops through game IDs and identifies where a new game begins.
end_game <- function(game_ids) {
  n <- length(game_ids)
  game_end <- integer(n)
  
  # Loop to check if game id changes from the previous one
  for (i in 2:n) {
    game_end[i - 1] <- is_game_changed(game_ids[i], game_ids[i - 1])
  }
  
  # Mark the final play as the end of the game
  game_end[n] <- 1
  return(game_end)
}

# -----------------------------------------------
# Calculating Runs and Play Details
# -----------------------------------------------

# Function to calculate runs scored on a play
# Compares the runs scored between consecutive plays for both teams.
runs_per_play <- function(a_text, h_text, a_score, h_score) {
  n <- length(a_text)
  runs_on_play <- integer(n)
  
  # Initialize with the first away score
  runs_on_play[1] <- a_score[1]
  
  # Loop through the plays to calculate runs based on score changes
  for (i in 2:n) {
    runs_on_play[i] <- case_when(
      a_text[i] == '' ~ as.integer(h_score[i] - h_score[i - 1]),
      TRUE ~ as.integer(a_score[i] - a_score[i - 1])
    )
  }
  return(runs_on_play)
}

# -----------------------------------------------
# Processing Runners on Base
# -----------------------------------------------

# Helper function to handle substitutions for runners
# This function checks if a substitution occurred and updates the runner accordingly.
process_substitution <- function(sub_out, sub_in, previous_runner_name) {
  if (sub_out != '' && sub_out == clean_whitespace(previous_runner_name)) {
    return(sub_in)
  } else {
    return(previous_runner_name)
  }
}

# Function to process the runner on first base
# Determines who is on first base after a play, taking substitutions into account.
process_runner1 <- function(bat_text, bat_name, r1_text, r1_name, inning_end, game_end, sub_in, sub_out) {
  n <- length(bat_text)
  new_r1_name <- character(n)
  
  # Loop through each at-bat
  for (i in 2:n) {
    if (inning_end[i - 1] == 0 & game_end[i - 1] == 0) {
      new_r1_name[i] <- case_when(
        # Handle substitution
        sub_out[i - 1] != '' & sub_out[i - 1] == clean_whitespace(r1_name[i - 1]) ~ sub_in[i - 1],
        # Conditions for advancing runners
        str_detect(bat_text[i - 1], '(singled|walked|hit by pitch|reached)') == TRUE &
          !str_detect(bat_text[i - 1], '(doubled|tripled|homered|advanced|scored|out|stole)') ~ bat_name[i - 1],
        r1_text[i - 1] == '' & str_detect(r1_text[i - 1], '(advanced to second|scored|out)') == FALSE ~ r1_name[i - 1],
        TRUE ~ r1_name[i - 1]
      )
    }
  }
  
  # Return cleaned runner names
  return(clean_whitespace(new_r1_name))
}

# Function to process the runner on second base
# Identifies the player on second base after the play.
process_runner2 <- function(bat_text, bat_name, r1_text, r1_name, r2_text, r2_name, inning_end, game_end, sub_in, sub_out) {
  n <- length(bat_text)
  new_r2_name <- character(n)
  
  # Loop through each at-bat
  for (i in 2:n) {
    if (inning_end[i - 1] == 0 & game_end[i - 1] == 0) {
      new_r2_name[i] <- case_when(
        # Handle substitution
        sub_out[i - 1] != '' & sub_out[i - 1] == clean_whitespace(r2_name[i - 1]) ~ sub_in[i - 1],
        # Handle advancing to second
        str_detect(bat_text[i - 1], '(doubled|advanced to second|stole second)') == TRUE ~ clean_whitespace(gsub('((doubled|advanced).*$)', '', bat_text[i - 1])),
        r2_text[i - 1] == '' ~ r2_name[i - 1],
        TRUE ~ r2_name[i - 1]
      )
    }
  }
  
  # Return cleaned runner names
  return(clean_whitespace(new_r2_name))
}

# Function to process the runner on third base
# Identifies the player on third base after the play.
process_runner3 <- function(bat_text, bat_name, r1_text, r1_name, r2_text, r2_name, r3_text, r3_name, inning_end, game_end, sub_in, sub_out) {
  n <- length(bat_text)
  new_r3_name <- character(n)
  
  # Loop through each at-bat
  for (i in 2:n) {
    if (inning_end[i - 1] == 0 & game_end[i - 1] == 0) {
      new_r3_name[i] <- case_when(
        # Handle substitution
        sub_out[i - 1] != '' & sub_out[i - 1] == clean_whitespace(r3_name[i - 1]) ~ sub_in[i - 1],
        # Handle advancing to third
        str_detect(bat_text[i - 1], '(tripled|advanced to third|stole third)') == TRUE ~ clean_whitespace(gsub('((tripled|advanced).*$)', '', bat_text[i - 1])),
        r3_text[i - 1] == '' ~ r3_name[i - 1],
        TRUE ~ r3_name[i - 1]
      )
    }
  }
  
  # Return cleaned runner names
  return(clean_whitespace(new_r3_name))
}

# -----------------------------------------------
# Handling Game and Inning Transitions
# -----------------------------------------------

# Function to determine if it's a new game
# Detects game transitions based on game-ending flags.
is_new_game <- function(game_end) {
  n <- length(game_end)
  new_game <- integer(n)
  new_game[1] <- 1
  
  # Loop through each play and check if a new game starts
  for (i in 2:n) {
    new_game[i] <- game_end[i - 1]
  }
  return(new_game)
}

# Function to determine if it's a new inning
# Detects inning transitions based on inning-ending flags.
is_new_inning <- function(inning_end) {
  n <- length(inning_end)
  new_inning <- integer(n)
  new_inning[1] <- 1
  
  # Loop through each play and check if a new inning starts
  for (i in 2:n) {
    new_inning[i] <- inning_end[i - 1]
  }
  return(new_inning)
}

# -----------------------------------------------
# Batting Order Management
# -----------------------------------------------

# Function to fill in missing batting order slots
# This function ensures that missing values in the batting order are filled in.
fill_batting_order <- function(bat_order, game_end) {
  n <- length(bat_order)
  
  # Fill missing batting order entries backward (from the end)
  for (i in n:2) {
    if (is.na(bat_order[i - 1]) & game_end[i - 1] == 0) {
      bat_order[i - 1] <- bat_order[i]
    }
  }
  
  # Fill missing batting order entries forward (from the start)
  for (i in 2:n) {
    if (is.na(bat_order[i])) {
      bat_order[i] <- bat_order[i - 1]
    }
  }
  
  return(bat_order)
}

##########################################################

# Main parsing function for play-by-play data
# This function processes the play-by-play data, identifying substitution events,
# inning and game transitions, and calculating runner positions.
parse_pbp_data <- function(pbp_data_frame) {
  pbp_data_frame <- pbp_data_frame %>%
    mutate(
      # Combine away and home text into a single column for easier parsing
      combined_text = paste(away_text, home_text),
      
      # Identify substitution events
      substitution_flag = case_when(
        str_detect(combined_text, '(singled|grounded out|walked|reached)') ~ 0,
        TRUE ~ 1
      ),
      
      # Compute inning and game end flags
      inning_end_flag = end_inning(top_inning),
      game_end_flag = end_game(game_id),
      
      # Process runners on bases
      runner_1 = process_runner1(bat_text, bat_name, r1_text, r1_name, inning_end_flag, game_end_flag, sub_in, sub_out),
      runner_2 = process_runner2(bat_text, bat_name, r1_text, r1_name, r2_text, r2_name, inning_end_flag, game_end_flag, sub_in, sub_out),
      runner_3 = process_runner3(bat_text, bat_name, r1_text, r1_name, r2_text, r2_name, r3_text, r3_name, inning_end_flag, game_end_flag, sub_in, sub_out)
    ) %>%
    select(year, date, game_id, away_team, home_team, inning, top_inning, away_score, home_score, runner_1, runner_2, runner_3)
  
  return(pbp_data_frame)
}

# Example usage:
# parsed_data <- parse_pbp_data(pbp_raw_data)
