# Required libraries
library(tidyverse)
library(XML)
library(stringr)
library(RCurl)

# -----------------------------------------------
# Utility Functions
# -----------------------------------------------

# Function to remove leading and trailing whitespace from a string
strip_whitespace <- function(x) {
  gsub("\\s*$", "", gsub("^\\s*", "", x))
}

# Function to fill in missing score data by propagating the previous score forward
# This ensures that scores are continuous throughout the game.
score_fill <- function(score_in) {
  m <- length(score_in)
  score_in[1] <- 0  # Initialize the first score to 0
  
  # Loop through the scores and fill missing values
  for (i in 2:m) {
    if (is.na(score_in[i])) {
      score_in[i] <- score_in[i - 1]
    }
  }
  return(score_in)
}

# -----------------------------------------------
# Fetch Play-by-Play Data for a Single Team
# -----------------------------------------------

# Function to fetch play-by-play (PBP) data for a specific team
fetch_team_pbp <- function(team, year, division = 1) {
  all_team_games <- get_team_schedule(team, year, division)
  
  # Extract and format the game details for the selected team
  games <- all_team_games %>%
    distinct(game_id, .keep_all = TRUE) %>%
    mutate(
      home_team = ifelse(loc %in% c('H', 'N'), team, opp),
      away_team = ifelse(loc == 'H', opp, team)
    ) %>%
    select(year, date, game_id, home_team, away_team)
  
  # Initialize an empty variable to store the PBP data
  pbp_data <- NULL
  
  # Loop through each game in the team's schedule
  for (k in 1:nrow(games)) {
    base_url <- 'http://stats.ncaa.org/game/play_by_play/'
    game_id <- games$game_id[k]
    year <- games$year[k]
    url <- paste0(base_url, game_id)
    
    # Fetch the HTML table from the website
    raw_html <- getURL(url)
    game_data <- readHTMLTable(raw_html)
    
    # Check if valid play-by-play data is available
    if (length(game_data) < ncol(game_data[[1]]) || length(game_data) == 0) {
      print(paste("Play-by-Play data not available for game", game_id))
      next
    } else {
      # Process the play-by-play data by inning
      for (i in 1:length(game_data)) {
        # Ignore null or invalid tables
        if (!is.null(game_data[[i]]) && ncol(game_data[[i]]) == 3) {
          inning_data <- as.data.frame(game_data[[i]]) %>%
            mutate(
              inning = i,
              game_id = game_id,
              year = year
            ) %>%
            select(year, game_id, inning, everything())
          
          # Combine the inning data into the PBP dataset
          if (is.null(pbp_data)) {
            pbp_data <- inning_data
          } else {
            pbp_data <- rbind(pbp_data, inning_data)
          }
        }
      }
    }
    
    # Post-process the PBP data: fill scores and format columns
    pbp_data <- pbp_data %>%
      mutate(
        away_team = colnames(pbp_data)[4],
        home_team = colnames(pbp_data)[6],
        away_score = as.integer(gsub('-.*', '', Score)),
        home_score = as.integer(gsub('.*-', '', Score)),
        away_score = score_fill(away_score),
        home_score = score_fill(home_score)
      ) %>%
      rename(
        away_text = 4,
        home_text = 6
      ) %>%
      filter(substr(away_text, 1, 3) != 'R: ') %>%
      select(year, game_id, inning, away_team, home_team, away_score, home_score, away_text, home_text)
  }
  
  return(pbp_data)
}

# -----------------------------------------------
# Fetch Play-by-Play Data for an Entire Season
# -----------------------------------------------

# Function to fetch play-by-play data for all games in a season
fetch_season_pbp <- function(year, division = 1) {
  all_season_games <- get_season_schedule(year_start = year, year_end = year, division = 1)
  
  # Extract and format the game details for the entire season
  games <- all_season_games %>%
    distinct(game_id, .keep_all = TRUE) %>%
    mutate(
      home_team = ifelse(loc %in% c('H', 'N'), team, opp),
      away_team = ifelse(loc == 'H', opp, team)
    ) %>%
    select(year, date, game_id, home_team, away_team)
  
  # Initialize an empty variable to store the PBP data
  pbp_data <- NULL
  
  # Loop through each game in the season
  for (k in 1:nrow(games)) {
    base_url <- 'http://stats.ncaa.org/game/play_by_play/'
    game_id <- games$game_id[k]
    year <- games$year[k]
    url <- paste0(base_url, game_id)
    
    # Fetch the HTML table from the website
    raw_html <- getURL(url)
    game_data <- readHTMLTable(raw_html)
    
    # Check if valid play-by-play data is available
    if (length(game_data) < ncol(game_data[[1]]) || length(game_data) == 0) {
      print(paste("Play-by-Play data not available for game", game_id))
      next
    } else {
      # Process the play-by-play data by inning
      for (i in 1:length(game_data)) {
        # Ignore null or invalid tables
        if (!is.null(game_data[[i]]) && ncol(game_data[[i]]) == 3) {
          inning_data <- as.data.frame(game_data[[i]]) %>%
            mutate(
              inning = i,
              game_id = game_id,
              year = year
            ) %>%
            select(year, game_id, inning, everything())
          
          # Combine the inning data into the PBP dataset
          if (is.null(pbp_data)) {
            pbp_data <- inning_data
          } else {
            pbp_data <- rbind(pbp_data, inning_data)
          }
        }
      }
    }
    
    # Post-process the PBP data: fill scores and format columns
    pbp_data <- pbp_data %>%
      mutate(
        away_team = colnames(pbp_data)[4],
        home_team = colnames(pbp_data)[6],
        away_score = as.integer(gsub('-.*', '', Score)),
        home_score = as.integer(gsub('.*-', '', Score)),
        away_score = score_fill(away_score),
        home_score = score_fill(home_score)
      ) %>%
      rename(
        away_text = 4,
        home_text = 6
      ) %>%
      filter(substr(away_text, 1, 3) != 'R: ') %>%
      select(year, game_id, inning, away_team, home_team, away_score, home_score, away_text, home_text)
  }
  
  return(pbp_data)
}

# Example usage:
# team_pbp_data <- fetch_team_pbp('Team Name', 2022)
# season_pbp_data <- fetch_season_pbp(2022)
