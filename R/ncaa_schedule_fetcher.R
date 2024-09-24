# Required libraries for data manipulation and web scraping
library(tidyverse)
library(RCurl)

# -----------------------------------------------
# Utility Functions for Data Cleaning and Parsing
# -----------------------------------------------

# Function to remove leading and trailing whitespace
strip_whitespace <- function(x) {
  gsub("\\s*$", "", gsub("^\\s*", "", x))
}

# -----------------------------------------------
# Core Web Scraping Functions
# -----------------------------------------------

# Function to build the URL for a team's schedule based on the team name and year
build_team_url <- function(school, year, base_url) {
  team_id <- team_ids %>%
    filter(team == school)
  team_id <- toString(team_id['id'])
  
  year_id <- year_ids %>%
    filter(year == year)
  year_id <- toString(year_id[2])
  
  team_url <- paste(base_url, "/team/index/", year_id, "?org_id=", team_id, sep = "")
  return(team_url)
}

# Function to extract game IDs from the HTML data
extract_game_id <- function(html_content) {
  game_date_rows <- grep(">\\d\\d/\\d\\d/\\d\\d\\d\\d<", html_content)
  game_results_rows <- game_date_rows + 6
  game_id <- as.numeric(gsub("^.*game/index/([0-9]*).*$", "\\1", html_content[game_results_rows]))
  return(game_id)
}

# Function to extract game results from the HTML data
extract_game_results <- function(html_content) {
  game_date_rows <- grep(">\\d\\d/\\d\\d/\\d\\d\\d\\d<", html_content)
  game_results_rows <- game_date_rows + 6
  game_results <- gsub("<[^<>]*>", "", html_content[game_results_rows])
  game_results <- strip_whitespace(game_results)
  return(game_results)
}

# Function to extract the opponent and game location from the HTML data
extract_game_opponent <- function(html_content) {
  game_date_rows <- grep(">\\d\\d/\\d\\d/\\d\\d\\d\\d<", html_content)
  game_opponent_rows <- game_date_rows + 3
  game_opponent <- gsub("<[^<>]*>", "", html_content[game_opponent_rows])
  game_opponent <- strip_whitespace(game_opponent)
  return(game_opponent)
}

# Function to extract the game date from the HTML data
extract_game_date <- function(html_content) {
  game_date_rows <- grep(">\\d\\d/\\d\\d/\\d\\d\\d\\d<", html_content)
  game_dates <- strip_whitespace(gsub("<[^<>]*>", "", html_content[game_date_rows]))
  return(game_dates)
}

# -----------------------------------------------
# Main Function to Get Team Schedule for a Specific Year
# -----------------------------------------------

# Fetches the schedule for a specific team for the given year
fetch_team_schedule <- function(team_name, year, division = 1) {
  year_ids <- get_year_ids(year, year, division)
  team_ids <- get_team_ids(year, year, division)
  
  team <- toString(team_name)
  team_id <- toString(team_ids$id[grep(team_name, team_ids$team)])
  
  year <- toString(year)
  year_id <- toString(year_ids$year_id[grep(year, year_ids$year)])
  
  # Construct the team URL
  team_url <- paste(base_url, "/team/index/", year_id, "?org_id=", team_id, sep = "")
  
  # Attempt to fetch data with a custom User-Agent to avoid blocking
  html_content <- try(getURL(team_url, .opts = list(useragent = "Mozilla/5.0")), silent = TRUE)
  
  # Handle connection error
  if (class(html_content) == 'try-error') {
    print(paste('Cannot connect to server for', team_name, 'in', year))
  } else {
    # Check if the team played any games in the selected year
    if (length(grep(">\\d\\d/\\d\\d/\\d\\d\\d\\d<", html_content)) == 0) {
      print(paste(team_name, 'did not play any games in', year))
    } else {
      # Extract game data from the HTML content
      game_results <- extract_game_results(html_content)
      game_dates <- extract_game_date(html_content)
      game_opponent <- extract_game_opponent(html_content)
      game_id <- as.numeric(extract_game_id(html_content))
      
      # Compile all data into a data frame
      team_schedule <- data.frame(
        year = year, date = game_dates, game_id = game_id,
        team = team_name, opponent_location = game_opponent, result = game_results,
        stringsAsFactors = FALSE
      ) %>%
        mutate(
          location = case_when(
            grepl("^@", opponent_location) ~ "Away",
            grepl(" @ ", opponent_location, fixed = TRUE) ~ "Neutral",
            TRUE ~ "Home"
          ),
          opponent = case_when(
            location == "Away" ~ gsub("@", "", opponent_location),
            location == "Home" ~ opponent_location,
            location == "Neutral" ~ substring(opponent_location, 1, regexpr("@", opponent_location) - 2)
          )
        ) %>%
        select(year, date, game_id, team, opponent, location, result)
    }
  }
  return(team_schedule)
}

# -----------------------------------------------
# Main Function to Get Season Schedule for All Teams
# -----------------------------------------------

# Fetches the schedule for all teams over a range of years
fetch_season_schedule <- function(year_start, year_end, division = 1) {
  if (year_start > year_end) {
    print('End year must be the same as, or later than, the starting year')
    return(NULL)
  }
  
  year_ids <- get_year_ids(year_start, year_end, division)
  team_ids <- get_team_ids(year_start, year_end, division)
  
  # Initialize an empty data frame to store the entire season schedule
  full_schedule <- NULL
  
  # Loop through each team and year
  for (i in 1:nrow(team_ids)) {
    for (j in 1:nrow(year_ids)) {
      team <- toString(team_ids[i, 'team'])
      team_id <- toString(team_ids[i, 'id'])
      
      year <- toString(year_ids[j, 'year'])
      year_id <- toString(year_ids[j, 'year_id'])
      
      # Build the URL for the team and year
      team_url <- paste(base_url, "/team/index/", year_id, "?org_id=", team_id, sep = "")
      
      # Attempt to fetch the data with a custom User-Agent
      html_content <- try(getURL(team_url, .opts = list(useragent = "Mozilla/5.0")), silent = TRUE)
      
      # Handle connection errors
      if (class(html_content) == 'try-error') {
        print(paste('Cannot connect to server for', team, 'in', year))
      } else {
        if (length(grep(">\\d\\d/\\d\\d/\\d\\d\\d\\d<", html_content)) == 0) {
          print(paste(team, 'did not play any games in', year))
        } else {
          # Extract game data and compile into a data frame
          game_results <- extract_game_results(html_content)
          game_dates <- extract_game_date(html_content)
          game_opponent <- extract_game_opponent(html_content)
          game_id <- as.numeric(extract_game_id(html_content))
          
          team_schedule <- data.frame(
            year = year, date = game_dates, game_id = game_id,
            team = team, opponent_location = game_opponent, result = game_results,
            stringsAsFactors = FALSE
          ) %>%
            mutate(
              location = case_when(
                grepl("^@", opponent_location) ~ "Away",
                grepl(" @ ", opponent_location, fixed = TRUE) ~ "Neutral",
                TRUE ~ "Home"
              ),
              opponent = case_when(
                location == "Away" ~ gsub("@", "", opponent_location),
                location == "Home" ~ opponent_location,
                location == "Neutral" ~ substring(opponent_location, 1, regexpr("@", opponent_location) - 2)
              )
            ) %>%
            select(year, date, game_id, team, opponent, location, result)
          
          # Combine the current team schedule with the full season schedule
          if (is.null(full_schedule)) {
            full_schedule <- team_schedule
          } else {
            full_schedule <- rbind(full_schedule, team_schedule)
          }
        }
      }
    }
  }
  return(full_schedule)
}

# Example usage:
# team_schedule <- fetch_team_schedule('School Name', 2022)
# season_schedule <- fetch_season_schedule(2022, 2022)
