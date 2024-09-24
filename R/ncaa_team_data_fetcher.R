# Required libraries
library(tidyverse)

# -----------------------------------------------
# Utility Functions for Cleaning and Formatting
# -----------------------------------------------

# Function to strip leading and trailing whitespace
clean_whitespace <- function(text) {
  gsub("\\s*$", "", gsub("^\\s*", "", text))
}

# -----------------------------------------------
# Core Data Fetching Functions
# -----------------------------------------------

# Function to retrieve the year IDs for a given range of years and division
get_year_ids <- function(year_start, year_end, division = 1) {
  if (year_start > year_end) {
    print('End year must be the same as, or later than, starting year')
    return(NULL)
  } else {
    # Initialize an empty vector to store year ranges
    year_range <- toString(year_start)
    
    # Loop to add all years from start to end
    for (i in (year_start + 1):year_end) {
      year_range <- c(year_range, toString(i))
    }
    
    # Initialize a data frame to store year IDs
    year_ids <- data.frame()
    
    # Scrape team and year IDs for the specified range of years
    for (year in year_range) {
      # Construct the URL for the specified division and year
      year_division_url <- paste("http://stats.ncaa.org/team/inst_team_list?sport_code=MBA&academic_year=", year, "&division=", division, "&conf_id=-1&schedule_date=", sep = '')
      
      # Scan the URL content
      html_content <- scan(year_division_url, what = "", sep = "\n")
      
      # Extract the lines containing team links
      team_links <- grep("/team/[0-9]*/", html_content)
      html_content <- html_content[team_links]
      
      # Extract the year ID using regular expressions
      year_id <- as.numeric(gsub("^.*team/([0-9]*)/([0-9]*).*$", "\\2", html_content))[1]
      year_info <- data.frame(year = year, year_id = year_id, stringsAsFactors = FALSE)
      
      # Combine the current year info with the previous data
      if (nrow(year_ids) == 0) {
        year_ids <- year_info
      } else {
        year_ids <- rbind(year_ids, year_info)
      }
    }
    
    return(year_ids)
  }
}

# Function to retrieve team IDs for a given range of years and division
get_team_ids <- function(year_start, year_end, division = 1) {
  if (year_start > year_end) {
    print('End year must be the same as, or later than, starting year')
    return(NULL)
  } else {
    # Initialize an empty vector to store year ranges
    year_range <- toString(year_start)
    
    for (i in year_start:year_end) {
      year_range <- c(year_range, toString(i))
    }
    
    # Initialize a data frame to store team IDs
    team_ids <- data.frame()
    
    # Scrape team and year IDs within the specified range
    for (year in year_range) {
      # Construct the URL for each year and division
      year_division_url <- paste("http://stats.ncaa.org/team/inst_team_list?sport_code=MBA&academic_year=", year, "&division=", division, "&conf_id=-1&schedule_date=", sep = '')
      
      # Scan the content of the URL
      html_content <- scan(year_division_url, what = "", sep = "\n")
      
      # Extract the relevant team link lines
      team_links <- grep("/team/[0-9]*/", html_content)
      html_content <- html_content[team_links]
      
      # Extract team IDs and names from the HTML content
      team_id <- as.numeric(gsub("^.*team/([0-9]*)/([0-9]*).*$", "\\1", html_content))
      team_name <- gsub("<[^<>]*>", "", html_content)
      team_name <- clean_whitespace(team_name)
      
      # Create a data frame for the current year, team name, and team ID
      team_info <- data.frame(year = year, team = team_name, id = team_id, stringsAsFactors = FALSE) %>%
        mutate(
          team = gsub("&#x27;", "'", team),   # Replace HTML encoding for apostrophes
          team = gsub("&amp;", "&", team)     # Replace HTML encoding for ampersands
        )
      
      # Combine the current team info with the previous data
      if (nrow(team_ids) == 0) {
        team_ids <- team_info
      } else {
        team_ids <- rbind(team_ids, team_info)
      }
    }
    
    # Remove duplicate team IDs and find the last year of existence for each team
    team_ids <- team_ids %>%
      group_by(team, id) %>%
      summarize(last_year = max(as.integer(year)))
    
    return(team_ids)
  }
}

# -----------------------------------------------
# Example Usage
# -----------------------------------------------

# Example usage of the functions to get team and year IDs
# year_ids <- get_year_ids(2020, 2022)
# team_ids <- get_team_ids(2020, 2022)
