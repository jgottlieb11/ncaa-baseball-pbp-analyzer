# Load required libraries
library(dplyr)

# -----------------------------------------------
# Function to Group Data and Calculate Expected Runs
# -----------------------------------------------

# Step 1: Create a data frame with base count, outs, and remaining runs
create_runs_dataframe <- function(base_cd, outs, runs_rest_of_inn) {
  data_frame(base_cd, outs, runs_rest_of_inn)
}

# Step 2: Group the data by base count and outs
group_by_base_and_outs <- function(data) {
  data %>%
    group_by(base_cd, outs)
}

# Step 3: Summarize data to calculate the mean of runs remaining in the inning
calculate_mean_runs <- function(grouped_data) {
  grouped_data %>%
    summarize(ERV = round(mean(runs_rest_of_inn), 3))
}

# Step 4: Ungroup the data and create a state identifier
create_state_identifier <- function(summarized_data) {
  summarized_data %>%
    ungroup() %>%
    mutate(state = paste(base_cd, outs, sep = ' '))
}

# -----------------------------------------------
# Function to Convert to Matrix
# -----------------------------------------------

# Step 5: Convert the summarized expected runs data into a matrix
convert_to_matrix <- function(data) {
  matrix_data <- matrix(data$ERV, ncol = 3, byrow = TRUE)
  
  # Assign row and column names
  rownames(matrix_data) <- c('_ _ _', 'X _ _', '_ X _', 'X X _', '_ _ X', 'X _ X', '_ X X', 'X X X')
  colnames(matrix_data) <- c('0', '1', '2')
  
  return(matrix_data)
}

# -----------------------------------------------
# Main Function to Get Expected Runs Matrix
# -----------------------------------------------

# This function takes the base count, outs, and runs remaining in the inning
# and returns an expected runs matrix.
calculate_expected_runs_matrix <- function(base_cd, outs, runs_rest_of_inn) {
  # Step 1: Create a dataframe from input vectors
  runs_data <- create_runs_dataframe(base_cd, outs, runs_rest_of_inn)
  
  # Step 2: Group the data by base count and outs
  grouped_data <- group_by_base_and_outs(runs_data)
  
  # Step 3: Calculate the mean expected runs for each base-out state
  summarized_data <- calculate_mean_runs(grouped_data)
  
  # Step 4: Create state identifiers
  final_data <- create_state_identifier(summarized_data)
  
  # Step 5: Convert the data into a matrix format
  expected_runs_matrix <- convert_to_matrix(final_data)
  
  # Return the final expected runs matrix
  return(expected_runs_matrix)
}

# Example usage:
# base_cd <- c(0, 1, 1, 0, 1, 0, 1, 1)
# outs <- c(0, 1, 2, 0, 1, 0, 2, 1)
# runs_rest_of_inn <- c(0.5, 1.2, 0.8, 2.3, 1.0, 0.0, 1.5, 0.7)
# expected_matrix <- calculate_expected_runs_matrix(base_cd, outs, runs_rest_of_inn)
# print(expected_matrix)
