#### Setup ####

options(scipen = 999)  # disable scientific notation
rm(list=ls())          # clear memory (removes all the variables from the workspace)
start.time <- Sys.time()# keep track of the time taken to run the script

# Load required packages
if (!require('pacman')) install.packages('pacman'); library(pacman)
# load (install if required) packages from CRAN
p_load("devtools", "dplyr", "scales", "ellipse", "ggplot2", "lazyeval", "igraph", "truncnorm", "ggraph", "reshape2", "knitr", "markdown", "stringr", "dampack", "msce", "approx", "openxlsx", "readxl")


#### List of Functions #### 


#gen_wcc functions is from DARTH package, it is used to calculate the within-cycle correction weights
gen_wcc <- function(n_cycles, method = c("Simpson1/3", "half-cycle", "none")){
  if(n_cycles <= 0){
    stop("Number of cycles should be positive")
  }
  
  method <- match.arg(method)
  
  n_cycles <- as.integer(n_cycles)
  
  if (method == "Simpson1/3"){
    ## Vector with cycles
    v_cycles <- seq(1, n_cycles + 1)
    ## Generate 2/3 and 4/3 multipliers for even and odd entries, respectively
    v_wcc <- ((v_cycles %% 2)==0)*(2/3) + ((v_cycles %% 2)!=0)*(4/3)
    ## Substitute 1/3 in first and last entries
    v_wcc[1] <- v_wcc[n_cycles + 1] <- 1/3
  }
  if (method == "half-cycle"){
    ## Initialize within-cycle correction vector
    v_wcc <- rep(1, n_cycles + 1)
    ## Within-cycle correction weights for first and last cycle
    v_wcc[1] <- v_wcc[n_cycles + 1] <- 0.5
  }
  if (method == "none"){
    ## Initialize within-cycle correction vector
    v_wcc <- rep(1, n_cycles + 1)
  }
  return(v_wcc)
}

# This function checks if there are NA values in a dataframe and returns the row and column indices of the NA values
check_NA <- function(df) {
  # Check for NA values in df
  na_check <- is.na(df)
  # Get the row and column indices of NA values
  na_indices <- which(na_check, arr.ind = TRUE)
  return(na_indices)
}

# This function is used to clean the NLST dataset and keep only the relevant columns useful for the PLCOm2012 score calculations
PLCO_Analysis <- function(NLST, check_NA) {
  
  
  # Sub-setting NLST dataset for PLCO
  
  PLCO_NLST <- NLST[NLST$conflc %in% c(1, 2), c("conflc", "age", "age_quit", "race", "ethnic", "educat", "height", "weight", "diagcopd", "cancblad", "cancbrea", "canccerv", "canccolo", "cancesop", "canckidn", "canclary", "cancnasa", "cancoral", "cancpanc", "cancphar", "cancstom", "cancthyr", "canctran", "canclung", "fambrother", "famchild", "famfather", "fammother", "famsister", "cigsmok", "smokeday", "pkyr", "smokeyr")]
  PLCO_NLST[c("cancblad", "cancbrea", "canccerv", "canccolo", "cancesop", "canckidn", "canclary", "cancnasa", "cancoral", "cancpanc", "cancphar", "cancstom", "cancthyr", "canctran", "canclung")] <- lapply(PLCO_NLST[c("cancblad", "cancbrea", "canccerv", "canccolo", "cancesop", "canckidn", "canclary", "cancnasa", "cancoral", "cancpanc", "cancphar", "cancstom", "cancthyr", "canctran", "canclung")], function(x) replace(x, is.na(x), 0))
  PLCO_NLST$cancer_hist <- as.integer(rowSums(PLCO_NLST[c("cancblad", "cancbrea", "canccerv", "canccolo", "cancesop", "canckidn", "canclary", "cancnasa", "cancoral", "cancpanc", "cancphar", "cancstom", "cancthyr", "canctran", "canclung")]) > 0)
  PLCO_NLST[c("fambrother", "famchild", "famfather", "fammother", "famsister")] <- lapply(PLCO_NLST[c("fambrother", "famchild", "famfather", "fammother", "famsister")], function(x) replace(x, is.na(x), 0))
  PLCO_NLST$family_hist <- as.integer(rowSums(PLCO_NLST[c("fambrother", "famchild", "famfather", "fammother", "famsister")]) > 0)
  PLCO_NLST <- PLCO_NLST[ , !(names(PLCO_NLST) %in% c("cancblad", "cancbrea", "canccerv", "canccolo", "cancesop", "canckidn", "canclary", "cancnasa", "cancoral", "cancpanc", "cancphar", "cancstom", "cancthyr", "canctran", "canclung", "fambrother", "famchild", "famfather", "fammother", "famsister"))]
  
  PLCO_NLST$height_meters <- PLCO_NLST$height * 0.0254
  PLCO_NLST$weight_kg <- PLCO_NLST$weight * 0.453592
  PLCO_NLST$BMI <- PLCO_NLST$weight_kg / (PLCO_NLST$height_meters * PLCO_NLST$height_meters)
  PLCO_NLST <- PLCO_NLST[ , !(names(PLCO_NLST) %in% c("height", "weight", "height_meters", "weight_kg"))]
  
  PLCO_NLST$smoking_quit_time <- PLCO_NLST$age - PLCO_NLST$age_quit
  PLCO_NLST[c("smoking_quit_time")] <- lapply(PLCO_NLST[c("smoking_quit_time")], function(x) replace(x, is.na(x), 0))  # replace NA with 0
  PLCO_NLST <- PLCO_NLST[ , !(names(PLCO_NLST) %in% c("age_quit"))]
  PLCO_NLST <- PLCO_NLST[, c("age", "race", "ethnic", "educat", "BMI", "diagcopd", "cancer_hist", "family_hist", "cigsmok", "smokeday", "smokeyr", "smoking_quit_time", "pkyr")]
  PLCO_NLST$educat <- ifelse(PLCO_NLST$educat %in% 1:2, 1, ifelse(PLCO_NLST$educat == 3, 2, ifelse(PLCO_NLST$educat == 4, 3, ifelse(PLCO_NLST$educat == 5, 4, ifelse(PLCO_NLST$educat %in% 6:7, 5, NA)))))
  
  PLCO_NLST$race <- ifelse(PLCO_NLST$race == 1, 1, ifelse(PLCO_NLST$race == 2, 2, ifelse(PLCO_NLST$race == 3, 4, ifelse(PLCO_NLST$race == 4, 1, ifelse(PLCO_NLST$race == 5, 5, ifelse(PLCO_NLST$ethnic == 1, 3, PLCO_NLST$race))))))
  PLCO_NLST <- PLCO_NLST[ , !(names(PLCO_NLST) %in% c("ethnic"))]
  PLCO_NLST <- PLCO_NLST[PLCO_NLST$race <= 5, ]
  PLCO_NLST <- PLCO_NLST[PLCO_NLST$smoking_quit_time >= 0, ]
  PLCO_NLST <- PLCO_NLST[ , !(names(PLCO_NLST) %in% c("age"))]
  n_i            <- 20000 # specify the number of individuals that we want to generate in the simulation
  df_X_PLCO <- data.frame(matrix(ncol = ncol(PLCO_NLST), nrow = n_i))  # Create a new dataframe with n_i rows
  names(df_X_PLCO) <- names(PLCO_NLST)  # Set the column names to be the same as the original dataframe
  
  # Loop over each column in the PLCO_NLST dataframe and sample new values based on those in the original dataframe
  for (col_name in names(PLCO_NLST)) {
    # Get the column
    col <- PLCO_NLST[[col_name]]
    
    # Get the non-NA values
    non_na_values <- col[!is.na(col)]
    
    # Sample non-NA values to create new column
    new_col <- sample(non_na_values, n_i, replace = TRUE)
    
    # Replace column in the new dataframe
    df_X_PLCO[[col_name]] <- new_col
  }
  
  
  check_NA(df_X_PLCO)
  
  
  PLCO_results <- list(PLCO_NLST = PLCO_NLST, df_X_PLCO = df_X_PLCO, n_i = n_i)
  return(PLCO_results) # return the cleaned results
  
}

# This function is used to convert Hazard Functions to probabilities
convert_hazard_to_probability <- function(r) {
  1 - exp(-r*cycle_length) # rate to probability formula
}

# this function checks for valid inputs before running the model
check_inputs <- function(n_i, df_X, seed, cycle_length) {
  
  if (!is.numeric(n_i) || n_i <= 0 || round(n_i) != n_i) {
    stop("n_i must be a positive integer.")
  }
  if (!is.data.frame(df_X)) {
    stop("df_X must be a data frame.")
  }
  if (!is.numeric(seed) || seed <= 0 || round(seed) != seed) {
    stop("seed must be a positive integer.")
  }
  if (!is.numeric(cycle_length) || cycle_length <= 0) {
    stop("cycle_length must be a positive number.")
  }
  # Check for NA values
  if (any(is.na(df_X))) {
    stop("df_X must not contain NA values.")
  }
  # Check for correct data types
  if (!is.numeric(df_X$Age)) { 
    stop("Age column in df_X must be numeric.")
  }
  
  # Check for valid health states
  if (!all(df_X$M_init %in% v_names_states)) { 
    stop("M_init column in df_X must contain valid health states.")
  }
  
  # Check for valid cycle number
  if (!is.numeric(n_cycles) || n_cycles <= 0 || round(n_cycles) != n_cycles) {
    stop("n_cycles must be a positive integer.")
  }
}

# This function counts the number of first appearances of "IA1_u"
count_total_initiations <- function(m_Stages, cycle) {
  # Subset the matrix to include only the cycles before the specified cycle
  m_M_subset <- m_Stages[, 1:cycle]
  
  # Initialize a counter
  count <- 0
  
  # Loop over each row (individual) in the matrix
  for (i in 1:nrow(m_M_subset)) {
    # Check if "IA1_u" appears in the row and if its first appearance is before the specified cycle
    if ("IA1_u" %in% m_M_subset[i, ] && match("IA1_u", m_M_subset[i, ]) <= cycle) {
      # Now increment the counter
      count <- count + 1 # update the counter when that happens
    }
  }
  
  return(count)
}

# This function counts the number of first appearances of "IA1_u" in specific intervals
count_new_initiations_intervals <- function(m_Stages, intervals) {
  # Initialize a vector to store the counts
  counts <- numeric(length(intervals) - 1)
  
  # Loop over each interval
  for (i in 1:(length(intervals) - 1)) {
    # Initialize a counter
    count <- 0
    
    # Loop over each row (individual) in the matrix
    for (j in 1:nrow(m_Stages)) {
      # Check if "IA1_u" appears in the row and if its first appearance is in the current interval
      if ("IA1_u" %in% m_Stages[j, ] && match("IA1_u", m_Stages[j, ]) > intervals[i] && match("IA1_u", m_Stages[j, ]) <= intervals[i + 1]) {
        # Increment the counter
        count <- count + 1
      }
    }
    
    # Store the count in the counts vector
    counts[i] <- count
  }
  
  return(counts)
}

# This function compares the counts in intervals with the total count
compare_counts <- function(outcomes_SoC, intervals, cycle_count) {
  # Count the number of first appearances of "IA" before cycle_count
  total_initiations <- count_total_initiations(outcomes_SoC$m_Stages, cycle_count)
  print(paste("Total number of lung cancer cases before cycle", cycle_count, ":", total_initiations))
  
  # Define your intervals
  new_initiations_intervals <- count_new_initiations_intervals(outcomes_SoC$m_Stages, intervals)
  
  # Print the number of first appearances of 'IA1_u' in each interval
  for (i in 1:length(new_initiations_intervals)) {
    print(paste("Number of first appearances of 'IA1_u' in the", i, "interval:", new_initiations_intervals[i]))
  }
  
  # Sum up the new_initiations_intervals
  sum_new_initiations_intervals <- sum(new_initiations_intervals)
  
  # Compare with IA1_count
  if (sum_new_initiations_intervals == total_initiations) {
    print("The sum of new_initiations_intervals is the same as total_initiations")
  } else {
    print("The sum of new_initiations_intervals is not the same as total_initiations")
  }
}

# This function checks if the number of individual per cycle sums up to the total number of individuals
check_column_sums <- function(m_count, n_i, n_states) {
  # Check if each column sums up to n_i in the first n_states rows
  column_sums <- colSums(m_count[1:n_states, ])
  incorrect_columns <- which(column_sums != n_i)
  if (length(incorrect_columns) > 0) {
    warning(paste("Columns", paste(incorrect_columns, collapse = ", "), "in the first", n_states, "rows of m_count do not sum up to", n_i, "."))
  } else {
    print(paste("All columns in the first", n_states, "rows of m_count sum up to", n_i, "."))
  }
}

# This function checks if there are any zeros after the first non-zero value in each row
check_zeros_after_values <- function(m_count, v_names_states, n_cycles) {
  # Get the number of rows and columns
  n_rows = length(v_names_states)
  n_cols = n_cycles + 1
  
  # Initialize a variable to keep track of rows with zeros after the first non-zero value
  rows_with_zeros <- FALSE
  
  # Generate column names
  v_names_cycles <- paste("cycle", 0:n_cycles, sep = " ")
  
  # Loop over each row
  for (i in 1:n_rows) {
    # Find the index of the first non-zero value in the row
    first_value_index <- which(m_count[i, ] != 0)[1]
    
    # If there is no non-zero value, skip to the next row
    if (is.na(first_value_index)) {
      next
    }
    
    # Check if there are any zeros after the first non-zero value
    zero_indices <- which(m_count[i, (first_value_index+1):n_cols] == 0)
    
    # Print a warning if there are zeros after the first non-zero value
    if (length(zero_indices) > 0) {
      warning(paste(v_names_states[i], "has zeros after the first non-zero value at columns", paste(v_names_cycles[zero_indices + first_value_index], collapse = ", "), "."))
      rows_with_zeros <- TRUE
    }
  }
  
  # Print a confirmation message if there are no rows with zeros after the first non-zero value
  if (!rows_with_zeros) {
    print("No rows have zeros after the first non-zero value.")
  }
}

# This function calculates the cancer prevalence ratio
calculate_ratio <- function(data, states) {
  total <- sum(!data %in% states)
  d_only <- sum(data != "D")
  return((total / d_only) * 100000)
}

# Modified PLCOm2012 function, I modified the education and age factor from the original function available in the PLCOm2012 publication
mplcom2012 <- function(age, race, education, bmi, copd, cancer_hist, family_hist_lung_cancer, smoking_status, smoking_intensity, duration_smoking,
                       smoking_quit_time, age_months) {
  
  race <- tolower(race)
  
  # Modify the education factor
  education_factor <- ifelse(education > 5, 0, education - 4)
  age_factor <- ifelse(age_months == 0, age, age/12)
  
  if (race == "white" | race == "american indian" | race == "alaskan native" | race == 1) {
    model <- 0.0778868 * (age_factor - 62) - 0.0812744 * education_factor - 0.0274194 * (bmi - 27) + 0.3553063 * copd + 0.4589971 * cancer_hist +
      0.587185 * family_hist_lung_cancer + 0.2597431 * smoking_status - 1.822606 * ((smoking_intensity/10)^(-1) - 0.4021541613) + 0.0317321 *
      (duration_smoking - 27) - 0.0308572 * (smoking_quit_time - 10) - 4.532506
  }
  
  if (race == "black" | race == 2) {
    model <- 0.0778868 * (age_factor - 62) - 0.0812744 * education_factor - 0.0274194 * (bmi - 27) + 0.3553063 * copd + 0.4589971 * cancer_hist +
      0.587185 * family_hist_lung_cancer + 0.2597431 * smoking_status - 1.822606 * ((smoking_intensity/10)^(-1) - 0.4021541613) + 0.0317321 *
      (duration_smoking - 27) - 0.0308572 * (smoking_quit_time - 10) - 4.532506 + 0.3944778
  }
  
  if (race == "hispanic" | race == 3) {
    model <- 0.0778868 * (age_factor - 62) - 0.0812744 * education_factor - 0.0274194 * (bmi - 27) + 0.3553063 * copd + 0.4589971 * cancer_hist +
      0.587185 * family_hist_lung_cancer + 0.2597431 * smoking_status - 1.822606 * ((smoking_intensity/10)^(-1) - 0.4021541613) + 0.0317321 *
      (duration_smoking - 27) - 0.0308572 * (smoking_quit_time - 10) - 4.532506 - 0.7434744
  }
  
  if (race == "asian" | race == 4) {
    model <- 0.0778868 * (age_factor - 62) - 0.0812744 * education_factor - 0.0274194 * (bmi - 27) + 0.3553063 * copd + 0.4589971 * cancer_hist +
      0.587185 * family_hist_lung_cancer + 0.2597431 * smoking_status - 1.822606 * ((smoking_intensity/10)^(-1) - 0.4021541613) + 0.0317321 *
      (duration_smoking - 27) - 0.0308572 * (smoking_quit_time - 10) - 4.532506 - 0.466585
  }
  
  if (race == "native hawaiian" | race == "pacific islander" | race == 5) {
    model <- 0.0778868 * (age_factor - 62) - 0.0812744 * education_factor - 0.0274194 * (bmi - 27) + 0.3553063 * copd + 0.4589971 * cancer_hist +
      0.587185 * family_hist_lung_cancer + 0.2597431 * smoking_status - 1.822606 * ((smoking_intensity/10)^(-1) - 0.4021541613) + 0.0317321 *
      (duration_smoking - 27) - 0.0308572 * (smoking_quit_time - 10) - 4.532506 + 1.027152
  }
  
  prob <- exp(model)/(1 + exp(model))
  results <- list()
  results$prob <- prob
  return(results)
  
}

# Function to keep only the first TRUE value in each row, if there are more than one
keep_first_true <- function(mat) {
    apply(mat, 1, function(row) {
      true_positions <- which(row) # find true values positions
      if (length(true_positions) > 1) { # if there are more than one true value
        row[-min(true_positions)] <- FALSE # keep only the first true value
      }
      row # return the row
    }) |> t() # transpose the matrix to get the original shape
  }
  
# This function calculates the stage distribution among all the individuals and detected stages
Stage_distribution_function <- function(m_Stages, intervals, time_unit) {
  # Define the stages
  stages <- c("IA1_d", "IA2_d", "IA3_d", "IB_d", "IIA_d", "IIB_d", "IIIA_d", "IIIB_d", "IIIC_d", "IVA_d")
  
  # Initialize a matrix to store the counts
  detection_matrix <- matrix(0, nrow = length(stages) + 1, ncol = length(intervals) - 1)
  rownames(detection_matrix) <- c(stages, "Sum")
  
  # Set the column names based on the time unit
  if (time_unit == "year") {
    colnames(detection_matrix) <- paste("Year", 1:(length(intervals) - 1))
  } else if (time_unit == "cycle_length") {
    colnames(detection_matrix) <- paste("Month", 1:(length(intervals) - 1))
  } else if (time_unit == "n_cycles") {
    colnames(detection_matrix) <- "Lifetime"
  }
  
  # Loop over each interval
  for (i in 1:(length(intervals) - 1)) {
    # Initialize a counter for the total detections in the interval
    total_detections <- 0
    
    # Loop over each stage
    for (s in 1:length(stages)) {
      stage <- stages[s]
      # Loop over each row (individual) in the matrix
      for (j in 1:nrow(m_Stages)) {
        # Check if the stage appears in the row and if its first appearance falls within the interval
        if (stage %in% m_Stages[j, ] && match(stage, m_Stages[j, ]) > intervals[i] && match(stage, m_Stages[j, ]) <= intervals[i + 1]) {
          # Increment the count for that stage and the total detections
          detection_matrix[s, i] <- detection_matrix[s, i] + 1
          total_detections <- total_detections + 1
        }
      }
    }
    
    # Divide the counts for each stage by the total detections in the interval to get the proportions
    detection_matrix[1:length(stages), i] <- detection_matrix[1:length(stages), i] / total_detections
    
    # Add a final row that checks if each column sums up to 1
    detection_matrix[length(stages) + 1, i] <- sum(detection_matrix[1:length(stages), i])
  }
  
  return(detection_matrix)
}

# the logic of the function only makes sense for year intervals
# if you want overall just do colsum of the matrix and you'll have until the end
# and for five years age groups sum the rows of those age groups. and you'll have the incidence in that year for that five years age group
# or take the average, depends on how you want to calculate it, but it's still possible

# This function counts the number of people alive at each cycle
count_alive <- function(m_Stages, cycle) {
  if (cycle <= 0) {
    return(nrow(m_Stages)) # if cycle zero everyone is alive
  }
  m_Stages_subset <- m_Stages[, 1:cycle] #define subset of m_Stages
  count <- 0 #initialize counter
  for (i in 1:nrow(m_Stages_subset)) {
    if (!("D" %in% m_Stages_subset[i, ]) || match("D", m_Stages_subset[i, ]) > cycle) { #it identifies the first occurrence of D
      count <- count + 1
    }
  }
  return(count)
}

# This function divides the number of first appearances of 'IA1_d' in each interval by the total number of people alive (not in D)
count_new_detections_intervals <- function(m_Stages, intervals, m_First_Detection) {
  counts <- numeric(length(intervals) - 1)  # Store the counts for each interval
  already_counted <- rep(FALSE, nrow(m_Stages))  # Track if an individual has been counted already
  
  for (i in 1:(length(intervals) - 1)) { # Loop over each interval
    count <- 0 # Initialize the count for the interval
    for (j in 1:nrow(m_Stages)) { # Loop over each individual
      if (!already_counted[j]) { # Check if the individual has not been counted already
        # Ensure the result is a matrix by setting drop = FALSE
        stage_positions <- which(m_First_Detection[j, , drop = FALSE]) # Find the first detection of each individual
        if (length(stage_positions) > 0) { # Check if there are any first detections
          first_occurrence <- stage_positions[1] # Get the first occurrence
          if (first_occurrence > intervals[i] && first_occurrence <= intervals[i + 1]) { # Check if the first occurrence is in the interval, and if yes
            count <- count + 1 # Increment the count
            already_counted[j] <- TRUE # Mark the individual as counted
          }
        }
      }
    }
    
    
    counts[i] <- (count / count_alive(m_Stages, intervals[i])) * 100000  # Adjust the count relative to the number of people alive
  }
  
  return(counts)
}


# This is the function that gives the matrix of incidence of IA_D over the entire period, not by intervals 
IA_D_Incidence_Function <- function(matrix_name, m_Stages, m_First_Detection, n_cycles, year, outcomes_SoC, count_alive, count_new_detections_intervals) {
  
  # Define your intervals
  intervals <- seq(0, n_cycles, by = year) #define how many times to check the incidence
  
  # Call the modified function
  IA_d_Incidence <- count_new_detections_intervals(outcomes_SoC$m_Stages, intervals, outcomes_SoC$m_First_Detection) #get the incidence
  
  # Store the incidence values in a matrix
  m_Incidence <- matrix(IA_d_Incidence, nrow = 1, dimnames = list("Incidence", paste0("Interval_", 1:(length(intervals) - 1)))) #store the incidence in a matrix
  
  # Add m_Incidence to outcomes_SoC list
  outcomes_SoC[[matrix_name]] <- m_Incidence #add the incidence to the outcomes_SoC list
  
  # Return the updated outcomes_SoC list
  return(outcomes_SoC)
}

# This function finds the interval index based on the age
find_age_custom_interval <- function(x, breaks_A) { #function to find the age interval
  for(i in length(breaks_A):2) { #loop over the breaks_A
    if(x >= breaks_A[i-1] & x < breaks_A[i]) { #modify the equal signs if you do not want to include lower bounds
      return(i-1)
    }
  }
  return(61)  # return NA if x does not fall within any interval, any value to make it visible that it's not in any interval
}

# Function to sum diagonal elements in groups of five
sum_diagonal_groups <- function(df) {
  # Extracting diagonal values
  diag_vals <- diag(as.matrix(df)) #extract the diagonal values
  # Summing every five values
  group_sums <- tapply(diag_vals, ceiling(seq_along(diag_vals)/5), sum)
  return(group_sums)
}

# Function to generate age group labels
generate_age_group_labels <- function(n_groups, start_age = 40, interval = 5) { #function to generate age group labels
  age_groups <- vector("character", length = n_groups) #initialize the age groups
  for (i in seq_len(n_groups)) { #loop over the number of groups
    start <- start_age + (i - 1) * interval #calculate the start of the interval
    end <- start + interval - 1 #calculate the end of the interval
    age_groups[i] <- paste(start, end, sep = "-") #store the name of the interval in the age_groups
  }
  return(age_groups)
}

# This function calculates the incidence of IA_D by age group
IA_D_Incidence_Function_By_Age <- function(matrix_name, m_Stages, m_First_Detection, m_E, n_cycles, year, outcomes_SoC, interval_type = "year", find_age_custom_interval, count_alive, EligibilityRestriction = TRUE) {
  ## Make the Age Interval Matrix ##
  
  m_A <- outcomes_SoC$m_A #get the age matrix
  
  # Define your new breaks_A
  # Define the age groups dynamically based on n_cycles so that every time you change the number of cycles you don't have to come back here
  breaks_A <- seq(480, 480 + n_cycles, by = 12) #define the age groups, by 12 = by year
  
  # Initialize m_Age_Intervals with m_A
  m_Age_Intervals <- m_A #initialize the age intervals
  
  
  # Replace each cell with the corresponding interval index
  for(i in 1:nrow(m_A)) { 
    for(j in 1:ncol(m_A)) {
      m_Age_Intervals[i,j] <- find_age_custom_interval(m_A[i,j], breaks_A)  #find the age interval for each age and replace it in the matrix
    }
  }
  
  # I have intervals now
  
  # Making columns together
  
  # Determine the number of compacted columns
  num_compact_cols <- ceiling(ncol(m_Age_Intervals) / year) #determine the number of compacted columns (by year)
  
  # Initialize the new matrix
  m_Compact <- matrix(nrow = nrow(m_Age_Intervals), ncol = num_compact_cols)
  
  # Compact the columns every 'year' cycles
  for(i in 1:nrow(m_Age_Intervals)) {
    for(j in seq(1, ncol(m_Age_Intervals), by = year)) {
      # Take the value from the first column of each 'year'-cycle block
      m_Compact[i, j %/% year + 1] <- m_Age_Intervals[i, j] #compact the columns
    }
  }
  
  # If the number of columns is not a multiple of 'year', compact the remaining columns
  if(ncol(m_Age_Intervals) %% year != 0) {
    for(i in 1:nrow(m_Age_Intervals)) {
      m_Compact[i, num_compact_cols] <- m_Age_Intervals[i, (num_compact_cols - 1) * year + 1]
    }
  }
  
  # Set the column names
  colnames(m_Compact) <- paste("Interval", 1:ncol(m_Compact))
  
  # Define a function to count the number of people alive (not in D)
  
  # Define your intervals based on the interval_type, this function will only work with "year", I did not need n_cycles so I didn't code it
  if (interval_type == "year") {
    intervals <- seq(0, n_cycles, by = year)
  } else if (interval_type == "n_cycles") {
    intervals <- seq(0, n_cycles, by = n_cycles)
  } else {
    stop("Invalid interval_type. Please choose either 'year' or 'n_cycles'.")
  }
  
  # Initialize the incidence matrix with rows as possible values in m_Compact and columns as intervals
  # Make a matrix to store the numerator and denominator of the incidence rate, instead of diving them in each interval, i want to sum the numerators and denominators of five intervals and then divide
  m_Incidence_ByAge_Numerator <- matrix(0, nrow = max(m_Compact), ncol = length(intervals) - 1, 
                                        dimnames = list(paste0("Value_", 1:max(m_Compact)), paste0("Interval_", 1:(length(intervals) - 1))))
  
  m_Incidence_ByAge_Denominator <- m_Incidence_ByAge_Numerator
  
  # Loop over each interval or each possible value in m_Compact based on the interval_type
  if (interval_type == "year") {
    
    
    
    first_d_detected <- rep(NA, nrow(m_Stages)) #initialize the first_d_detected to keep track of the first detection
    
    
    
    if (!EligibilityRestriction) { #if there is no eligibility restriction
      
      for (interval in 1:(length(intervals) - 1)) {
        for (age_group in 1:max(m_Compact, na.rm = TRUE)) {
          indices <- which(m_Compact[, interval] == age_group) #get the indices of the age group
          if (length(indices) > 0) {
            
            
            alive <- 0
            count_new_d <- 0
            
            for (k in indices) {
              # Extract relevant stages and detection flags for the current interval
              stages_in_interval <- m_Stages[k, intervals[interval]:min(intervals[interval + 1], ncol(m_Stages))] #get the stages in the interval
              first_d_in_interval <- m_First_Detection[k, intervals[interval]:min(intervals[interval + 1], ncol(m_Stages))] #get the first detection in the interval
              
              # Ensure we count first "_d" detections only when the stage is neither "D" nor NA
              valid_detections <- which(first_d_in_interval & !is.na(stages_in_interval) & (stages_in_interval != "D"))
              
              if (length(valid_detections) > 0) {
                first_valid_detection_position <- valid_detections[1] + intervals[interval] - 1 #get the first valid detection position
                if (is.na(first_d_detected[k]) || first_d_detected[k] > first_valid_detection_position) { #if the first detection is NA or the first detection is greater than the valid detection
                  count_new_d <- count_new_d + 1 #increment the count
                  first_d_detected[k] <- first_valid_detection_position #store the first valid detection position
                }
              }
              
              # Only consider alive if stages in the interval are not exclusively "D" and not NA
              if (any(!is.na(stages_in_interval) & stages_in_interval != "D")) { #if the stages are not exclusively "D" and not NA
                alive <- alive + 1
              }
            }
            
            # Calculate the incidence rate for the current age group and interval
            if (alive > 0) {
              m_Incidence_ByAge_Numerator[age_group, interval]   <- count_new_d
              m_Incidence_ByAge_Denominator[age_group, interval] <- alive
            } else {
              
            }
          }
        }
      }
    } else { #if there is eligibility restriction, same as before but calculate only on eligible people
      
      
      for (interval in 1:(length(intervals) - 1)) {
        for (age_group in 1:max(m_Compact, na.rm = TRUE)) {
          indices <- which(m_Compact[, interval] == age_group)
          if (length(indices) > 0) {
            
            
            col_range <- intervals[interval]:min(intervals[interval + 1], ncol(m_Stages)) #get the column range
            
            # Corrected checks for emptiness
            if (length(indices) > 0 && all(indices <= nrow(m_E)) && length(col_range) > 0 && all(col_range <= ncol(m_E))) { # Check if the indices are within the range of m_E
              # Create a subset and check if it has elements
              subset_m_E <- m_E[indices, col_range, drop = FALSE] # Use drop = FALSE to keep matrix structure, this line is used to check if the subset is not empty
              
              if (is.matrix(subset_m_E) && nrow(subset_m_E) > 0 && ncol(subset_m_E) > 0) { # Check if the subset is a matrix and has elements
                # Now it's safe to apply
                eligible_indices <- indices[apply(subset_m_E, 1, any)] #get the eligible indices
                
                if (length(eligible_indices) > 0) {
                  # Proceed with further calculations
                  alive <- 0
                  count_new_d <- 0
                  
                  for (k in eligible_indices) { #loop over the eligible indices
                    # Extract relevant stages and detection flags for the current interval
                    stages_in_interval <- m_Stages[k, col_range]
                    first_d_in_interval <- m_First_Detection[k, col_range]
                    
                    # Ensure we count first "_d" detections only
                    valid_detections <- which(first_d_in_interval & !is.na(stages_in_interval) & (stages_in_interval != "D"))
                    
                    if (length(valid_detections) > 0) {
                      first_valid_detection_position <- valid_detections[1] + intervals[interval] - 1
                      if (is.na(first_d_detected[k]) || first_d_detected[k] > first_valid_detection_position) {
                        count_new_d <- count_new_d + 1
                        first_d_detected[k] <- first_valid_detection_position
                      }
                    }
                    
                    # Only consider alive if not exclusively "D" or NA
                    if (any(!is.na(stages_in_interval) & stages_in_interval != "D")) {
                      alive <- alive + 1
                    }
                  }
                  
                  # Debug print statements for checking NAs
                  #if (alive == 0) {
                  #  print(paste("Alive is 0 for age_group:", age_group, "in interval:", interval))
                  #}
                  #if (count_new_d == 0) {
                  #  print(paste("count_new_d is 0 for age_group:", age_group, "in interval:", interval))
                  #}
                  # NAs because people are either dead or no new initiations, it seems that it makes sense
                  
                  
                  
                  
                  
                  # Calculate the incidence rate for the current age group and interval
                  if (alive > 0) {
                    m_Incidence_ByAge_Numerator[age_group, interval]   <- count_new_d
                    m_Incidence_ByAge_Denominator[age_group, interval] <- alive
                  }
                }
              }
            }
          }
        }  
      } 
    }
    
    
  } else if (interval_type == "n_cycles") {
    
    print("I do not really need it so I didn't make it")
    
  }
  
  
  # Applying the function to both dataframes
  numerator_sums <- sum_diagonal_groups(m_Incidence_ByAge_Numerator)
  denominator_sums <- sum_diagonal_groups(m_Incidence_ByAge_Denominator)
  
  # Creating the third dataframe by dividing and multiplying by 100,000
  m_Incidence_ByAge <- as.data.frame(numerator_sums / denominator_sums * 100000)
  
  
  # Assuming m_Incidence_ByAge has been created as before
  
  
  # Generate row names based on the number of rows in m_Incidence_ByAge
  n_groups <- nrow(m_Incidence_ByAge)
  age_group_labels <- generate_age_group_labels(n_groups)
  
  # Assign the generated names to the rows of m_Incidence_ByAge
  rownames(m_Incidence_ByAge) <- age_group_labels
  colnames(m_Incidence_ByAge) <- "Incidence Rate"
  
  
  
  # Add m_Incidence_ByAge to outcomes_SoC list
  outcomes_SoC[[matrix_name]] <- m_Incidence_ByAge 
  
  # Return the updated outcomes_SoC list
  return(outcomes_SoC)
}

# Define your specific growth function for tumour growth, this is a modified version of the Gompertz function can be others
growth_function <- function(y0, K, mumax, lambda, t_G) {
  y0 * (K/y0)^(exp(-exp((exp(1)*mumax*(lambda - t_G))/log(K/y0)+1)))
} 

# Define the derivative of the growth function
d_growth_function <- function(t_VDT, mumax) { 
  eval(D(expression(y0 * (K/y0)^(exp(-exp((exp(1)*mumax*(lambda - t_VDT))/log(K/y0)+1)))), 
         "t_VDT")) 
}

# This function assign the T stage based on the tumour size
find_tumor_T_stage <- function(size) {
  if (is.na(size)) {
    return(NA)  # return "NA" as a string if the size is NA
  } else if (size == 0) {
    return("T0")
  } else if (size > 0 && size <= 10) {
    return("T1A")
  } else if (size > 10 && size <= 20) {
    return("T1B")
  } else if (size > 20 && size <= 30) {
    return("T1C")
  } else if (size > 30 && size <= 40) {
    return("T2A")
  } else if (size > 40 && size <= 50) {
    return("T2B")
  } else if (size > 50 && size <= 70) {
    return("T3")
  } else if (size > 70) {
    return("T4")
  } else {
    return(NA)  # return NA if the size is outside the specified intervals
  }
}


# This function finds the interval index based on the tumor size, useful when differentiating by size
find_custom_interval <- function(x, breaks) {
  if (is.na(x)) {
    return(NA)  # return NA if x is NA
  } else {
    for(i in length(breaks):2) { 
      if(x > breaks[i-1] & x <= breaks[i]) { # if x is greater than the lower bound and less than or equal to the upper bound of the interval assign the corresponding interval
        return(i-1) 
      }
    }
    return(NA)  # return NA if x does not fall within any interval
  }
}

# This funcion combines the T, N, and M to get the stage
get_state <- function(t, n, m, detected) {
  if (!is.na(t) && t == "T0") { # If T0 there is no tumour
    return("H")
  } else if (is.na(t) || is.na(n) || is.na(m)) {
    return(NA)
  } else {
    row <- match(t, state_table$T0)
    col <- match(n, colnames(state_table))
    if (m == "M1") { # If distant metastasis straight to stage IV
      state <- "IVA"
    } else {
      state <- state_table[row, col] # Otherwise match the stage from the table
    }
    state <- paste0(state, ifelse(detected, "_d", "_u"))
    if (!is.na(detected) && !is.na(detection_probabilities[state]) && length(detection_probabilities[state]) == 1 && !detected && runif(1) < detection_probabilities[state]) { # If detection occurs
      state <- gsub("_u", "_d", state) # Replace _u with _d
    }
    return(state)
  }
}

# Here apply the function and keep the first detected state _d state forever
apply_get_state <- function(m_T, m_N, m_M, state_table, detection_probabilities) {
  # Define a local version of get_state inside apply_get_state, for scope definition
  get_state <- function(t, n, m, detected) {
    if (!is.na(t) && t == "T0") {
      return("H")
    } else if (is.na(t) || is.na(n) || is.na(m)) {
      return(NA)
    } else {
      row <- match(t, state_table$T0)
      col <- match(n, colnames(state_table))
      if (m == "M1") {
        state <- "IVA"
      } else {
        state <- state_table[row, col]
      }
      state <- paste0(state, ifelse(detected, "_d", "_u"))
      if (!is.na(detected) && !is.na(detection_probabilities[state]) && length(detection_probabilities[state]) == 1 && !detected && runif(1) < detection_probabilities[state]) {
        state <- gsub("_u", "_d", state) # Replace _u with _d
      }
      return(state)
    }
  }
  
  detected <- rep(FALSE, nrow(m_T)) # Initialize detected vector
  last_diagnosed <- rep(NA, nrow(m_T)) # Initialize last_diagnosed vector 
  states <- matrix(NA, nrow = nrow(m_T), ncol = ncol(m_T)) # Initialize states matrix
  first_detection <- matrix(FALSE, nrow = nrow(m_T), ncol = ncol(m_T)) # Initialize first_detection matrix
  
  for (i in 1:nrow(m_T)) {
    for (j in 1:ncol(m_T)) {
      if (!is.na(last_diagnosed[i])) {
        states[i, j] <- last_diagnosed[i] # If previously diagnosed, keep the last diagnosed state
      } else {
        states[i, j] <- get_state(m_T[i, j], m_N[i, j], m_M[i, j], detected[i]) # Get the state
        if (!is.na(m_T[i, j]) && m_T[i, j] != "T0" && !detected[i]) { # Check if not previously detected
          detected[i] <- grepl("_d", states[i, j]) # Check if detected
          if (detected[i]) {
            last_diagnosed[i] <- states[i, j] # Store the last diagnosed state
            first_detection[i, j] <- TRUE # Mark the first detection
          }
        }
      }
    }
  }
  
  return(list(states = states, first_detection = first_detection)) # Return both states and first_detection matrices
}

# Function to subset the dataset and select the columns of interest for TSCE calculations
subset_and_select <- function(df, quit_time_min, quit_time_max, pkyr_min, pkyr_max, cigsmok) {
  if (cigsmok == 1) {
    df <- subset(df, cigsmok == 1 & pkyr_min <= pkyr & pkyr < pkyr_max) # Subset the dataset based on the conditions
  } else {
    df <- subset(df, quit_time_min < smoking_quit_time & smoking_quit_time <= quit_time_max & pkyr_min <= pkyr & pkyr < pkyr_max & cigsmok == 0) # Subset the dataset based on the conditions
  }
  df <- df[df$conflc %in% c(1, 2), c("age", "smokeage", "age_quit", "smokeday", "fup_days", "conflc")] # Select the columns of interest
  return(df)
}

### TSCE Functions ###
# This is the main function that calculates the TSCE model, taken from the original code and adapted
process_data <- function(dataset, smoker = FALSE) {
  
  # Preprocess the dataset
  
  dataset$fup_days_years <- dataset$fup_days/365.25
  dataset$conflc <- ifelse(dataset$conflc == 2, 0, dataset$conflc)
  dataset$fup_days <- NULL
  
  dataset <- rename(dataset, pyr = fup_days_years)
  dataset <- rename(dataset, ageStart = smokeage)
  dataset <- rename(dataset, ageQuit = age_quit)
  dataset <- rename(dataset, cigsPerDay = smokeday)
  dataset <- rename(dataset, cases = conflc)
  
  if (smoker) {
    dataset$ageQuit <- dataset$age
  }
  
  
  dataset <- aggregate(cbind(pyr, cases) ~ age + ageStart + ageQuit + cigsPerDay, data = dataset, FUN = function(x) {if(length(x) > 1) sum(x) else x})
  dataset <- subset(dataset, pyr != 0)
  
  
  lagtime <- 5 # lagtime in years
  dataset$laggedAge <- dataset$age - lagtime
  
  
  # Individuals who never started smoking or started smoking at an age later than or equal to laggedAge
  dataset$nonSmoke <- (dataset$ageStart==dataset$ageQuit) |
    (dataset$ageStart >= dataset$laggedAge)
  dataset$exSmoke <- (dataset$ageQuit < dataset$laggedAge) &
    !dataset$nonSmoke
  dataset$smoke <- is.na(dataset$ageQuit) | (!dataset$nonSmoke & !dataset$exSmoke)
  
  
  
  # Check that every individual has only be assigned to one status
  check_rows <- function(row) {
    sum(row) == 1
  }
  
  dataset$check <- apply(dataset[, c('nonSmoke', 'exSmoke', 'smoke')], 1, check_rows)
  
  # Find rows where condition is not met
  incorrect_rows <- which(dataset$check == FALSE)
  
  if(length(incorrect_rows) > 0) {
    print(paste("The condition is not met in the following rows:", paste(incorrect_rows, collapse=", ")))
  } else {
    print("Everything is okay for every row.")
  }
  dataset$check <- NULL
  
  # t: nonSmoke: 0        0        laggedAge
  #    exSmoke:  ageStart ageQuit  laggedAge
  #    smoke:    0        ageStart laggedAge
  t<-matrix(0,nrow=NROW(dataset),ncol=3)
  t[,3] <- dataset$laggedAge
  t[dataset$smoke,2] <-dataset$ageStart[dataset$smoke]
  t[dataset$exSmoke,2] <- dataset$ageQuit[dataset$exSmoke]
  t[dataset$exSmoke,1] <- dataset$ageStart[dataset$exSmoke]
  
  # smInd: nonSmoke: 0 0 0
  #        exSmoke:  0 1 0
  #        smoke:    0 0 1
  smInd <- matrix(0,nrow=NROW(dataset),ncol=3)
  smInd[dataset$smoke,3] <- 1
  smInd[dataset$exSmoke,2] <- 1
  
  wrap <-function(pars)
  {
    # assume alpha to be small and constant
    alpha <- matrix(1,nrow=1,ncol=3)
    
    Nnu0 <- matrix(exp(pars[1]),nrow=1,ncol=3)
    nu1 <- matrix(exp(pars[2]),nrow=1,ncol=3)
    
    # assume only gamma to depend on smoking
    gamma <- matrix(pars[3],nrow=NROW(dataset),ncol=3) + 
      pars[4]*smInd +
      pars[5]*smInd * (dataset$cigsPerDay>5)
    
    parList <- list(Nnu0=Nnu0, alpha=alpha,gamma=gamma,nu1=nu1)
    result <- tsce(t,parList)
    
    return (result$hazard)
  }
  
  
  loglik <- function(pars)
  {
    return (-sum(dpois(dataset$cases, dataset$pyr*wrap(pars), log = TRUE)))
  }
  
  # use upper bounds to ensure gamma < alpha
  minResult <- nlminb(start = c(-3,-14,0.1,0.05,0.0), objective = loglik, upper=c(0,0,0.3,0.3,0.3))
  bestEstimates <- minResult$par
  bestEstimates
  #> [1]  -1.66526970 -12.65232441   0.10313019   0.04412732   0.06342015
  
 
  # Interpolate to create more time points
  interp_time_points <- seq(min(dataset$laggedAge), max(dataset$laggedAge), length.out = 10000)
 
  # Calculate the hazard function for non-smokers, ex-smokers, and smokers
  hazard_function_nonSmoke <- wrap(c(bestEstimates[1:2], bestEstimates[3], 0, 0))
  hazard_function_exSmoke <- wrap(c(bestEstimates[1:2], bestEstimates[3], bestEstimates[4], 0))
  hazard_function_smoke <- wrap(bestEstimates)
  
  # Interpolate the hazard functions
  interp_hazard_function_nonSmoke <- approx(dataset$laggedAge, hazard_function_nonSmoke, xout = interp_time_points)$y
  interp_hazard_function_exSmoke <- approx(dataset$laggedAge, hazard_function_exSmoke, xout = interp_time_points)$y
  interp_hazard_function_smoke <- approx(dataset$laggedAge, hazard_function_smoke, xout = interp_time_points)$y
  
  
  # Create data frames for plotting
  nonSmoke_df <- data.frame(Age = interp_time_points, PredictedHazard = interp_hazard_function_nonSmoke)
  exSmoke_df <- data.frame(Age = interp_time_points, PredictedHazard = interp_hazard_function_exSmoke)
  smoke_df <- data.frame(Age = interp_time_points, PredictedHazard = interp_hazard_function_smoke)
  
  #rm(hazard_function)
  rm(hazard_function_smoke)
  rm(hazard_function_exSmoke)
  rm(hazard_function_nonSmoke)
  #rm(interp_hazard_function)
  rm(interp_hazard_function_smoke)
  rm(interp_hazard_function_exSmoke)
  rm(interp_hazard_function_nonSmoke)
  rm(interp_time_points)
  rm(bestEstimates)
  rm(t)
  rm(smInd)
  rm(dataset)
  rm(minResult)
  rm(wrap)
  rm(loglik)
  rm(check_rows)
  rm(incorrect_rows)
  rm(lagtime)
  
  
  # using dataframes generated in TSCE
  smoke_df$Age <- cut(smoke_df$Age, breaks = seq(14, 92, by = 1), labels = seq(14, 91, by = 1), right = FALSE, include.lowest = TRUE)
  hazard_Smoke_df <- aggregate(PredictedHazard ~ Age, smoke_df, mean)
  hazard_Smoke_df$Age <- as.numeric(as.character(hazard_Smoke_df$Age))
  
  exSmoke_df$Age <- cut(exSmoke_df$Age, breaks = seq(14, 92, by = 1), labels = seq(14, 91, by = 1), right = FALSE, include.lowest = TRUE)
  hazard_exSmoke_df <- aggregate(PredictedHazard ~ Age, exSmoke_df, mean)
  hazard_exSmoke_df$Age <- as.numeric(as.character(hazard_exSmoke_df$Age))
  
  nonSmoke_df$Age <- cut(nonSmoke_df$Age, breaks = seq(14, 92, by = 1), labels = seq(14, 91, by = 1), right = FALSE, include.lowest = TRUE)
  hazard_nonSmoke_df <- aggregate(PredictedHazard ~ Age, nonSmoke_df, mean)
  hazard_nonSmoke_df$Age <- as.numeric(as.character(hazard_nonSmoke_df$Age))
  
  
  # Prediction of values if don't have enough
  # Prediction of future values when Smoker
  # Data
  values <- log(hazard_Smoke_df$PredictedHazard)  
  time_points <- hazard_Smoke_df$Age
  # Create and fit your model
  model <- lm(values ~ time_points)
  # Predict past values
  Age <- c(35:hazard_Smoke_df[1, "Age"]-1)
  PredictedHazard <- exp(predict(model, newdata=data.frame(time_points=Age)))
  PredictedHazard <- pmin(PredictedHazard, min(hazard_Smoke_df$PredictedHazard))
  new_data <- data.frame(Age, PredictedHazard)
  # Add the new data to the original data frame
  hazard_Smoke_df <- rbind(new_data, hazard_Smoke_df)
  # Predict future values
  # Data
  values <- hazard_Smoke_df$PredictedHazard  
  time_points <- hazard_Smoke_df$Age
  # Create and fit your model
  model <- lm(values ~ time_points)
  Age <- c((hazard_Smoke_df[nrow(hazard_Smoke_df), 1] + 1):107)
  PredictedHazard <- predict(model, newdata=data.frame(time_points=Age))
  new_data <- data.frame(Age, PredictedHazard)
  # Add the new data to the original data frame
  hazard_Smoke_df <- rbind(hazard_Smoke_df, new_data)
  
  
  # Prediction of future values when Ex-Smoker
  # Data
  values <- log(hazard_exSmoke_df$PredictedHazard)
  time_points <- hazard_exSmoke_df$Age
  # Create and fit your model
  model <- lm(values ~ time_points)
  # Predict past values
  Age <- c(35:hazard_exSmoke_df[1, "Age"]-1)
  PredictedHazard <- exp(predict(model, newdata=data.frame(time_points=Age)))
  PredictedHazard <- pmin(PredictedHazard, min(hazard_exSmoke_df$PredictedHazard))
  new_data <- data.frame(Age, PredictedHazard)
  # Add the new data to the original data frame
  hazard_exSmoke_df <- rbind(new_data, hazard_exSmoke_df)
  # Predict future values
  # Data
  values <- hazard_exSmoke_df$PredictedHazard
  time_points <- hazard_exSmoke_df$Age
  # Create and fit your model
  model <- lm(values ~ time_points)
  Age <- c((hazard_exSmoke_df[nrow(hazard_exSmoke_df), 1] + 1):107)
  PredictedHazard <- predict(model, newdata=data.frame(time_points=Age))
  new_data <- data.frame(Age, PredictedHazard)
  # Add the new data to the original data frame
  hazard_exSmoke_df <- rbind(hazard_exSmoke_df, new_data)
  
  
  # Prediction of future values when Non-Smoker, not really needed anymore but kept for consistency
  # Data
  values <- log(hazard_nonSmoke_df$PredictedHazard)
  time_points <- hazard_nonSmoke_df$Age
  # Create and fit your model
  model <- lm(values ~ time_points)
  # Predict past values
  Age <- c(35:hazard_nonSmoke_df[1, "Age"]-1)
  PredictedHazard <- exp(predict(model, newdata=data.frame(time_points=Age)))
  PredictedHazard <- pmin(PredictedHazard, min(hazard_nonSmoke_df$PredictedHazard))
  new_data <- data.frame(Age, PredictedHazard)
  # Add the new data to the original data frame
  hazard_nonSmoke_df <- rbind(new_data, hazard_nonSmoke_df)
  # Predict future values
  # Data
  values <- hazard_nonSmoke_df$PredictedHazard
  time_points <- hazard_nonSmoke_df$Age
  # Create and fit your model
  model <- lm(values ~ time_points)
  Age <- c((hazard_nonSmoke_df[nrow(hazard_nonSmoke_df), 1] + 1):107)
  PredictedHazard <- predict(model, newdata=data.frame(time_points=Age))
  new_data <- data.frame(Age, PredictedHazard)
  # Add the new data to the original data frame
  hazard_nonSmoke_df <- rbind(hazard_nonSmoke_df, new_data)
  
  
  # Convert age from years to months
  hazard_Smoke_df$Age <- hazard_Smoke_df$Age*12
  hazard_exSmoke_df$Age <- hazard_exSmoke_df$Age*12
  hazard_nonSmoke_df$Age <- hazard_nonSmoke_df$Age*12
  
  # Create a new age vector from min Age to max Age in monthly steps
  
  new_age <- seq(min(hazard_Smoke_df$Age), max(hazard_Smoke_df$Age), by = 1)
 
  # Use linear interpolation to find the hazard values for smokers corresponding to the new ages
  new_hazard <- approx(hazard_Smoke_df$Age, hazard_Smoke_df$PredictedHazard, new_age)$y
  # Create a new data frame with the new ages and hazards
  hazard_Smoke_df_dataset <- data.frame(Age = new_age, PredictedHazard = new_hazard)
  
  # Use linear interpolation to find the hazard values for ex-smokers corresponding to the new ages
  new_hazard <- approx(hazard_exSmoke_df$Age, hazard_exSmoke_df$PredictedHazard, new_age)$y
  # Create a new data frame with the new ages and hazards
  hazard_exSmoke_df_dataset <- data.frame(Age = new_age, PredictedHazard = new_hazard)
  
  # Use linear interpolation to find the hazard values for non-smokers corresponding to the new ages
  new_hazard <- approx(hazard_nonSmoke_df$Age, hazard_nonSmoke_df$PredictedHazard, new_age)$y
  # Create a new data frame with the new ages and hazards
  hazard_nonSmoke_df_dataset <- data.frame(Age = new_age, PredictedHazard = new_hazard)
  
  
  # remove unnecessary objects
  rm(smoke_df)
  rm(exSmoke_df)
  rm(nonSmoke_df)
  rm(model)
  rm(new_data)
  rm(new_age)
  rm(new_hazard)
  rm(PredictedHazard)
  rm(Age)
  rm(time_points)
  rm(values)
  
  
  # Return all three datasets as a list
  return(list(smoke = hazard_Smoke_df_dataset, exSmoke = hazard_exSmoke_df_dataset, nonSmoke = hazard_nonSmoke_df_dataset))
  
}

# This function gets the probabilities of nodal involvement based on the volume of the lesion and distant metastasis calculated via calibration
Transition_Probabilities <- function(NLST){
  
  STAGE_NLST <- NLST[, c("lesionsize", "clinical_m_7thed", "clinical_n_7thed")] # keep only volume and M/N values
  
  # Define the mapping for clinical_m_7thed
  m_mapping <- c("0" = "M0", "100" = "M1", "110" = "M1a", "120" = "M1b", "999" = "MX")
  # Define the mapping for clinical_n_7thed
  n_mapping <- c("0" = "N0", "100" = "N1", "200" = "N2", "300" = "N3", "999" = "NX")
  # Apply the mapping to the data frame, give names to values
  STAGE_NLST <- STAGE_NLST %>% mutate(clinical_m_7thed = m_mapping[as.character(clinical_m_7thed)], clinical_n_7thed = n_mapping[as.character(clinical_n_7thed)])
  
  # Cleaning dataset, remove NA and MX/NX values
  # Keep only rows where lesionsize, clinical_m_7thed, and clinical_n_7thed are not NA
  STAGE_NLST <- STAGE_NLST[complete.cases(STAGE_NLST[, c("lesionsize", "clinical_m_7thed", "clinical_n_7thed")]), ]
  # Remove rows where clinical_m_7thed is MX or clinical_n_7thed is NX
  STAGE_NLST <- STAGE_NLST %>% filter(!(clinical_m_7thed == "MX" | clinical_n_7thed == "NX"))
  STAGE_NLST$clinical_m_7thed <- ifelse(STAGE_NLST$clinical_m_7thed != 'M0', 'M1', STAGE_NLST$clinical_m_7thed)
  
  
  # Create quartiles for volume ranges
  quartiles <- quantile(STAGE_NLST$lesionsize, probs=0:4/4)
  # Manually set the breaks
  breaks <- c(0, 15, 25, 40, 260) # breaks based on quantiles but with zero as first value
  
  # Create a new column for volume ranges
  STAGE_NLST$VolumeRange <- cut(STAGE_NLST$lesionsize, breaks = breaks, include.lowest = FALSE)
  
  # Create a list of M and N stages
  m_stages <- c("M0", "M1")
  n_stages <- c("N0", "N1", "N2", "N3")
  
  
  # Create the matrices
  N0 <- matrix(0, nrow = 4, ncol = length(n_stages), dimnames = list(levels(STAGE_NLST$VolumeRange), n_stages))
  N1 <- matrix(0, nrow = 4, ncol = length(n_stages), dimnames = list(levels(STAGE_NLST$VolumeRange), n_stages))
  N2 <- matrix(0, nrow = 4, ncol = length(n_stages), dimnames = list(levels(STAGE_NLST$VolumeRange), n_stages))
  N3 <- matrix(0, nrow = 4, ncol = length(n_stages), dimnames = list(levels(STAGE_NLST$VolumeRange), n_stages))
  N3[,4] <- 1 # Staying in N3 when you get there
  
  # Create the list
  N_Other_Matrices <- list(N0 = N0, N1 = N1, N2 = N2, N3 = N3)
  
  M0 <- matrix(0, nrow = 4, ncol = length(m_stages), dimnames = list(levels(STAGE_NLST$VolumeRange), m_stages))
  M1 <- matrix(0, nrow = 4, ncol = length(m_stages), dimnames = list(levels(STAGE_NLST$VolumeRange), m_stages))
  M1[,2] <- 1 # Staying in M1 when you get there
  
  M_Other_Matrices <- list(M0 = M0, M1 = M1)
  
  
  rm(N0, N1, N2, N3, M0, M1)
  
  # Update your model's matrices with these new parameters obtained from calibration
  
  M_Other_Matrices$M0[,2] <- param5[,4]/12 #convert to monthly probabilities
  N_Other_Matrices$N0[,2] <- param5[,1]/12 #convert to monthly probabilities
  N_Other_Matrices$N1[,3] <- param5[,2]/12 #convert to monthly probabilities
  N_Other_Matrices$N2[,4] <- param5[,3]/12 #convert to monthly probabilities
  
  # Adjust the remaining columns to account for the change in the initial values
  
  M_Other_Matrices$M0[,1] <- 1 - M_Other_Matrices$M0[,2]
  N_Other_Matrices$N0[,1] <- 1 - N_Other_Matrices$N0[,2]
  N_Other_Matrices$N1[,2] <- 1 - N_Other_Matrices$N1[,3]
  N_Other_Matrices$N2[,3] <- 1 - N_Other_Matrices$N2[,4]
  
  results <- list(M_Other_Matrices = M_Other_Matrices, N_Other_Matrices = N_Other_Matrices, breaks = breaks)
  return(results)
  
}

# This function calculates the probability of death from other causes and from lung cancer
Prob_Death <- function(m_Stages, df_X, OC_Mortality, p_StagesD, m_A) {
  
  # Initialize an empty matrix to store the risks and the new health states
  risks <- matrix(NA, nrow = nrow(m_Stages), ncol = ncol(m_Stages))
  
  # Define the mapping from stages to columns in p_StagesD, that stores survival curves
  stage_mapping <- list(
    "IA1_d" = "I",
    "IA2_d" = "I",
    "IA3_d" = "I",
    "IB_d" = "I",
    "IIA_d" = "II",
    "IIB_d" = "II",
    "IIIA_d" = "III",
    "IIIB_d" = "III",
    "IIIC_d" = "III",
    "IVA_d" = "IV"
  )
  
  find_age_group <- function(age) { # Function to find the age group in months
    age_breaks <- c(0, 12, 60, 120, 180, 240, 300, 360, 420, 480, 540, 600, 660, 720, 780, 840, 900, 960, 1020, Inf)
    age_labels <- c("[0]", "[12-48]", "[60-108]", "[120-168]", "[180-228]", "[240-288]", "[300-348]",
                    "[360-408]", "[420-468]", "[480-528]", "[540-588]", "[600-648]", "[660-708]",
                    "[720-768]", "[780-828]", "[840-888]", "[900-948]", "[960-1008]", "[1020+]")
    age_group_index <- findInterval(age, age_breaks, rightmost.closed = TRUE) # Find the age group index
    if (age_group_index > length(age_labels)) {
      return(tail(age_labels, 1)) # Return the last label if the age is greater than the last break
    } else {
      return(age_labels[age_group_index])
    }
  }
  
  time_since_diagnosis <- matrix(0, nrow = nrow(m_Stages), ncol = length(stage_mapping)) # Initialize time since diagnosis matrix
  death_from_disease <- matrix(FALSE, nrow = nrow(m_Stages), ncol = ncol(m_Stages)) # Initialize matrix to track death from disease
  death_from_other_causes <- matrix(FALSE, nrow = nrow(m_Stages), ncol = ncol(m_Stages)) # Initialize matrix to track death from other causes
  
  for (i in 1:nrow(m_Stages)) {
    death_encountered <- FALSE  # Initialize death encountered variable
    age_at_death <- NA  # Initialize age at death variable
    
    for (j in 1:ncol(m_Stages)) {
      if (death_encountered) {
        m_Stages[i, j] <- "D"  # Set the health state to "D" if death has been encountered
        m_A[i, j] <- age_at_death  # Apply the captured age for subsequent cycles
        next
      }
      
      health_state <- m_Stages[i, j]
      if (!is.na(health_state) && health_state == "D") {
        death_encountered <- TRUE
        age_at_death <- m_A[i, j]  # Capture the age at the point of death
      }
      
      age_group <- find_age_group(m_A[i, j]) # Get the age group
      
      if (age_group %in% rownames(OC_Mortality)) {
        prob_death_other_causes <- OC_Mortality[age_group, paste("OC_Mortality", ifelse(df_X$cigsmok[i] == 1, "S", "ExS"), ifelse(df_X$Gender[i] == "male", "M", "F"), sep = "_")] # Get the probability of death from other causes
        if (runif(1) < prob_death_other_causes) { # Check if the random number is less than the probability, indicating death from other causes happened
          m_Stages[i, j] <- "D"
          death_encountered <- TRUE
          age_at_death <- m_A[i, j]  # Update the age at death here as well
          death_from_other_causes[i, j] <- TRUE
        }
      }
      
      if (!is.na(health_state) && grepl("_d$", health_state)) { # Check if the health state is a disease state
        current_stage_time <- time_since_diagnosis[i, which(names(stage_mapping) == health_state)] + 1 # Increment the time since diagnosis
        time_since_diagnosis[i, which(names(stage_mapping) == health_state)] <- current_stage_time # Update the time since diagnosis
        if (current_stage_time <= nrow(p_StagesD) && stage_mapping[[health_state]] %in% colnames(p_StagesD)) { # Check if the current stage time is within the survival curve matrix
          risks[i, j] <- p_StagesD[current_stage_time, stage_mapping[[health_state]]] # Get the risk of death from lung cancer
          if (runif(1) < risks[i, j]) { # Check if the random number is less than the risk, indicating death from lung cancer happened
            m_Stages[i, j] <- "D"
            death_encountered <- TRUE
            age_at_death <- m_A[i, j]  # Update the age at death here as well
            death_from_disease[i, j] <- TRUE
          }
        }
      }
    }
  }
  
  return(list(
    m_Stages = m_Stages,
    death_from_disease = death_from_disease,
    death_from_other_causes = death_from_other_causes,
    m_A = m_A
  ))
}

# This function simulates the screening process
Screening <- function(m_A, m_PLCO, df_X, m_Stages, m_G, m_DT, screening_interval, min_pkyr, max_age) {
  
  m_G3 <- (4/3)*pi*(m_G/2)^3 # get volume from diameter
  m_DTdays <- m_DT*30.44 # get VDT in days from months
  
  
  # Eligibility based on age and smoking criteria, based on TIDL criteria
  age_eligibility <- m_A >= 720 & m_A <= max_age 
  smoking_eligibility <- df_X$pkyr >= min_pkyr & ((df_X$cigsmok == 1) | (df_X$cigsmok == 0 & df_X$smoking_quit_time < 10))
  
  # Eligibility based on PLCO criteria
  plco_eligibility <- m_PLCO > 0.0260 & m_A <= max_age
  
  # Combine criteria to get the final eligibility matrix
  # We replicate the smoking_eligibility to match the dimensions of age_eligibility and plco_eligibility matrices
  eligibility_criteria_1 <- age_eligibility & matrix(rep(smoking_eligibility, each = ncol(m_A)), nrow = nrow(m_A), byrow = FALSE)
  eligibility_criteria_2 <- plco_eligibility
  
  # Final eligibility matrix
  m_E <- eligibility_criteria_1 | eligibility_criteria_2 # Combine the two eligibility criteria
  # Replace NA values with FALSE in the eligibility matrix
  m_E[is.na(m_E)] <- FALSE
  
  
  # Initialize matrices
  # m_BaselineResult to track screening outcomes: "UNSCREENED", "NEGATIVE", "POSITIVE", "INTERMEDIATE"
  m_BaselineResult <- matrix("UNSCREENED", nrow = nrow(m_E), ncol = ncol(m_E))
  m_ScrStages <- m_Stages
  m_ScrStaged <- matrix(FALSE, nrow = nrow(m_E), ncol = ncol(m_E))
  
  # Sensitivity function by volume sizes, based on MGH-HMS model
  sensitivity <- function(volume) {
    if (volume <= 33.51032) {
      return(0.6)
    } else if (volume > 33.51032 && volume <= 268.0826) {
      return(0.77)
    } else if (volume > 268.0826) {
      return(1.0)
    } else {
      return(0) # Should not happen if volume sizes are within the specified ranges
    }
  }
  
  # Specificity is constant
  specificity <- 0.98
  
  # Initialize matrices to track outcomes
  
  m_FalsePositive <- matrix(FALSE, nrow(m_E), ncol(m_E))
  m_FalseNegative <- matrix(FALSE, nrow(m_E), ncol(m_E))
  m_TruePositive <- matrix(FALSE, nrow(m_E), ncol(m_E))
  m_TrueNegative <- matrix(FALSE, nrow(m_E), ncol(m_E))
  m_Intermediate <- matrix(FALSE, nrow(m_E), ncol(m_E)) # Track intermediate results
  m_FollowUp      <- matrix(FALSE, nrow(m_E), ncol(m_E)) # Track follow-up screenings
  
  # Function to update state and future cycles for positive cases
  updateStateAndFutureCycles <- function(i, cycle, m_ScrStages, m_ScrStaged) {
    
    
    
    
    # Proceed only if the current stage is not NA
    if (!is.na(m_ScrStages[i, cycle])) {
      new_state <- "" # Initialize the new state
      if (m_ScrStages[i, cycle] == "H") {
        new_state <- "H" # Do not alter NHM
      } else {
        new_state <- sub("_u$", "_d", m_ScrStages[i, cycle])
        m_ScrStaged[i, cycle] <- TRUE # putting it here marks only those who switch to not those who are positive but are false positive, m_ScrStaged becomes the second matrix that marks when turni into _d, now they must be combined (m_First_Detected seems to be the other one)
         }
      for (future_cycle in cycle:ncol(m_ScrStages)) {
        m_ScrStages[i, future_cycle] <- new_state # Update the future cycles with the new state
      }
      
    }
    
    next_screen_cycle <- Inf # Set the next screening cycle to infinity, as the individual will not be screened again
  
    return(list(m_ScrStages = m_ScrStages, next_screen_cycle = next_screen_cycle, m_ScrStaged = m_ScrStaged))
    
    
    
    
    
  }
  
  # Function to handle intermediate follow-up based on specific criteria, VDT in days
  handleIntermediateFollowUp <- function(i, cycle, m_BaselineResult, m_DTdays, m_ScrStages, m_TruePositive, m_TrueNegative, screening_interval, m_FollowUp) {
    
    if (m_DTdays[i, cycle] <= 600) { # If VDT is less than or equal to 600 days
      m_BaselineResult[i, cycle] <- "POSITIVE"
      m_TruePositive[i, cycle] <- TRUE
      m_FollowUp[i, cycle] <- TRUE
      pos_result <- updateStateAndFutureCycles(i, cycle, m_ScrStages, m_ScrStaged)
      m_ScrStages <- pos_result$m_ScrStages
      m_ScrStaged <- pos_result$m_ScrStaged
      next_screen_cycle <- pos_result$next_screen_cycle
      #
    } else { # If VDT is greater than 600 days
      m_BaselineResult[i, cycle] <- "NEGATIVE"
      m_TrueNegative[i, cycle] <- TRUE
      m_FollowUp[i, cycle] <- TRUE
      next_screen_cycle <- cycle + screening_interval # Schedule the next screening
    }
    
    return(list(m_BaselineResult = m_BaselineResult, m_ScrStages = m_ScrStages, next_screen_cycle = next_screen_cycle, m_TruePositive = m_TruePositive, m_TrueNegative = m_TrueNegative, m_ScrStaged = m_ScrStaged, m_FollowUp = m_FollowUp))
    
  }
  
  screened_flag <- rep(FALSE, nrow(m_E)) # this is a flag for people who fell above runif(1) <= 0.7 and therefore do not get screened
  condition_checked <- rep(NA, nrow(m_E)) # this is a flag for people who have been checked for runif(1) <= 0.7 and do not do it again
  intermediate_followup_flag <- rep(FALSE, nrow(m_E))  # Assuming m_E is your individual matrix
  
  for (i in 1:nrow(m_E)) {
    
    next_screen_cycle <- 0
    
    for (cycle in 1:ncol(m_E)) {
      if (cycle < next_screen_cycle) next  # Skip to the next cycle if not yet time for the next screening
      
      if (!screened_flag[i] && m_E[i, cycle] && cycle >= next_screen_cycle) { # Check if the individual is eligible for screening and not yet screened
        
        # Check if condition has not been evaluated before for the individual
        if (is.na(condition_checked[i])) {
          # Evaluate the condition and store the outcome
          condition_checked[i] <- runif(1) <= 1
        }
        
        if (condition_checked[i]) {
          
          disease_risk <- m_G3[i, cycle]  # Using m_G3 value for initial guess
          
          # Initial classification, if disease risk (volume of the tumour) is high, classify as POSITIVE, if low, classify as NEGATIVE, otherwise classify as INTERMEDIATE
          if (disease_risk >= 300) { 
            initial_guess <- "POSITIVE"
          } else if (disease_risk < 100) {
            initial_guess <- "NEGATIVE"
          } else {
            initial_guess <- "INTERMEDIATE"
          }
          
          # Assume a decision to screen is made
          test_positive_chance <- runif(1)  # Random chance for determining test outcome
          actual_sensitivity <- sensitivity(m_G3[i, cycle])  # 
          actual_specificity <- specificity  # 
          
          # Apply misclassification based on sensitivity and specificity
          # sensitivity and specificity are the probabilities of a positive test given the disease is present and a negative test given the disease is absent, respectively
          if (initial_guess == "POSITIVE") {
            if (test_positive_chance <= actual_sensitivity) {
              m_BaselineResult[i, cycle] <- "POSITIVE"
              m_TruePositive[i, cycle] <- TRUE
              # Update states and ensure no return for those positively diagnosed
              pos_result <- updateStateAndFutureCycles(i, cycle, m_ScrStages, m_ScrStaged)
              m_ScrStages <- pos_result$m_ScrStages
              m_ScrStaged <- pos_result$m_ScrStaged
              next_screen_cycle <- pos_result$next_screen_cycle
              #
            } else {
              m_BaselineResult[i, cycle] <- "NEGATIVE"  # False negative case
              m_FalseNegative[i, cycle] <- TRUE
              next_screen_cycle <- cycle + screening_interval
            }
          } else if (initial_guess == "NEGATIVE") {
            if (test_positive_chance <= actual_specificity) {
              m_BaselineResult[i, cycle] <- "NEGATIVE"
              m_TrueNegative[i, cycle] <- TRUE
              next_screen_cycle <- cycle + screening_interval
            } else {
              m_BaselineResult[i, cycle] <- "POSITIVE"  # False positive case
              m_FalsePositive[i, cycle] <- TRUE
              pos_result <- updateStateAndFutureCycles(i, cycle, m_ScrStages, m_ScrStaged)
              m_ScrStages <- pos_result$m_ScrStages
              m_ScrStaged <- pos_result$m_ScrStaged
              next_screen_cycle <- pos_result$next_screen_cycle
              #
            }
            
            
            
          } else if (initial_guess == "INTERMEDIATE") {
            if (!intermediate_followup_flag[i]) {
              # First-time INTERMEDIATE classification
              m_BaselineResult[i, cycle] <- "INTERMEDIATE"
              m_Intermediate[i, cycle] <- TRUE
              intermediate_followup_flag[i] <- TRUE  # Flag for follow-up
              next_screen_cycle <- cycle + 3  # Schedule the follow-up
            } else {
              # Follow-up for an INTERMEDIATE classification
              if (cycle == next_screen_cycle && m_BaselineResult[i, cycle - 3] == "INTERMEDIATE") {
                result <- handleIntermediateFollowUp(i, cycle, m_BaselineResult, m_DTdays, m_ScrStages, m_TruePositive, m_TrueNegative, screening_interval, m_FollowUp)
                m_BaselineResult <- result$m_BaselineResult
                m_ScrStages <- result$m_ScrStages
                m_ScrStaged <- result$m_ScrStaged
                next_screen_cycle <- result$next_screen_cycle
                m_TruePositive <- result$m_TruePositive
                m_TrueNegative <- result$m_TrueNegative
                m_FollowUp <- result$m_FollowUp
                intermediate_followup_flag[i] <- FALSE  # Follow-up handled
              }
            }
          }
          
        } else {
          # Individual not screened
          screened_flag[i] <- TRUE
          # No need to update next_screen_cycle here since they won't be screened further
        }
      } 
    }
  }
  
  
  
  
  
  
  # end of screening function
  
  # Returning the results
  return(list(m_G3 = m_G3, m_DTdays = m_DTdays, m_E = m_E, m_BaselineResult = m_BaselineResult, m_ScrStages = m_ScrStages, m_ScrStaged = m_ScrStaged, 
              m_FalsePositive = m_FalsePositive, m_FalseNegative = m_FalseNegative, m_TruePositive = m_TruePositive, m_TrueNegative = m_TrueNegative, m_Intermediate = m_Intermediate, m_FollowUp = m_FollowUp))
}

# This is the function that assigns costs to the individuals
Costs_Function <- function(death_from_disease, death_from_other_causes, death_from_disease_Scr, death_from_other_causes_Scr, 
                           m_Stages, m_ScrStages, m_First_Detection, m_First_Detection_Scr, m_FalsePositive, m_FalseNegative, 
                           m_TruePositive, m_TrueNegative, m_Intermediate, m_FollowUp, Costs) {

  end_of_life_costs <- function(all_deaths, m_Stages, Costs, health_states, Total_Costs_NHM_No_death) { # Function to calculate end-of-life costs
    # Iterate over each individual and cycle
    for (person in 1:nrow(all_deaths)) {
      for (cycle in 1:ncol(all_deaths)) {
        if (cycle == 1) {
          # For the first cycle, skip as we don't adjust pre-existing costs
          next
        } else if (all_deaths[person, cycle]) {
          # Retrieve the health state from the previous cycle
          prev_health_state <- m_Stages[person, cycle - 1]
          
          # Check if the previous health state is not among the specified conditions, end-of-life costs are only applied to diagnosed individuals, it's the incremental cost due to a lung cancer diagnosis
          if (!prev_health_state %in% health_states) {
            # Add the cost to the specific cycle of the individual's death in Total_Costs_NHM_No_death
            Total_Costs_NHM_No_death[person, cycle] <- Total_Costs_NHM_No_death[person, cycle] + Costs["Lump Sum", "C_St_D"]
            
            # Set all future values for the individual to zero
            if (cycle < ncol(all_deaths)) {
              Total_Costs_NHM_No_death[person, (cycle+1):ncol(all_deaths)] <- 0
            }
            
            break # Exit the cycle loop for the current individual as no future costs should be applied
          }
        }
      }
    }
    
    return(Total_Costs_NHM_No_death)
  }
  
  
  
  diagnosis_and_treatment_costs <- function(m_First_Detection, m_Stages, Costs, m_Screening_Count_All, screening_on = TRUE) { # Function to calculate diagnosis and treatment costs
    # Mapping of health states to specific cost columns in the 'Costs' matrix
    state_to_cost_col <- list(
      IA1_d = "C_St_IA1_d",
      IA2_d = "C_St_IA2_d",
      IA3_d = "C_St_IA3_d",
      IB_d = "C_St_IB_d",
      IIA_d = "C_St_IIA_d",
      IIB_d = "C_St_IIB_d",
      IIIA_d = "C_St_IIIA_d",
      IIIB_d = "C_St_IIIB_d",
      IIIC_d = "C_St_IIIC_d",
      IVA_d = "C_St_IVA_d"
    )
    
    
    # Initialize a matrix to store the assigned costs, matching the structure of 'm_First_Detection'
    assigned_costs <- matrix(0, nrow = nrow(m_First_Detection), ncol = ncol(m_First_Detection))
    
    
    #surv_cost <- Costs["Lump Sum", "C_Surv_FU"], this is the cost of follow-up, if needed
    surv_cost <- 0
    
    
    # Loop through each individual and cycle
    for (i in 1:nrow(m_First_Detection)) {
      diagnosis_cycle <- 0  # Track the cycle of first diagnosis
      for (j in 1:ncol(m_First_Detection)) {
        
        # Check if cost should be assigned based on first detection
        if (m_First_Detection[i, j]) {
          diagnosis_cycle <- j  # Update diagnosis cycle
          health_state <- m_Stages[i, j]
          cost_col <- state_to_cost_col[[health_state]] # Get the corresponding cost column based on health state
          
          if (!is.null(cost_col)) { # If a mapping exists
            
            if (screening_on) { # Check if screening is on, add also the cost of screening
            
            if (m_Screening_Count_All[i, j]) {
              # Add costs for diagnosis with screening
              assigned_costs[i, j] <- Costs["Lump Sum", cost_col] + Costs["Lump Sum", "C_Diagnostic_Scr"]
            } else {
              # Add costs for diagnosis without screening
              assigned_costs[i, j] <- Costs["Lump Sum", cost_col] + Costs["Lump Sum", "C_Diagnostic_NHM"]
            }
            
            } else {
              assigned_costs[i, j] <- Costs["Lump Sum", cost_col] + Costs["Lump Sum", "C_Diagnostic_NHM"]
            }
            
            
          }
          
        }
        
        # Check if there's a previous diagnosis and assign surv_cost every three cycles after that
        if (diagnosis_cycle > 0 && (j - diagnosis_cycle) > 0 && (j - diagnosis_cycle) %% 3 == 0 && (j - diagnosis_cycle) <= 60) {
          assigned_costs[i, j] <- assigned_costs[i, j] + surv_cost
        }
      }
    }
    
    return(assigned_costs)
  }
  
  
  replace_ones <- function(row) {
    first_one <- which(row == 1)[1] # Find the first one
    if (!is.na(first_one)) {
      row[-(1:first_one)] <- 0 # Replace ones after the first one with zeros
    }
    return(row)
  }
  
  screening_costs <- function(m_Screening_Count, m_FollowUp, m_FalsePositive, Costs) { # Function to calculate screening costs
    # Initialize a matrix to store the summed costs, matching the structure of 'm_Screening_Count'
    assigned_costs <- matrix(0, nrow = nrow(m_Screening_Count), ncol = ncol(m_Screening_Count))
    
    # Loop through each individual and cycle
    for (i in 1:nrow(m_Screening_Count)) {
      for (j in 1:ncol(m_Screening_Count)) {
        if (m_Screening_Count[i, j]) { # Check if baseline screening cost should be added
          assigned_costs[i, j] <- assigned_costs[i, j] + Costs["Lump Sum", "C_Baseline_Scr"]
        }
        if (m_FollowUp[i, j]) { # Check if follow-up screening cost should be added
          assigned_costs[i, j] <- assigned_costs[i, j] + Costs["Lump Sum", "C_Follow_Up_Scr"]
        }
        if (m_FalsePositive[i, j]) { # Check if false positive cost should be added
          assigned_costs[i, j] <- assigned_costs[i, j] + Costs["Lump Sum", "C_False_Positive_Scr"]
        }
      }
    }
    
    return(assigned_costs)
  }
  
  m_Screening_Count_All <- m_FalsePositive + m_FalseNegative + m_TruePositive + m_TrueNegative + m_Intermediate
  
  
  # Diagnosis and treatment costs # 
  
  diagnosis_and_treatment_costs_matrix_noScr <- diagnosis_and_treatment_costs(    m_First_Detection,    m_Stages, Costs, m_Screening_Count_All, screening_on = FALSE)
  diagnosis_and_treatment_costs_matrix_Scr   <- diagnosis_and_treatment_costs(m_First_Detection_Scr, m_ScrStages, Costs, m_Screening_Count_All, screening_on = TRUE)
  
  
  
  # Screening Costs # 
  
  
  m_Screening_Count <- m_FalsePositive + m_FalseNegative + m_TruePositive + m_TrueNegative + m_Intermediate - m_FollowUp
  
  
  screening_costs_matrix <- screening_costs(m_Screening_Count, m_FollowUp, m_FalsePositive, Costs)
  
  # Diagnosis and treatment costs #
  
  
  Total_Costs_NHM_No_death <- diagnosis_and_treatment_costs_matrix_noScr 
  Total_Costs_Scr_No_death <- diagnosis_and_treatment_costs_matrix_Scr + screening_costs_matrix
  
  
  all_deaths     <- death_from_disease     + death_from_other_causes
  all_deaths_Scr <- death_from_disease_Scr + death_from_other_causes_Scr
  
  
  # Assuming the matrices all_deaths, m_Stages, and Costs are defined in your environment
  
  
  # Usage example with the matrices all_deaths, m_Stages, and Costs, and health_states vector defined
  health_states <- c("H", "IA1_u", "IA2_u", "IA3_u", "IB_u", "IIA_u", "IIB_U", "IIIA_u", "IIIB_u", "IIIC_u", "IVA_u", "D")
  
  
  #  Total Costs # with end of life costs
  
  Total_Costs_NHM   <- end_of_life_costs(    all_deaths,    m_Stages, Costs, health_states, Total_Costs_NHM_No_death)
  Total_Costs_Scr   <- end_of_life_costs(all_deaths_Scr, m_ScrStages, Costs, health_states, Total_Costs_Scr_No_death)
  
  end_of_life_costs_matrix_noScr <- abs(Total_Costs_NHM - Total_Costs_NHM_No_death)
  end_of_life_costs_matrix_Scr   <- abs(Total_Costs_Scr - Total_Costs_Scr_No_death)
  
  
  
  return(list(Total_Costs_NHM = Total_Costs_NHM, Total_Costs_Scr = Total_Costs_Scr, end_of_life_costs_matrix_noScr = end_of_life_costs_matrix_noScr, end_of_life_costs_matrix_Scr = end_of_life_costs_matrix_Scr, diagnosis_and_treatment_costs_matrix_noScr = diagnosis_and_treatment_costs_matrix_noScr, diagnosis_and_treatment_costs_matrix_Scr = diagnosis_and_treatment_costs_matrix_Scr, screening_costs_matrix = screening_costs_matrix, m_Screening_Count = m_Screening_Count, m_FollowUp = m_FollowUp, m_Screening_Count = m_Screening_Count))
  
}

# This function calculates the baseline utilities for each individual based on their health states and profiles
utility_matrix <- function(m_Stages, m_A, df_X, Utilities_dataframe, m_E, m_First_Detection) {
  
 
  
  # Loop through each individual
  for (i in 1:nrow(m_Stages)) {
    for (j in 1:ncol(m_Stages)) {
      # Fetch the age and profile for the current individual and cycle
      age <- m_A[i, j]
      profile <- df_X$smoking_profile[i]
      
      # Determine the utility value based on profile and age
      if (profile == "ExS_Reg") { # Check the profile and assign the corresponding utility value
        utility_value <- Utilities_dataframe$ExS_Reg[Utilities_dataframe$Age == age]
      } else if (profile == "Moderate_S") {
        utility_value <- Utilities_dataframe$Moderate_S[Utilities_dataframe$Age == age]
      } else if (profile == "Heavy_S") {
        utility_value <- Utilities_dataframe$Heavy_S[Utilities_dataframe$Age == age]
      }
      
      # Assign the utility value to m_E
      m_E[i, j] <- utility_value
    }
  }
  
  
  
  
  
  # Predefined health states and their corresponding columns in Utilities_dataframe
  health_states <- c("IA1", "IA2", "IA3", "IB", "IIA", "IIB", "IIIA", "IIIB", "IIIC", "IVA", "D")
  profile_types <- c("ExS_Reg", "Moderate_S", "Heavy_S")
  
  # Loop through each individual
  for (i in 1:nrow(m_Stages)) {
    detected <- FALSE  # To keep track of whether a change has been detected for the individual
    for (j in 1:ncol(m_Stages)) {
      if (m_First_Detection[i, j] && !detected) {
        detected <- TRUE  # Set detected to TRUE upon the first detection
      }
      if (detected) {
        # Get the current health state and check if it is one of the predefined states
        current_state <- m_Stages[i, j]
        if (current_state %in% health_states) {
          # Get the profile and define the column name to fetch from Utilities_dataframe
          profile <- df_X$smoking_profile[i]
          column_name <- sprintf("%s_U_St_%s", profile, current_state)
          
          # Fetch the age and utility value from Utilities_dataframe
          age <- m_A[i, j]
          utility_value <- Utilities_dataframe[Utilities_dataframe$Age == age, column_name]
          
          # Update m_E with the new utility value
          m_E[i, j] <- utility_value
          
        }
      }
    }
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  return(m_E)
  
}

# This function updates the utility matrix based on specific conditions (screening and diagnostic work-up) and disutilities
update_utility_matrix <- function(m_E, condition_matrices, Disutilities_data) {
  
  
  
  
  num_rows <- nrow(m_E)
  num_cols <- ncol(m_E)
  
  # Map condition matrices to the corresponding disutility column names and cycles to apply
  conditions_map <- list(
    m_First_Detection = list(disutility_col = "D_Diagnostic_WU", cycles = 1),
    m_Screening_Count = list(disutility_col = "D_Baseline_Scr", cycles = 2),
    m_Intermediate    = list(disutility_col = "D_Follow_Up_Scr", cycles = 3),
    m_FalsePositive   = list(disutility_col = "D_False_Positive_Scr", cycles = 3)
    
  )
  
  # Iterate through each condition and its corresponding matrix
  for (condition_name in names(condition_matrices)) {
    condition_matrix <- condition_matrices[[condition_name]] # Get the condition matrix for the current condition
    
    # Get the corresponding disutility info
    disutility_info <- conditions_map[[condition_name]]
    if (is.null(disutility_info)) {
      stop(paste("No disutility mapping found for condition:", condition_name))
    }
    
    # Fetch the disutility value and cycles to apply
    disutility_value <- Disutilities_data["Monthly", disutility_info$disutility_col] # Assuming the disutility value is monthly
    cycles_to_apply <- disutility_info$cycles # Number of cycles to apply the disutility value
    
    # Apply the disutility value for the specified cycles where the condition is TRUE
    for (i in 1:num_rows) {
      for (j in 1:num_cols) {
        if (condition_matrix[i, j]) { # Check if the condition is TRUE
          cycle_end <- min(j + cycles_to_apply - 1, num_cols) # Calculate the end cycle for applying the disutility
          m_E[i, j:cycle_end] <- m_E[i, j:cycle_end] - disutility_value # Apply the disutility value
        }
      }
    }
  }
  
  return(m_E)
}

# This is the final function that assigns utilities to the individuals
Utilities <- function(m_First_Detection, m_Screening_Count, m_FalsePositive, m_Intermediate, m_Stages, m_ScrStages, m_A, m_A_Scr, df_X, Utilities_dataframe, Disutilities_data, update_utility_matrix, utility_matrix) {
  
  
  m_Stages <- gsub("(_d)$", "", m_Stages) # Remove the "_d" suffix from the health states because utilities are by aggregate stages (Stage I, doens't matter if IA1, IA2, etc.)
  m_A <- round(m_A/12) # Convert age to years
  m_ScrStages <- gsub("(_d)$", "", m_ScrStages) # same here
  m_A_Scr <- round(m_A_Scr/12) # same here
  
  
  
  # Initialize m_E with the same dimensions as m_Stages, filled with NA values
  m_E <- matrix(NA, nrow = nrow(m_Stages), ncol = ncol(m_Stages))
  
  # Get the baseline utilities for each individual
  m_E_NHM <- utility_matrix(m_Stages = m_Stages, m_A = m_A, df_X = df_X, Utilities_dataframe = Utilities_dataframe, m_E, m_First_Detection = m_First_Detection)
  m_E_Scr <- utility_matrix(m_Stages = m_ScrStages, m_A = m_A_Scr, df_X = df_X, Utilities_dataframe = Utilities_dataframe, m_E, m_First_Detection = m_First_Detection)
  
  
  
  # Second loop to update utilities based on specific factors
  
  # Example of calling the function
  # Assuming the condition matrices are correctly defined (e.g., m_First_Detection, m_Screening_Count, etc.)
  condition_matrices <- list(
    m_First_Detection = m_First_Detection,
    m_Screening_Count = m_Screening_Count,
    m_Intermediate    = m_Intermediate,
    m_FalsePositive   = m_FalsePositive
  )
  
  
  
  m_E_Scr <- update_utility_matrix(m_E = m_E_Scr, condition_matrices = condition_matrices, Disutilities_data = Disutilities_data)
  
  return(list(m_E_NHM = m_E_NHM, m_E_Scr = m_E_Scr))
  
  
}

#### Load Data ####

# Load the NLST lung_cancer dataset
NLST_lung <- read.csv("/.../lung_cancer_d040722 - in use.csv", header = TRUE, sep = ",")
# Load the NLST participant dataset
NLST_participant <- read.csv("/.../participant_d040722 - in_use.csv", header = TRUE, sep = ",")
# Upload yearly calibrated probabilities by volume size
load("/.../Solution_1235.RData")
# Loading survival curves by stage
load("/.../p_StagesD.RData")
# Open other causes mortality values from 2023
load("/.../OC Mortality_2023.RData")
# Open the baseline utilities by age, from Norwegian guidelines
baseline_utilities <- read_excel("/.../Utilities.xlsx", range = "edit!A1:B107")
# Open real costs and utilities
input_costs <- read_excel("/.../Input Costs and Utilities.xlsx", range = "input!A1:B11")
input_utilities <- read_excel("/.../Input Costs and Utilities.xlsx", range = "input!A12:B21")
colnames(input_costs) <- c("Type", "Cost")
colnames(input_utilities) <- c("Type", "Utility")
# Load baseline utilities by age
baseline_utilities <- read_excel("/.../Utilities.xlsx", range = "edit!A1:B107")
# Load conversion parameters for specific profiles
baseline_conversion <- read_excel("/.../Input Costs and Utilities.xlsx", range = "input!E6:L11")
# Load m_G, m_DT, m_Cell_Type, precalculated values
load("/.../m_DT-G-Cell_Type.RData")

#### Preprocessing ####

# Getting individual data for PLCO calculations #

PLCO <- PLCO_Analysis(NLST_participant, check_NA)
PLCO_NLST <- PLCO$PLCO_NLST
df_X_PLCO <- PLCO$df_X_PLCO
n_i <- PLCO$n_i

# Getting Hazard Functions #

m_Initiation_calculated <- 1 # when m_Initiation_calculated is zero, you need to calculate because it is not already calculated, if m_Initiation_calculated 1 do not need to calculate because it's already prepared
# If running with 720 cycles only run once and then load it, it is quicker
if (m_Initiation_calculated == 0) {
  
  
  #### TSCE Inputs ####
  
  # Sub-setting NLST dataset for TSCE
  
  TSCE_NLST <- NLST_participant[c("conflc", "age", "smokeage", "age_quit", "smokeday", "fup_days", "pkyr", "cigsmok")]
  TSCE_NLST$smoking_quit_time <- TSCE_NLST$age - TSCE_NLST$age_quit # calculate time since quit smoking
  TSCE_NLST[c("smoking_quit_time")] <- lapply(TSCE_NLST[c("smoking_quit_time")], function(x) replace(x, is.na(x), 0))  # replace NA with 0
  TSCE_NLST <- TSCE_NLST[TSCE_NLST$smoking_quit_time >= 0, ]  # remove negative values
  TSCE_NLST <- subset(TSCE_NLST, !(smoking_quit_time > 0 & cigsmok == 1))
  
  # Save the original dataset
  TSCE_NLST_0 <- TSCE_NLST
  
  

  # Newest Profiles 
  TSCE_NLST_1 <- subset_and_select(TSCE_NLST, NA, NA, 30, 175, 1) # pk_yr 30-175, cigsmok 1
  TSCE_NLST_2 <- subset_and_select(TSCE_NLST, 0, 5, 30, Inf, 0) # smoking quit time 0-5, pk_yr 30-inf, cigsmok 0
  TSCE_NLST_3 <- subset_and_select(TSCE_NLST, 5, 10, 30, 50, 0) # smoking quit time 5-10, pk_yr 30-50, cigsmok 0
  TSCE_NLST_4 <- subset_and_select(TSCE_NLST, 5, 10, 50, 75, 0) # smoking quit time 5-10, pk_yr 50-75, cigsmok 0
  TSCE_NLST_5 <- subset_and_select(TSCE_NLST, 5, 10, 75, Inf, 0) # smoking quit time 5-10, pk_yr 75-inf, cigsmok 0
  
  
  #### TSCE Applying The Functions ####
  
  # Smokers datasets
  datasets_smokers <- list(TSCE_NLST_0, TSCE_NLST_1)
  # Ex-smokers datasets
  datasets_exsmokers <- list(TSCE_NLST_2, TSCE_NLST_3, TSCE_NLST_4, TSCE_NLST_5)
  
  # Apply the function to each dataset
  Hazard_Functions_smokers <- lapply(datasets_smokers, function(dataset) process_data(dataset, smoker = TRUE))
  Hazard_Functions_exsmokers <- lapply(datasets_exsmokers, process_data)  
  
  # Combine the results
  Hazard_Functions <- c(Hazard_Functions_smokers, Hazard_Functions_exsmokers)
  
  # List of your dataset names
  dataset_names <- c("Hazards_NLST0", "Hazards_NLST1", "Hazards_NLST2", "Hazards_NLST3", "Hazards_NLST4", "Hazards_NLST5")
  
  # Assign names to the processed datasets
  names(Hazard_Functions) <- dataset_names
  
  # Remove temporary variables
  rm(datasets_smokers, datasets_exsmokers, Hazard_Functions_smokers, Hazard_Functions_exsmokers)
  
  # Adjusting hazard functions by month
  # Assuming Hazard_Functions is your list of lists, each containing dataframes
  Hazard_Types <- c("Hazards_NLST0", "Hazards_NLST1", "Hazards_NLST2", "Hazards_NLST3", "Hazards_NLST4", "Hazards_NLST5")
  Smoking_Statuses <- c("smoke", "exSmoke", "nonSmoke")
  
  cycle_length   <- 1/12 # cycle length in years
  
  # Function to convert hazard to probability and monthly
  
  for (hazard in Hazard_Types) {
    for (status in Smoking_Statuses) {
      # Access the dataframe
      df <- Hazard_Functions[[hazard]][[status]]
      
      # Check if the dataframe exists before attempting to modify it
      if (!is.null(df)) {
        # Divide the PredictedHazard column by 12
        df$PredictedHazard <- df$PredictedHazard / 12
        df$PredictedHazard <- convert_hazard_to_probability(df$PredictedHazard)
        
        # Update the dataframe in the list
        Hazard_Functions[[hazard]][[status]] <- df
      }
    }
  }
  
  
  #Cleaning and leaving hazard functions
  rm(plot_data)
  rm(current_data)
  rm(dataset_names)
  rm(datasets_to_plot)
  rm(i)
  rm(TSCE_NLST, TSCE_NLST_0, TSCE_NLST_1, TSCE_NLST_2, TSCE_NLST_3, TSCE_NLST_4, TSCE_NLST_5, TSCE_NLST_6)
  rm(process_data_smokers)
  rm(process_data_exsmokers)
  rm(subset_and_select)
  
  
  
} else {
  
  message("m_Initiation and m_Initiated will be loaded in Calculating m_Initiation")
  
}

#### Microsim model parameters ####

## General model setup 
cycle_length   <- if(!exists("cycle_length")) { cycle_length <- 1/12 } else { cycle_length }     
seed           <- 24   # choose the seed
set.seed(seed)         # set the seed  
n_cycles       <- 20   # time horizon, number of cycles, 720 months works but takes forever
n_i            <- n_i  # number of individuals
two_years      <- 24   # number of months in two years
year           <- 12   # number of months in a year
half_year      <- 6    # number of months in half a year
age_months     <- 1    # is age in months? 1 = yes, 0 = no
str_names_baseline   <- c("NHM", "Screening - Baseline")
str_names_scenario1  <- c("NHM", "Screening - Scenario 1") # change the screening intervals from 2 to 3 years
str_names_scenario2  <- c("NHM", "Screening - Scenario 2") # pkyrs from 35 to 40
str_names_scenario3  <- c("NHM", "Screening - Scenario 3") # pkyrs from 35 to 50
str_names_scenario4  <- c("NHM", "Screening - Scenario 4") # age from 79 to 70 tops (948 to 840)
screening_interval   <- 24 # basecase scenario, 24 months
min_pkyr             <- 35 # basecase scenario, minimum pkyr to be eligible for screening
max_age              <- 948 # basecase scenario, maximum age to be eligible for screening


# the health states of the model:
# “H”, "IA1_u", "IA2_u", "IA3_u", "IA1_d", "IA2_d", "IA3_d", "IB_u", "IB_d", "IIA_u", "IIA_d", "IIB_U", "IIB_d", "IIIA_u", "IIIA_d", "IIIB_u", "IIIB_d", "IIIC_u", "IIIC_d", "IVA_u", "IVA_d", "D"
v_names_states  <- c("H",  # Healthy (H)
                     "IA1_u", # IA_Undetected
                     "IA1_d", # IA_Detected
                     "IA2_u", # IA_Undetected
                     "IA2_d", # IA_Detected
                     "IA3_u", # IA_Undetected
                     "IA3_d", # IA_Detected
                     "IB_u", # IB_Undetected
                     "IB_d", # IB_Detected
                     "IIA_u", # IIA_Undetected
                     "IIA_d", # IIA_Detected
                     "IIB_u", # IIB_Undetected
                     "IIB_d", # IIB_Detected
                     "IIIA_u", # IIIA_Undetected
                     "IIIA_d", # IIIA_Detected
                     "IIIB_u", # IIIB_Undetected
                     "IIIB_d", # IIIB_Detected
                     "IIIC_u", # IIIB_Undetected
                     "IIIC_d", # IIIB_Detected
                     "IVA_u", # IV_Undetected
                     "IVA_d", # IV_Detected
                     "D")  # Dead (D)
v_names_cycles  <- paste("cycle", 0:n_cycles)    # cycle names
n_states        <- length(v_names_states)        # number of health states                   

### Discounting factors 
d_c <- 0.04 # annual discount rate for costs 
d_e <- 0.04 # annual discount rate for QALYs


# Get the log-transformed rates, Table 57, Clinical presentation rates with progression heterogeneity. Detection probabilities by stage
Snowsill_log_rates <- c(-2.583, -2.120, -1.876, -2.299, -1.302, -0.518, -0.084)

# Convert log-transformed rates to original scale
Snowsill_original_scale <- exp(Snowsill_log_rates)

# Vector of detection probabilities by stage
detection_probabilities <- c("IA1_u" = Snowsill_original_scale[1], "IA2_u" = Snowsill_original_scale[1], "IA3_u" = Snowsill_original_scale[1], "IB_u" = Snowsill_original_scale[2], "IIA_u" = Snowsill_original_scale[3], "IIB_u" = Snowsill_original_scale[4], "IIIA_u" = Snowsill_original_scale[5], "IIIB_u" = Snowsill_original_scale[6], "IIIC_u" = Snowsill_original_scale[6], "IVA_u" = Snowsill_original_scale[7])
detection_probabilities <- detection_probabilities/12 # trying to see if they were yearly calculated


### Discount weight for costs and effects 
v_dwc   <- 1 / ((1 + (d_e * cycle_length)) ^ (0:n_cycles)) # discount weight for costs
v_dwe   <- 1 / ((1 + (d_c * cycle_length)) ^ (0:n_cycles)) # discount weight for effects

# Within-cycle correction (WCC) - method  options Simpson's 1/3 rule, "half-cycle" or "none" 


v_wcc    <- gen_wcc(n_cycles = n_cycles, method = "Simpson1/3") # vector of wcc


# Individual level characteristics

# Initial age in months for every individual

v_age0  <- rep(480, n_i) # everyone starts at 40 years old

# Smoking Status Vector
S_t <- rep(0, n_i) # initial smoking status vector

# Specify the initial health state of the individuals everyone begins in the healthy state (in this example)
v_M_init          <- rep("H", times = n_i) # everyone begins in the healthy state

# Create a dataframe with the individual characteristics

df_X <- data.frame(ID = 1:n_i, Age = v_age0, M_init = v_M_init, Status = S_t) 
df_X <- cbind(df_X, df_X_PLCO)
df_X$Status <- ifelse(df_X$cigsmok == 1, "S", "ExS")
df_X$Gender <- sample(c("male", "female"), nrow(df_X), replace = TRUE)

# update S_t
S_t <- df_X$Status



# Gompertz Growth Function parameters definition


# Define your parameters

y0 <- 1 # initial population size
K <- 260 # carrying capacity
lambda <- 5 # lag time


# Vectorize the derivative of the growth function
d_growth_function_vec <- Vectorize(d_growth_function)

# Getting Probabilities of Nodal Involvement and Distant Metastasis #

Probabilities <- Transition_Probabilities(NLST_lung)

M_Other_Matrices <- Probabilities$M_Other_Matrices
N_Other_Matrices <- Probabilities$N_Other_Matrices
breaks <- Probabilities$breaks


# Create the state table
state_table <- data.frame(
  T0 = c("T1A", "T1B", "T1C", "T2A", "T2B", "T3", "T4", "M1"),
  N0 = c("IA1", "IA2", "IA3", "IB", "IIA", "IIB","IIIA","IVA"),
  N1 = c("IIB", "IIB", "IIB", "IIB", "IIB", "IIIA", "IIIA", "IVA"),
  N2 = c("IIIA", "IIIA", "IIIA", "IIIA", "IIIA" , "IIIB" , "IIIB" , "IVA"),
  N3 = c("IIIB" , "IIIB" , "IIIB" , "IIIB" , "IIIB" ,"IIIC", "IIIC", "IVA")
)


# Convert the hazard rates to probabilities for p_StagesD # 
# No need for OC because they state they are already probabilities

for (i in 6:9) {
  p_StagesD[, i] <- convert_hazard_to_probability(p_StagesD[, i])
}

# Calculating/Retrieving m_Initiation #

# Check if m_Initiation has been calculated, otherwise calculate it
if (m_Initiation_calculated == 0) {
  
  
  m_A <-  matrix(nrow = n_i, ncol = n_cycles + 1, 
                 dimnames = list(paste("ind"  , 1:n_i, sep = " "), 
                                 paste("cycle", 0:n_cycles, sep = " ")))  
  
  age_m_Initiation <- 480:(480 + n_cycles) # it should be - 1 but since we have cycle 0 it is okay like this
  
  m_A[] <- rep(age_m_Initiation, each=nrow(m_A))
  
  
  m_Hazard <- matrix(nrow = n_i, ncol = n_cycles + 1, 
                     dimnames = list(paste("ind"  , 1:n_i, sep = " "), 
                                     paste("cycle", 0:n_cycles, sep = " ")))
  
  # Step 1: Apply baseline hazard values based on age and smoking status
  for (ind in 1:nrow(m_A)) {
    for (cycle in 1:ncol(m_A)) {
      age <- m_A[ind, cycle]  # Age from m_A
      smoking_status <- df_X$Status[ind]  # Assuming 'S', 'ExS', etc.
      
      # Initialize a variable to store the hazard value
      hazard_value <- NA
      
      # Fetch the hazard value based on smoking status and age
      if (smoking_status == "S") {
        # Accessing the smoke data frame for smokers
        smoke_df <- Hazard_Functions$Hazards_NLST0$smoke
        hazard_value <- smoke_df[smoke_df$Age == age, "PredictedHazard", drop = TRUE]
      } else if (smoking_status == "ExS") {
        # Accessing the exSmoke data frame for ex-smokers
        exSmoke_df <- Hazard_Functions$Hazards_NLST0$exSmoke
        hazard_value <- exSmoke_df[exSmoke_df$Age == age, "PredictedHazard", drop = TRUE]
      }
      # Assuming a similar structure exists for non-smokers ('NS'), adjust accordingly.
      
      # Check and assign the hazard value
      if (length(hazard_value) == 1 && !is.na(hazard_value)) {
        m_Hazard[ind, cycle] <- hazard_value
      } else {
        # Logging for any issues encountered
        message("Issue with hazard value for individual ", ind, ", cycle ", cycle, ", age ", age, ": ", 
                length(hazard_value), " values found.")
      }
    }
  }
  
  # Step 2: Refine hazard values for specific profiles
  # Define thresholds and statuses for the profiles
  pkyr_thresholds <- list(c(30, 175), c(30, Inf), c(30, 50), c(50, 75), c(75, Inf))
  smoking_quit_time_thresholds <- list(NULL, c(0, 5), c(5, 10), c(5, 10), c(5, 10))
  cigsmok_status <- c(1, 0, 0, 0, 0)
  # Assuming m_A, df_X, m_Hazard initialization, and Hazard_Functions structure are set up correctly.
  
  # Iterate through each individual and cycle for specific profiles
  
  for (ind in 1:nrow(df_X)) {
    for (cycle in 1:ncol(m_A)) {
      age <- m_A[ind, cycle]  # Get age from m_A
      cigsmok <- df_X$cigsmok[ind]  # Smoking status
      pkyr <- df_X$pkyr[ind]  # Pack-years
      smoking_quit_time <- df_X$smoking_quit_time[ind]  # Quit time
      
      # Initialize the flag to check if a dataset was found
      dataset_found <- FALSE
      
      # Loop to determine the appropriate dataset based on the profile
      for (i in 1:5) {
        if (cigsmok_status[i] == cigsmok && !is.null(pkyr_thresholds[[i]])) {
          if (pkyr >= pkyr_thresholds[[i]][1] && (is.infinite(pkyr_thresholds[[i]][2]) || pkyr < pkyr_thresholds[[i]][2])) { # Check if the pack-years fall within the threshold
            # Additional check for ex-smokers
            if (is.null(smoking_quit_time_thresholds[[i]]) || 
                (!is.na(smoking_quit_time) && smoking_quit_time > smoking_quit_time_thresholds[[i]][1] && smoking_quit_time <= smoking_quit_time_thresholds[[i]][2])) { # Check if the quit time falls within the threshold
              
              dataset_key <- ifelse(cigsmok_status[i] == 1, "smoke", "exSmoke") # Determine the dataset key based on the smoking status
              dataset <- Hazard_Functions[[paste0("Hazards_NLST", i)]][[dataset_key]] # Access the dataset
              
              matched_hazard <- dataset %>%  # Match the hazard value based on age
                filter(Age == age) %>%  # Filter by age
                pull(PredictedHazard) # Extract the hazard value
              
              if (length(matched_hazard) == 1) { # Check if a single value was found
                m_Hazard[ind, cycle] <- matched_hazard # Assign the matched hazard value
                dataset_found <- TRUE # Set the flag to indicate a dataset was found
                break  # Exit the loop after finding a match
              }
            }
          }
        }
      }
      
      # If no dataset was found, retain the existing value in m_Hazard
      if (!dataset_found) {
        message("No dataset found for individual ", ind, ", cycle ", cycle, ", age ", age, ", cigsmok ", cigsmok, ", pkyr ", pkyr, ", quit time ", smoking_quit_time)
      }
    }
  }
  
  
  # calculating m_Initiation
  
  m_Initiation <-  matrix("H", nrow = n_i, ncol = n_cycles + 1, 
                          dimnames = list(paste("ind"  , 1:n_i, sep = " "), 
                                          paste("cycle", 0:n_cycles, sep = " ")))  
  
  m_Initiated <- matrix(FALSE, nrow = n_i, ncol = n_cycles + 1, 
                        dimnames = list(paste("ind"  , 1:n_i, sep = " "), 
                                        paste("cycle", 0:n_cycles, sep = " ")))
  
  
  set.seed(seed) # For reproducibility
  
  # Simulate events without unnecessary updates to m_Initiation
  for(i in 1:n_i){
    for(j in 2:n_cycles){
      # Generate a random probability
      random_prob <- runif(1)
      # Check if an event occurs (random_prob less than or equal to the hazard rate)
      # and no event has previously occurred for this individual
      if(random_prob <= m_Hazard[i, j]){
        # Update the current and all subsequent cycles to "IA1_u" in one step
        m_Initiation[i, j:n_cycles] <- "IA1_u"
        m_Initiated[i, j] <- TRUE # track the first occurrence
        # No further checks are needed for this individual, move to the next one
        break
      }
      # If no event occurs, simply continue to the next cycle
    }
  }
  
  
} else {
  
  load("/.../m_Initiation.RData") # Load the pre-calculated m_Initiation after the first time you run it 
  
  # Dinamically adjust the dimensions of m_Initiation and m_Initiated
  m_Initiated <- m_Initiated[, 1:(n_cycles + 1)]
  m_Initiation <- m_Initiation[, 1:(n_cycles + 1)]
  
  
  
  m_A <-  matrix(nrow = n_i, ncol = n_cycles + 1, 
                 dimnames = list(paste("ind"  , 1:n_i, sep = " "), 
                                 paste("cycle", 0:n_cycles, sep = " ")))  
  
  age_m_Initiation <- 480:(480 + n_cycles) # it should be - 1 but since we have cycle 0 it is okay like this
  
  m_A[] <- rep(age_m_Initiation, each=nrow(m_A))
  
  
  
  
  
}

# Define costs and utilities #

# Define costs
cost_values <- c(
  input_costs$Cost[input_costs$Type == "C_St_I_d"], input_costs$Cost[input_costs$Type == "C_St_I_d"], input_costs$Cost[input_costs$Type == "C_St_I_d"], input_costs$Cost[input_costs$Type == "C_St_I_d"], # Costs stage IA1, IA2, IA3, IB
  input_costs$Cost[input_costs$Type == "C_St_II_d"], input_costs$Cost[input_costs$Type == "C_St_II_d"], # Costs stage IIA, IIB
  input_costs$Cost[input_costs$Type == "C_St_III_d"], input_costs$Cost[input_costs$Type == "C_St_III_d"], input_costs$Cost[input_costs$Type == "C_St_III_d"], # Costs stage IIIA, IIIB, IIIC
  input_costs$Cost[input_costs$Type == "C_St_IV_d"], # Cost stage IVA
  input_costs$Cost[input_costs$Type == "C_St_D"], # Incremental End-of-life costs
  input_costs$Cost[input_costs$Type == "C_Baseline_Scr"], input_costs$Cost[input_costs$Type == "C_Follow_Up_Scr"], input_costs$Cost[input_costs$Type == "C_False_Positive_Scr"], # Screening Costs: "C_Baseline_Scr", "C_Follow_Up_Scr", "C_False_Positive_Scr"
  input_costs$Cost[input_costs$Type == "C_Diagnostic_NHM"], input_costs$Cost[input_costs$Type == "C_Diagnostic_Scr"] # C_Diagnostic_NHM and C_Diagnostic_Scr
  
)



# Create the matrix with costs
Costs_data <- matrix(cost_values, nrow = 1, ncol = 16, dimnames = list("Lump Sum", c(
  "C_St_IA1_d", "C_St_IA2_d", "C_St_IA3_d", "C_St_IB_d", "C_St_IIA_d", 
  "C_St_IIB_d", "C_St_IIIA_d", "C_St_IIIB_d", "C_St_IIIC_d", "C_St_IVA_d", 
  "C_St_D", "C_Baseline_Scr", "C_Follow_Up_Scr", "C_False_Positive_Scr",
  "C_Diagnostic_NHM", "C_Diagnostic_Scr"
)))



# Define utilities

disutility_values <- c(
  
  
  input_utilities$Utility[input_utilities$Type == "U_St_I"], input_utilities$Utility[input_utilities$Type == "U_St_I"], input_utilities$Utility[input_utilities$Type == "U_St_I"], input_utilities$Utility[input_utilities$Type == "U_St_I"], # Utilities Stage I
  input_utilities$Utility[input_utilities$Type == "U_St_II"], input_utilities$Utility[input_utilities$Type == "U_St_II"], # Utilities Stage II
  input_utilities$Utility[input_utilities$Type == "U_St_III"], input_utilities$Utility[input_utilities$Type == "U_St_III"], input_utilities$Utility[input_utilities$Type == "U_St_III"], # Utilities Stage III
  input_utilities$Utility[input_utilities$Type == "U_St_IV"], # Utilities Stage IV
  input_utilities$Utility[input_utilities$Type == "U_St_D"], # Utilities death
  input_utilities$Utility[input_utilities$Type == "D_Baseline_Scr"], input_utilities$Utility[input_utilities$Type == "D_Follow_Up_Scr"], input_utilities$Utility[input_utilities$Type == "D_False_Positive_Scr"], # Screening Disutility: "D_Baseline_Scr", "D_Follow_Up_Scr", "D_False_Positive_Scr"
  input_utilities$Utility[input_utilities$Type == "D_Diagnostic_WU"] # Utility decrement of diagnostic work-up
  
)



# Create the matrix with utilities

Disutilities_data <- matrix(disutility_values, nrow = 1, ncol = 15, dimnames = list("Monthly", c(
  
  
  "U_St_IA1", "U_St_IA2", "U_St_IA3", "U_St_IB", "U_St_IIA", "U_St_IIB", "U_St_IIIA", "U_St_IIIB", "U_St_IIIC", "U_St_IVA",
  "U_St_D",
  "D_Baseline_Scr", "D_Follow_Up_Scr", "D_False_Positive_Scr",
  "D_Diagnostic_WU"
  
)))



# Modify baseline here
baseline_utilities <- as.data.frame(baseline_utilities)
colnames(baseline_utilities) <- c("Age", "H")

baseline_utilities$ExS_Reg <- 0
baseline_utilities$Moderate_S <- 0
baseline_utilities$Heavy_S <- 0


# Convert baseline_utilities to specific profiles, according to the conversion rates
baseline_conversion <- as.data.frame(baseline_conversion)

colnames(baseline_conversion) <- c("Profile", "16-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-100")
rownames(baseline_conversion) <- c("ExS_Occ", "ExS_Reg", "Light_S", "Moderate_S", "Heavy_S")



# Sort of hazard ratios for the risk profiles based on their smoking status and age group

baseline_utilities$ExS_Reg[1:25] <- baseline_utilities$H[1:25] * baseline_conversion["ExS_Reg", "16-24"]
baseline_utilities$ExS_Reg[26:35] <- baseline_utilities$H[26:35] * baseline_conversion["ExS_Reg", "25-34"]
baseline_utilities$ExS_Reg[36:55] <- baseline_utilities$H[36:55] * baseline_conversion["ExS_Reg", "35-44"]
baseline_utilities$ExS_Reg[56:65] <- baseline_utilities$H[56:65] * baseline_conversion["ExS_Reg", "45-54"]
baseline_utilities$ExS_Reg[66:75] <- baseline_utilities$H[66:75] * baseline_conversion["ExS_Reg", "55-64"]
baseline_utilities$ExS_Reg[76:106] <- baseline_utilities$H[76:106] * baseline_conversion["ExS_Reg", "75-100"]


baseline_utilities$Moderate_S[1:25] <- baseline_utilities$H[1:25] * baseline_conversion["Moderate_S", "16-24"]
baseline_utilities$Moderate_S[26:35] <- baseline_utilities$H[26:35] * baseline_conversion["Moderate_S", "25-34"]
baseline_utilities$Moderate_S[36:55] <- baseline_utilities$H[36:55] * baseline_conversion["Moderate_S", "35-44"]
baseline_utilities$Moderate_S[56:65] <- baseline_utilities$H[56:65] * baseline_conversion["Moderate_S", "45-54"]
baseline_utilities$Moderate_S[66:75] <- baseline_utilities$H[66:75] * baseline_conversion["Moderate_S", "55-64"]
baseline_utilities$Moderate_S[76:106] <- baseline_utilities$H[76:106] * baseline_conversion["Moderate_S", "75-100"]

baseline_utilities$Heavy_S[1:25] <- baseline_utilities$H[1:25] * baseline_conversion["Heavy_S", "16-24"]
baseline_utilities$Heavy_S[26:35] <- baseline_utilities$H[26:35] * baseline_conversion["Heavy_S", "25-34"]
baseline_utilities$Heavy_S[36:55] <- baseline_utilities$H[36:55] * baseline_conversion["Heavy_S", "35-44"]
baseline_utilities$Heavy_S[56:65] <- baseline_utilities$H[56:65] * baseline_conversion["Heavy_S", "45-54"]
baseline_utilities$Heavy_S[66:75] <- baseline_utilities$H[66:75] * baseline_conversion["Heavy_S", "55-64"]
baseline_utilities$Heavy_S[76:106] <- baseline_utilities$H[76:106] * baseline_conversion["Heavy_S", "75-100"]




Utilities_dataframe <- data.frame(baseline_utilities)



Utilities_dataframe$ExS_Reg_U_St_IA1 <- Utilities_dataframe$ExS_Reg * Disutilities_data["Monthly", "U_St_IA1"]
Utilities_dataframe$ExS_Reg_U_St_IA2 <- Utilities_dataframe$ExS_Reg * Disutilities_data["Monthly", "U_St_IA2"]
Utilities_dataframe$ExS_Reg_U_St_IA3 <- Utilities_dataframe$ExS_Reg * Disutilities_data["Monthly", "U_St_IA3"]
Utilities_dataframe$ExS_Reg_U_St_IB <- Utilities_dataframe$ExS_Reg * Disutilities_data["Monthly", "U_St_IB"]
Utilities_dataframe$ExS_Reg_U_St_IIA <- Utilities_dataframe$ExS_Reg * Disutilities_data["Monthly", "U_St_IIA"]
Utilities_dataframe$ExS_Reg_U_St_IIB <- Utilities_dataframe$ExS_Reg * Disutilities_data["Monthly", "U_St_IIB"]
Utilities_dataframe$ExS_Reg_U_St_IIIA <- Utilities_dataframe$ExS_Reg * Disutilities_data["Monthly", "U_St_IIIA"]
Utilities_dataframe$ExS_Reg_U_St_IIIB <- Utilities_dataframe$ExS_Reg * Disutilities_data["Monthly", "U_St_IIIB"]
Utilities_dataframe$ExS_Reg_U_St_IIIC <- Utilities_dataframe$ExS_Reg * Disutilities_data["Monthly", "U_St_IIIC"]
Utilities_dataframe$ExS_Reg_U_St_IVA <- Utilities_dataframe$ExS_Reg * Disutilities_data["Monthly", "U_St_IVA"]
Utilities_dataframe$ExS_Reg_U_St_D <- Utilities_dataframe$ExS_Reg * Disutilities_data["Monthly", "U_St_D"]


Utilities_dataframe$Moderate_S_U_St_IA1 <- Utilities_dataframe$Moderate_S * Disutilities_data["Monthly", "U_St_IA1"]
Utilities_dataframe$Moderate_S_U_St_IA2 <- Utilities_dataframe$Moderate_S * Disutilities_data["Monthly", "U_St_IA2"]
Utilities_dataframe$Moderate_S_U_St_IA3 <- Utilities_dataframe$Moderate_S * Disutilities_data["Monthly", "U_St_IA3"]
Utilities_dataframe$Moderate_S_U_St_IB <- Utilities_dataframe$Moderate_S * Disutilities_data["Monthly", "U_St_IB"]
Utilities_dataframe$Moderate_S_U_St_IIA <- Utilities_dataframe$Moderate_S * Disutilities_data["Monthly", "U_St_IIA"]
Utilities_dataframe$Moderate_S_U_St_IIB <- Utilities_dataframe$Moderate_S * Disutilities_data["Monthly", "U_St_IIB"]
Utilities_dataframe$Moderate_S_U_St_IIIA <- Utilities_dataframe$Moderate_S * Disutilities_data["Monthly", "U_St_IIIA"]
Utilities_dataframe$Moderate_S_U_St_IIIB <- Utilities_dataframe$Moderate_S * Disutilities_data["Monthly", "U_St_IIIB"]
Utilities_dataframe$Moderate_S_U_St_IIIC <- Utilities_dataframe$Moderate_S * Disutilities_data["Monthly", "U_St_IIIC"]
Utilities_dataframe$Moderate_S_U_St_IVA <- Utilities_dataframe$Moderate_S * Disutilities_data["Monthly", "U_St_IVA"]
Utilities_dataframe$Moderate_S_U_St_D <- Utilities_dataframe$Moderate_S * Disutilities_data["Monthly", "U_St_D"]

Utilities_dataframe$Heavy_S_U_St_IA1 <- Utilities_dataframe$Heavy_S * Disutilities_data["Monthly", "U_St_IA1"]
Utilities_dataframe$Heavy_S_U_St_IA2 <- Utilities_dataframe$Heavy_S * Disutilities_data["Monthly", "U_St_IA2"]
Utilities_dataframe$Heavy_S_U_St_IA3 <- Utilities_dataframe$Heavy_S * Disutilities_data["Monthly", "U_St_IA3"]
Utilities_dataframe$Heavy_S_U_St_IB <- Utilities_dataframe$Heavy_S * Disutilities_data["Monthly", "U_St_IB"]
Utilities_dataframe$Heavy_S_U_St_IIA <- Utilities_dataframe$Heavy_S * Disutilities_data["Monthly", "U_St_IIA"]
Utilities_dataframe$Heavy_S_U_St_IIB <- Utilities_dataframe$Heavy_S * Disutilities_data["Monthly", "U_St_IIB"]
Utilities_dataframe$Heavy_S_U_St_IIIA <- Utilities_dataframe$Heavy_S * Disutilities_data["Monthly", "U_St_IIIA"]
Utilities_dataframe$Heavy_S_U_St_IIIB <- Utilities_dataframe$Heavy_S * Disutilities_data["Monthly", "U_St_IIIB"]
Utilities_dataframe$Heavy_S_U_St_IIIC <- Utilities_dataframe$Heavy_S * Disutilities_data["Monthly", "U_St_IIIC"]
Utilities_dataframe$Heavy_S_U_St_IVA <- Utilities_dataframe$Heavy_S * Disutilities_data["Monthly", "U_St_IVA"]
Utilities_dataframe$Heavy_S_U_St_D <- Utilities_dataframe$Heavy_S * Disutilities_data["Monthly", "U_St_D"]

# Create the smoking_profile vector based on the conditions
df_X <- df_X %>%
  mutate(smoking_profile = case_when(
    cigsmok == 0 ~ "ExS_Reg",
    cigsmok == 1 & smokeday < 20 ~ "Moderate_S",
    cigsmok == 1 & smokeday >= 20 ~ "Heavy_S",
    TRUE ~ NA_character_  # Handle any cases that don't match the conditions
  ))


#### Microsimulation function ####

# Define the core Microsimulation function
LCModel <- function() {
  
  set.seed(seed) # set a seed to be able to reproduce the same results
  check_inputs(n_i, df_X, seed, cycle_length) # check the inputs
  # create three matrices called m_Initiation, m_C and m_E
  # number of rows is equal to the n_i, the number of columns is equal to n_cycles  
  # (the initial state and all the n_cycles cycles)
  # m_Initiation is used to store the health state information over time for every individual
  # m_C is used to store the costs information over time for every individual
  # m_E is used to store the effects information over time for every individual
  # m_G is used to store the growth information over time for every individual
  
  
  
  
  
  
  ###
  
  if (n_cycles == 720) {
    
    print("Using imported m_G, m_DT, m_Cell_Type")
    
  } else {
    
    
    m_Cell_Type <- matrix(NA, nrow = n_i, ncol = n_cycles + 1, 
                          dimnames = dimnames(m_Initiation))
    
    
    cell_type_probabilities <- c(0.47, 0.16, 0.37)
    names(cell_type_probabilities) <- c("1", "2", "3")
    
    
    
    m_G <-  matrix(0, nrow = n_i, ncol = n_cycles + 1,
                   dimnames = list(paste("ind"  , 1:n_i, sep = " "), 
                                   paste("cycle", 0:n_cycles, sep = " ")))
    
    m_DT <-  matrix(0, nrow = n_i, ncol = n_cycles + 1,
                    dimnames = list(paste("ind"  , 1:n_i, sep = " "), 
                                    paste("cycle", 0:n_cycles, sep = " ")))
    
    
    # Initialize a vector to store the event_occurred status for each row
    event_occurred <- rep(FALSE, n_i) # initialize all individuals to have no event occurred
    event_occurred_IA <- rep(FALSE, n_i) # initialize all individuals to have no event occurred
    counter_IA <- rep(0, nrow(m_DT))
    
    # Initialize a vector to store the t_G value for each row
    t_G <- rep(0, n_i) # initialize all individuals to have t_G = 0
    t_VDT <- rep(0, n_i) # initialize all individuals to have t_VDT = 0
    # Initialize a vector to keep track of individuals for whom "D" has occurred
    
    d_occurred <- rep(FALSE, nrow(m_G))
    d_occurred <- rep(FALSE, nrow(m_DT))
    
    
    # 47% adeno
    # 16% squamous
    # 37% other
    
    # Labels in mumax and m_Cell_Type  
    # Value 1 is "Adenocarcinoma", Value 2 is "Small Cell", and Value 3 "Other"
    
    
    # Iterate over each row
    for (i in 1:n_i) {
      # Flag to indicate if the individual has transitioned to IA1_u
      transitioned <- FALSE
      
      # Iterate over each cycle for the individual
      for (j in 1:(n_cycles + 1)) {
        if (m_Initiation[i, j] == "IA1_u" && !transitioned) {
          # Transition detected, assign a cell type based on cell_type_probabilities
          # for this and all subsequent cycles
          cell_type <- sample(x = c("1", "2", "3"), size = 1, replace = TRUE, prob = cell_type_probabilities)
          m_Cell_Type[i, j:ncol(m_Cell_Type)] <- cell_type
          transitioned <- TRUE
        }
      }
    }
    print("Cell types assigned")
    # m_Cell_Type now contains the assigned cell types based on m_Initiation states
    
    
    # Growth rate parameters are obtained from the Harvard model
    # Adenocarcinoma (1) is from Adenocarcinoma, Small Cell (2) is from Small cell, Other (3) is from Squamous Cell
    
    
    # Generate mumax for each cell type
    mumax <- rlnorm(n_i, meanlog = -7.765, sdlog = 0.5504)*(33000/n_cycles) # generate a mumax for each individual
    mumax_1 <- rlnorm(n_i, meanlog = -7.765, sdlog = 0.5504) * (33000 / n_cycles)
    mumax_2 <- rlnorm(n_i, meanlog = -5.44357, sdlog = 0.611485) * (33000 / n_cycles)
    mumax_3 <- rlnorm(n_i, meanlog = -6.6111, sdlog = 0.7935) * (33000 / n_cycles)
    
    for (t in 1:n_cycles) {
      for(i in 1:nrow(m_G)) {
        if(d_occurred[i]) {
          m_G[i, t:n_cycles] <- NA
          next
        }
        
        if(m_Initiation[i, t] == "IA1_u" && !event_occurred[i]) {
          event_occurred[i] <- TRUE
        }
        
        if(event_occurred[i]) {
          # Check cell type and select corresponding mumax
          cell_type <- m_Cell_Type[i, t]
          if (!is.na(cell_type)) {
            if (cell_type == 1) {
              mumax_current <- mumax_1[i]
            } else if (cell_type == 2) {
              mumax_current <- mumax_2[i]
            } else if (cell_type == 3) {
              mumax_current <- mumax_3[i]
            } else {
              mumax_current <- mumax[i] # Default case if needed
            }
          } else {
            mumax_current <- mumax[i] # Use general mumax if cell type is NA
          }
          
          #epsilon <- abs(rnorm(1, mean = 0, sd = 0.25))
          m_G[i, t] <- growth_function(y0, K, mumax_current, lambda, t_G[i])# + epsilon
          t_G[i] <- t_G[i] + 1
        }
        
        if(any(m_Initiation[i, t] == "D")) {
          d_occurred[i] <- TRUE
        }
      } 
    } # end of the growth loop for the individuals
    
    print("End of Growth Loop")
    
    for (t in 1:n_cycles) { 
      for(i in 1:nrow(m_DT)) {
        if(d_occurred[i]) {
          m_DT[i, t:n_cycles] <- NA 
          next
        }
        
        if(m_Initiation[i, t] == "IA1_u" && !event_occurred_IA[i]) {
          event_occurred_IA[i] <- TRUE
          counter_IA[i] <- counter_IA[i] + 1
          t_VDT[i] <- t
        }
        
        if(event_occurred_IA[i]) {
          # Check cell type and select corresponding mumax
          cell_type <- m_Cell_Type[i, t]
          if (!is.na(cell_type)) {
            if (cell_type == 1) {
              mumax_current <- mumax_1[i]
            } else if (cell_type == 2) {
              mumax_current <- mumax_2[i]
            } else if (cell_type == 3) {
              mumax_current <- mumax_3[i]
            } else {
              mumax_current <- mumax[i] # Default case if needed
            }
          } else {
            mumax_current <- mumax[i] # Use general mumax if cell type is NA
          }
          
          remaining_cycles <- seq(t, n_cycles+1)
          d_growth_function_sym <- d_growth_function_vec(remaining_cycles - t_VDT[i], mumax_current)
          td_sym  <- log(2) / (d_growth_function_sym / growth_function(y0, K, mumax_current, lambda, remaining_cycles - t_VDT[i])) 
          #epsilon <- rnorm(length(remaining_cycles), mean = 0, sd = 0.025) 
          m_DT[i, remaining_cycles] <- td_sym# + epsilon
        }
        
        if(any(m_Initiation[i, t] == "D")) {
          d_occurred[i] <- TRUE 
        }
      } 
    } # end of the doubling time loop for the individuals
    
    
    print("End of doubling time loop")
    
    
    
  }
  
  ###
  
  
  
  
  
  
  
  
  
  
  # m_count is for NHM
  m_count <- matrix(0, nrow = length(v_names_states), ncol = n_cycles + 1,
                    dimnames = list(v_names_states, paste("cycle", 0:n_cycles, sep = " ")))
  
  m_count[1, 1] <- n_i        # initial health state at cycle 0 for individual i
  
  # m_count_Scr is for screening
  
  m_count_Scr <- m_count
  
  m_count_Scr[1, 1] <- n_i
  
  
  
  
  
  
  proportion_sick <- numeric(n_cycles + 1)
  proportion_sick[1] <- 1
  
  # create a matrix to store the age information over time for every individual
  
  
  
  if (always == 1) {
    
    # Loading "m_Stages", "m_First_Detection", "m_PLCO", "m_T", "m_N", "m_M"
    load("/.../NHM.RData")
    
  } else {
    
    # Calculating "m_Stages", "m_First_Detection", "m_PLCO", "m_T", "m_N", "m_M" 
    
    # create a matrix to store the PLCO score over time for every individual not dead or detected
    
    m_PLCO <-  matrix(nrow = n_i, ncol = n_cycles + 1, 
                      dimnames = list(paste("ind"  , 1:n_i, sep = " "), 
                                      paste("cycle", 0:n_cycles, sep = " ")))
    
    m_PLCO[, 1] <- 0
    
    
    # Make the matrix for T stage of lung cancer, only based on size
    
    # Assuming m_G is your matrix with volume sizes
    m_T <- matrix(nrow = nrow(m_G), ncol = ncol(m_G))  # Create an empty matrix with the same dimensions as m_G
    
    # Apply the function to each element in m_G and store the results in m_T
    for (i in 1:nrow(m_G)) {
      for (j in 1:ncol(m_G)) {
        m_T[i, j] <- find_tumor_T_stage(m_G[i, j])
      }
    }
    
    # m_T is made
    
    
    # Making m_M, matrix of M0 and M1
    
    # Create a new matrix with the same dimensions as m_G
    m_Volume_Intervals <- m_G #outcome_SoC$m_G
    
    
    # Replace each cell with the corresponding interval index, to be used for sampling the states
    for(i in 1:nrow(m_G)) {
      for(j in 1:ncol(m_G)) {
        m_Volume_Intervals[i,j] <- find_custom_interval(m_G[i,j], breaks)
      }
    }
    
    states <- dimnames(M_Other_Matrices$M0)[[2]] 
    
    # Create a new matrix to store the drawn states
    m_M <- m_Volume_Intervals  # start with a copy of m_Volume_Intervals
    
    # Draw a state for each individual at each cycle
    for(i in 1:nrow(m_Volume_Intervals)) {
      
      first_drawn <- FALSE  # Flag to track if the first non-NA state has been drawn for this individual
      for(j in 1:ncol(m_Volume_Intervals)) {
        if (!is.na(m_Volume_Intervals[i,j]) && m_Volume_Intervals[i,j] <= nrow(M_Other_Matrices$M0)) {
          if (!first_drawn) {
            # For the first non-NA draw for this individual, use M_First_Matrix
            m_M[i,j] <- "M0"
            first_drawn <- TRUE
          } else {
            
            
            # For subsequent draws, use the matrix corresponding to the previous state
            prev_state <- m_M[i, j-1]
            m_M[i,j] <- sample(states, size = 1, prob = M_Other_Matrices[[prev_state]][m_Volume_Intervals[i,j],])
            
          }
        } else {
          m_M[i,j] <- NA  # or any other value you want to assign
        }
      }
    }
    
    
    # m_M is made
    
    
    # Making m_N, matrix of N0, N1, N2, N3 #
    
    #finding intervals like m_M
    
    states <- dimnames(N_Other_Matrices$N0)[[2]] 
    
    # Create a new matrix to store the drawn states
    m_N <- m_Volume_Intervals  # start with a copy of m_Volume_Intervals
    
    # Draw a state for each individual at each cycle
    for(i in 1:nrow(m_Volume_Intervals)) {
      first_drawn <- FALSE  # Flag to track if the first non-NA state has been drawn for this individual
      for(j in 1:ncol(m_Volume_Intervals)) {
        if (!is.na(m_Volume_Intervals[i,j]) && m_Volume_Intervals[i,j] <= nrow(N_Other_Matrices$N0)) {
          if (!first_drawn) {
            # For the first non-NA draw for this individual, use N_First_Matrix
            m_N[i,j] <- "N0"
            first_drawn <- TRUE
          } else {
            # For subsequent draws, use the matrix corresponding to the previous state
            prev_state <- m_N[i, j-1]
            m_N[i,j] <- sample(states, size = 1, prob = N_Other_Matrices[[prev_state]][m_Volume_Intervals[i,j],])
          }
        } else {
          m_N[i,j] <- NA  # or any other value you want to assign
        }
      }
    }
    
    # m_N, m_M and m_T are made
    
    print("Done with T, N and M")
    
    # Apply the function to each corresponding element in m_T, m_N, and m_M
    states_list <- apply_get_state(m_T, m_N, m_M, state_table, detection_probabilities)
    states <- states_list$states  # Extract the states matrix from the list returned by apply_get_state
    first_detection <- states_list$first_detection  # Extract the first_detection matrix from the list returned by apply_get_state
    
    
    # Save the states into a matrix called m_Stages
    m_Stages <- matrix(states, nrow = nrow(m_T), ncol = ncol(m_T))
    m_First_Detection <- matrix(first_detection, nrow = nrow(m_T), ncol = ncol(m_T))
    
    print("m_Stages and m_First_Detection are made")
    
    no_PLCO <- c("D", "IA1_d", "IA2_d", "IA3_d", "IB_d", "IIA_d", "IIB_d", "IIIA_d", "IIIB_d", "IIIC_d", "IVA_d")
    # if you are dead, or detected we do not screen so no need to calculate PLCO
    for (i in 1:nrow(m_PLCO)) {
      for (j in 1:(n_cycles + 1)) {
        # Check if the individual is in no_PLCO
        if (m_Stages[i, j] %in% no_PLCO) {
          break  # Skip to the next individual
        }
        
        # Get the age from the m_A matrix
        age <- m_A[i, j]
        
        # Call the mplcom2012 function with the appropriate parameters
        result <- mplcom2012(
          age = age,  # Use the age from the m_A matrix
          race = df_X$race[i],
          education = df_X$educat[i],
          bmi = df_X$BMI[i],
          copd = df_X$diagcopd[i],
          cancer_hist = df_X$cancer_hist[i],
          family_hist_lung_cancer = df_X$family_hist[i],
          smoking_status = df_X$cigsmok[i],
          smoking_intensity = df_X$smokeday[i],
          duration_smoking = df_X$smokeyr[i],
          smoking_quit_time = df_X$smoking_quit_time[i],
          age_months = age_months
        )
        
        # Store the result in the m_PLCO matrix
        m_PLCO[i, j] <- result$prob
        
        
      }
    } #end plco loop
    
    print("PLCO scores done")
    
    
    
  }
  
  
  
  
  
  # Screening
  
  Screening_Results <- Screening(m_A = m_A, m_PLCO = m_PLCO, df_X = df_X, m_Stages = m_Stages, m_G = m_G, m_DT = m_DT, screening_interval = screening_interval, min_pkyr = min_pkyr, max_age = max_age)
  
  m_G3              <- Screening_Results$m_G3
  m_DTdays          <- Screening_Results$m_DTdays
  m_E               <- Screening_Results$m_E
  m_BaselineResult  <- Screening_Results$m_BaselineResult
  m_ScrStages       <- Screening_Results$m_ScrStages
  m_ScrStaged       <- Screening_Results$m_ScrStaged
  m_FalsePositive   <- Screening_Results$m_FalsePositive
  m_FalseNegative   <- Screening_Results$m_FalseNegative
  m_TruePositive    <- Screening_Results$m_TruePositive
  m_TrueNegative    <- Screening_Results$m_TrueNegative
  m_Intermediate    <- Screening_Results$m_Intermediate
  m_FollowUp        <- Screening_Results$m_FollowUp
  
  # Merging m_First_Detection and m_ScrStaged to get the final screening detection matrix AND only keep the first TRUE values
  # m_First_Detection should be in the environment already
  
  
  #m_First_Detection <- SCR_outcomes_SoC$m_First_Detection
  
  
  # Combine the two matrices
  combined_matrix <- m_First_Detection | m_ScrStaged
  
  # Assuming combined_matrix is the matrix you want to check if there are more rows with at least more than one TRUE value
  #any(apply(combined_matrix, 1, function(row) {
  #  sum(row) > 1
  #}))
  
  # Only keep the first TRUE value in each row
  m_First_Detection_Scr <- keep_first_true(combined_matrix)
  
  print("Screening done")
  
  # Death Function enters the chat
  
  
  if (scenarios == 1) {
    
    
    load("/.../NHM_death.RData")
    
    
    
  } else {
    
    print("loaded stuff, calculating death")
    # Death no screening
    
    
    Death_Output <- Prob_Death(m_Stages = m_Stages, 
                               df_X = df_X, 
                               OC_Mortality = OC_Mortality, 
                               p_StagesD = p_StagesD, 
                               m_A = m_A)
    
    m_Stages <- Death_Output$m_Stages
    death_from_disease <- Death_Output$death_from_disease
    death_from_other_causes <- Death_Output$death_from_other_causes
    m_A <- Death_Output$m_A
    
    print("Death no screening done")
    
  }
  
  
  # Death with Screening
  
  
  Death_Output_Scr <- Prob_Death(m_Stages = m_ScrStages, 
                                 df_X = df_X, 
                                 OC_Mortality = OC_Mortality, 
                                 p_StagesD = p_StagesD, 
                                 m_A = m_A)
  
  m_ScrStages <- Death_Output_Scr$m_Stages
  death_from_disease_Scr <- Death_Output_Scr$death_from_disease
  death_from_other_causes_Scr <- Death_Output_Scr$death_from_other_causes
  m_A_Scr <- Death_Output_Scr$m_A
  
  print("Death with screening done")
  
  for (t in 1:n_cycles) {
    
    
    
    # Update the count matrix
    for (state in v_names_states) {
      
      m_count[state, t + 1] <- sum(m_Stages[, t + 1] == state)
      m_count_Scr[state, t + 1] <- sum(m_ScrStages[, t + 1] == state)
      
    }
    
    
    # Calculate the proportion for cycle t
    proportion_sick[t + 1] <- sum(m_Stages[, t + 1] != "D" & m_Stages[, t + 1] != "H") / sum(m_Stages[, t + 1] != "D")
    
    
    
  }
  
  # Add proportion_sick as a row to m_count
  m_count <- rbind(m_count, proportion_sick = proportion_sick)
  
  print("m_Count done")
  
  # Define the prevalence intervals
  prevalence_intervals <- seq(480, 540, by = year)
  
  # Initialize m_I
  m_I <- matrix(nrow = length(paste(prevalence_intervals, prevalence_intervals + year, sep = "-")), ncol = n_cycles + 1, 
                dimnames = list(paste(prevalence_intervals, prevalence_intervals + year, sep = "-"), 
                                paste("cycle", 0:n_cycles, sep = " ")))
  
  # Loop m_I
  for (t in 0:n_cycles) {
    # Subset data for the current cycle
    cycle_data_A <- m_A[, t]
    cycle_data_M <- m_Stages[, t]
    
    # Loop over age intervals
    for (i in 1:length(prevalence_intervals)) {
      # Define age interval
      age_interval <- c(prevalence_intervals[i], prevalence_intervals[i] + year)
      
      # Tag individuals in the age interval
      tagged_individuals <- which(cycle_data_A >= age_interval[1] & cycle_data_A < age_interval[2])
      
      # Subset data for the tagged individuals
      tagged_data <- cycle_data_M[tagged_individuals]
      
      # Calculate and store the ratio
      m_I[i, t] <- calculate_ratio(tagged_data, c("H", "D"))
    }
    
    
    # Display simulation progress
    if(t/(n_cycles/10) == round(t/(n_cycles/10), 0)) { # display progress every 10%
      cat('\r', paste("m_I Loop" , t/n_cycles * 100, "% done", sep = " "))
    }
    
    
  } # end of the loop for m_I
  
  print("m_I done")
  
  # Cost Function
  
  Costs_List <- Costs_Function(death_from_disease = death_from_disease, death_from_other_causes = death_from_other_causes, death_from_disease_Scr = death_from_disease_Scr, death_from_other_causes_Scr = death_from_other_causes_Scr, m_Stages = m_Stages, m_ScrStages = m_ScrStages, m_First_Detection = m_First_Detection, m_First_Detection_Scr = m_First_Detection_Scr, m_FalsePositive = m_FalsePositive, m_FalseNegative = m_FalseNegative, m_TruePositive = m_TruePositive, m_TrueNegative = m_TrueNegative, m_Intermediate = m_Intermediate, m_FollowUp = m_FollowUp,  Costs = Costs_data)
  
  
  Total_Costs_NHM                             <- Costs_List$Total_Costs_NHM
  Total_Costs_Scr                             <- Costs_List$Total_Costs_Scr
  end_of_life_costs_matrix_noScr              <- Costs_List$end_of_life_costs_matrix_noScr
  end_of_life_costs_matrix_Scr                <- Costs_List$end_of_life_costs_matrix_Scr
  diagnosis_and_treatment_costs_matrix_noScr  <- Costs_List$diagnosis_and_treatment_costs_matrix_noScr
  diagnosis_and_treatment_costs_matrix_Scr    <- Costs_List$diagnosis_and_treatment_costs_matrix_Scr
  screening_costs_matrix                      <- Costs_List$screening_costs_matrix
  m_Baseline_Screen                           <- Costs_List$m_Baseline_Screen
  m_Follow_Up_Screens                         <- Costs_List$m_Follow_Up_Screens
  m_FollowUp                                  <- Costs_List$m_FollowUp
  m_Screening_Count                           <- Costs_List$m_Screening_Count
  
  print("Costs done")
  
  
  # Apply Utilities Function and get m_U, matrix with utilities
  
  
  
  
  Utilities_List <- Utilities(m_First_Detection = m_First_Detection, m_Screening_Count = m_Screening_Count, m_FalsePositive = m_FalsePositive, m_Intermediate = m_Intermediate, m_Stages = m_Stages, m_ScrStages = m_ScrStages, m_A = m_A, m_A_Scr = m_A_Scr, df_X = df_X, Utilities_dataframe = Utilities_dataframe, Disutilities_data = Disutilities_data, update_utility_matrix = update_utility_matrix, utility_matrix = utility_matrix)
  m_E_NHM        <- Utilities_List$m_E_NHM
  m_E_Scr        <- Utilities_List$m_E_Scr
  
  print("Utilities done")
  
  
  
  
  # Discounted total expected QALYs and Costs per strategy and apply cycle correction ####
  tc_NHM      <- Total_Costs_NHM %*% (v_dwc * v_wcc)  # total (discounted and cycle corrected) cost per individual
  tc_Scr      <- Total_Costs_Scr %*% (v_dwc * v_wcc)  # total (discounted and cycle corrected) cost per individual
  
  
  te_NHM      <- m_E_NHM %*% (v_dwe * v_wcc)  # total (discounted and cycle corrected) QALYs per individual 
  te_Scr      <- m_E_Scr %*% (v_dwe * v_wcc)  # total (discounted and cycle corrected) QALYs per individual
  
  
  tc_NHM_hat <- mean(tc_NHM)  # average (discounted and cycle corrected) cost
  te_NHM_hat <- mean(te_NHM)  # average (discounted and cycle corrected) QALY
  
  tc_Scr_hat <- mean(tc_Scr)  # average (discounted and cycle corrected) cost
  te_Scr_hat <- mean(te_Scr)  # average (discounted and cycle corrected) QALY
  
  
  
  # store the results from the simulation in a list
  results <- list(m_Stages = m_Stages, m_G = m_G, m_Initiation = m_Initiation, m_Initiated = m_Initiated, 
                  m_T = m_T, m_M = m_M, m_N = m_N, m_DT = m_DT, m_count = m_count, m_I = m_I, m_A = m_A, 
                  m_PLCO = m_PLCO, death_from_disease=death_from_disease, death_from_other_causes=death_from_other_causes, 
                  m_Cell_Type = m_Cell_Type, m_First_Detection = m_First_Detection, m_Volume_Intervals = m_Volume_Intervals, 
                  m_G3 = m_G3, m_DTdays = m_DTdays, m_E = m_E, m_BaselineResult = m_BaselineResult, m_ScrStages = m_ScrStages, 
                  m_ScrStaged = m_ScrStaged, m_FalsePositive = m_FalsePositive, m_FalseNegative = m_FalseNegative, 
                  m_TruePositive = m_TruePositive, m_TrueNegative = m_TrueNegative, m_A_Scr = m_A_Scr, 
                  death_from_disease_Scr = death_from_disease_Scr, death_from_other_causes_Scr = death_from_other_causes_Scr, 
                  m_First_Detection_Scr = m_First_Detection_Scr, m_Intermediate = m_Intermediate, m_count_Scr = m_count_Scr, 
                  m_I = m_I, Total_Costs_NHM = Total_Costs_NHM, Total_Costs_Scr = Total_Costs_Scr, 
                  end_of_life_costs_matrix_noScr = end_of_life_costs_matrix_noScr, end_of_life_costs_matrix_Scr = end_of_life_costs_matrix_Scr, 
                  diagnosis_and_treatment_costs_matrix_noScr = diagnosis_and_treatment_costs_matrix_noScr, 
                  diagnosis_and_treatment_costs_matrix_Scr = diagnosis_and_treatment_costs_matrix_Scr, 
                  screening_costs_matrix = screening_costs_matrix, m_Baseline_Screen = m_Baseline_Screen, 
                  m_Follow_Up_Screens = m_Follow_Up_Screens, m_FollowUp = m_FollowUp, m_Screening_Count = m_Screening_Count,
                  m_E_NHM = m_E_NHM, m_E_Scr = m_E_Scr, tc_NHM = tc_NHM, tc_Scr = tc_Scr, te_NHM = te_NHM, te_Scr = te_Scr,
                  tc_Scr_hat = tc_Scr_hat, te_Scr_hat = te_Scr_hat, tc_NHM_hat = tc_NHM_hat, te_NHM_hat = te_NHM_hat)
  
  
  
  # Check_column_sums function
  #check_column_sums(m_count, n_i, n_states) # check if the column sums are equal to n_i
  # Check_zeros_after_values function
  #check_zeros_after_values(m_count, v_names_states, n_cycles) # check if there are no zeros after first non-zero value in each row of m_count
  
  return(results)  # return the results
  
} # end of the MicroSim function  

# Run Microsimulation
always <- 1
# Baseline Microsimulation
outcomes_SoC   <- LCModel()

print("Base case done")


scenarios <- 1

# Price Sensitivity Analysis
# Sensitivity 1 
originalcost <- Costs_data["Lump Sum", "C_Baseline_Scr"]


Costs_data["Lump Sum", "C_Baseline_Scr"] <- originalcost * 0.5

outcomes_Sensitivity1 <- LCModel()

print("Sensitivity 1 done")

# Sensitivity 2

Costs_data["Lump Sum", "C_Baseline_Scr"] <- originalcost * 0.75

outcomes_Sensitivity2 <- LCModel()

print("Sensitivity 2 done")

# Sensitivity 3

Costs_data["Lump Sum", "C_Baseline_Scr"] <- originalcost * 1.25

outcomes_Sensitivity3 <- LCModel()

print("Sensitivity 3 done")

# Sensitivity 4

Costs_data["Lump Sum", "C_Baseline_Scr"] <- originalcost * 1.5

outcomes_Sensitivity4 <- LCModel()

print("Sensitivity 4 done")



# Scenario 1: updating screening interval from 24 to 36 months
screening_interval <- 36 # baseline scenario, 24 months
outcomes_Scenario1 <- LCModel()

print("Scenario 1 done")

# putting back baseline scenario, 24 months
screening_interval <- 24 # 


# Scenario 2: updating screening eligibility criterion of pkyr, moving the min_pkyr from 35 to 40
min_pkyr <- 40 # baseline scenario, 35 pkyr
outcomes_Scenario2 <- LCModel()

print("Scenario 2 done")

# Scenario 3: updating screening eligibility criterion of pkyr, moving the min_pkyr from 35 to 50
min_pkyr <- 50 # baseline scenario, 35 pkyr
outcomes_Scenario3 <- LCModel()

print("Scenario 3 done")

# putting back baseline scenario, 35 pkyr
min_pkyr <- 35 #

# Scenario 4: updating screening eligibility criterion of pkyr, moving the max_age from 948 to 840
max_age <- 840 # baseline scenario, 948 months
outcomes_Scenario4 <- LCModel()

print("Scenario 4 done")

# putting back baseline scenario, 948 months
max_age <- 948 #

# Scenario 5: updating screening interval from 24 to 12 months

screening_interval <- 12 # baseline scenario, 24 months

outcomes_Scenario5 <- LCModel()

print("Scenario 5 done")

# putting back baseline scenario, 24 months
screening_interval <- 24 # 



# Define your intervals
intervals <- seq(0, n_cycles, by = year)
cycle_count <- n_cycles
#compare_counts(outcomes_SoC, intervals, cycle_count)

# Calculate the incidence by age group and stage distribution and store it, for both NHM (SoC) and Screening


### Without Screening ###

# Total Incidence
outcomes_SoC <- IA_D_Incidence_Function("m_Incidence", outcomes_SoC$m_Stages, outcomes_SoC$m_First_Detection, n_cycles, year, outcomes_SoC, count_alive, count_new_detections_intervals) # Calculate the incidence of IA_D

# Incidence by age
outcomes_SoC <- IA_D_Incidence_Function_By_Age("m_Incidence_ByAge_Eligible", outcomes_SoC$m_Stages, outcomes_SoC$m_First_Detection, outcomes_SoC$m_E, n_cycles, year, outcomes_SoC, interval_type = "year", find_age_custom_interval, count_alive, EligibilityRestriction = TRUE)

outcomes_SoC <- IA_D_Incidence_Function_By_Age("m_Incidence_ByAge", outcomes_SoC$m_Stages, outcomes_SoC$m_First_Detection, outcomes_SoC$m_E, n_cycles, year, outcomes_SoC, interval_type = "year", find_age_custom_interval, count_alive, EligibilityRestriction = FALSE)


# Calculate the stage distribution and store it in the outcomes_SoC list
outcomes_SoC$Stage_distribution <- Stage_distribution_function(outcomes_SoC$m_Stages, intervals, "year")  # "year", "cycle_length", or "n_cycles" | year, month, entire time horizon
outcomes_Scenario1$Stage_distribution <- Stage_distribution_function(outcomes_Scenario1$m_Stages, intervals, "year")  # "year", "cycle_length", or "n_cycles" | year, month, entire time horizon
outcomes_Scenario2$Stage_distribution <- Stage_distribution_function(outcomes_Scenario2$m_Stages, intervals, "year")  # "year", "cycle_length", or "n_cycles" | year, month, entire time horizon
outcomes_Scenario3$Stage_distribution <- Stage_distribution_function(outcomes_Scenario3$m_Stages, intervals, "year")  # "year", "cycle_length", or "n_cycles" | year, month, entire time horizon
outcomes_Scenario4$Stage_distribution <- Stage_distribution_function(outcomes_Scenario4$m_Stages, intervals, "year")  # "year", "cycle_length", or "n_cycles" | year, month, entire time horizon
outcomes_Scenario5$Stage_distribution <- Stage_distribution_function(outcomes_Scenario5$m_Stages, intervals, "year")  # "year", "cycle_length", or "n_cycles" | year, month, entire time horizon



### With Screening ###

# Total Incidence
outcomes_SoC <- IA_D_Incidence_Function("m_Incidence_Scr", outcomes_SoC$m_ScrStages, outcomes_SoC$m_First_Detection_Scr, n_cycles, year, outcomes_SoC, count_alive, count_new_detections_intervals) # Calculate the incidence of IA_D

# Incidence by age

outcomes_SoC <- IA_D_Incidence_Function_By_Age("m_Incidence_ByAge_Eligible_Scr", outcomes_SoC$m_ScrStages, outcomes_SoC$m_First_Detection_Scr, outcomes_SoC$m_E, n_cycles, year, outcomes_SoC, interval_type = "year", find_age_custom_interval, count_alive, EligibilityRestriction = TRUE)

outcomes_SoC <- IA_D_Incidence_Function_By_Age("m_Incidence_ByAge_Scr", outcomes_SoC$m_ScrStages, outcomes_SoC$m_First_Detection_Scr, outcomes_SoC$m_E, n_cycles, year, outcomes_SoC, interval_type = "year", find_age_custom_interval, count_alive, EligibilityRestriction = FALSE)


# Calculate the stage distribution and store it in the outcomes_SoC list
outcomes_SoC$Stage_distribution_Scr <- Stage_distribution_function(outcomes_SoC$m_ScrStages, intervals, "year")  # "year", "cycle_length", or "n_cycles" | year, month, entire time horizon
outcomes_Scenario1$Stage_distribution_Scr <- Stage_distribution_function(outcomes_Scenario1$m_ScrStages, intervals, "year")  # "year", "cycle_length", or "n_cycles" | year, month, entire time horizon
outcomes_Scenario2$Stage_distribution_Scr <- Stage_distribution_function(outcomes_Scenario2$m_ScrStages, intervals, "year")  # "year", "cycle_length", or "n_cycles" | year, month, entire time horizon
outcomes_Scenario3$Stage_distribution_Scr <- Stage_distribution_function(outcomes_Scenario3$m_ScrStages, intervals, "year")  # "year", "cycle_length", or "n_cycles" | year, month, entire time horizon
outcomes_Scenario4$Stage_distribution_Scr <- Stage_distribution_function(outcomes_Scenario4$m_ScrStages, intervals, "year")  # "year", "cycle_length", or "n_cycles" | year, month, entire time horizon
outcomes_Scenario5$Stage_distribution_Scr <- Stage_distribution_function(outcomes_Scenario5$m_ScrStages, intervals, "year")  # "year", "cycle_length", or "n_cycles" | year, month, entire time horizon


# checking number of IA_u
outcomes_SoC <- IA_D_Incidence_Function_By_Age("m_Incidence_ByAge_IA_u", outcomes_SoC$m_Initiation, outcomes_SoC$m_Initiated, outcomes_SoC$m_E, n_cycles, year, outcomes_SoC, interval_type = "year", find_age_custom_interval, count_alive, EligibilityRestriction = FALSE)

print("Done with Incidences")


# Cost-effectiveness analyses in script CEA.R

# Quick mortality check

# Base case
print(paste("There is a difference of", (sum(as.numeric(outcomes_SoC$death_from_disease)) - sum(as.numeric(outcomes_SoC$death_from_disease_Scr))), " deaths from LC between NHM and Base case scenario"))
print(paste("There is a difference of", (sum(as.numeric(outcomes_SoC$death_from_other_causes)) - sum(as.numeric(outcomes_SoC$death_from_other_causes_Scr))), " deaths from other causes between NHM and Base case scenario"))
# Scenario 1
print(paste("There is a difference of", (sum(as.numeric(outcomes_Scenario1$death_from_disease)) - sum(as.numeric(outcomes_Scenario1$death_from_disease_Scr))), " deaths from LC between NHM and Scenario 1"))
print(paste("There is a difference of", (sum(as.numeric(outcomes_Scenario1$death_from_other_causes)) - sum(as.numeric(outcomes_Scenario1$death_from_other_causes_Scr))), " deaths from other causes between NHM and Scenario 1"))
# Scenario 2
print(paste("There is a difference of", (sum(as.numeric(outcomes_Scenario2$death_from_disease)) - sum(as.numeric(outcomes_Scenario2$death_from_disease_Scr))), " deaths from LC between NHM and Scenario 2"))
print(paste("There is a difference of", (sum(as.numeric(outcomes_Scenario2$death_from_other_causes)) - sum(as.numeric(outcomes_Scenario2$death_from_other_causes_Scr))), " deaths from other causes between NHM and Scenario 2"))
# Scenario 3
print(paste("There is a difference of", (sum(as.numeric(outcomes_Scenario3$death_from_disease)) - sum(as.numeric(outcomes_Scenario3$death_from_disease_Scr))), " deaths from LC between NHM and Scenario 3"))
print(paste("There is a difference of", (sum(as.numeric(outcomes_Scenario3$death_from_other_causes)) - sum(as.numeric(outcomes_Scenario3$death_from_other_causes_Scr))), " deaths from other causes between NHM and Scenario 3"))
# Scenario 4
print(paste("There is a difference of", (sum(as.numeric(outcomes_Scenario4$death_from_disease)) - sum(as.numeric(outcomes_Scenario4$death_from_disease_Scr))), " deaths from LC between NHM and Scenario 4"))
print(paste("There is a difference of", (sum(as.numeric(outcomes_Scenario4$death_from_other_causes)) - sum(as.numeric(outcomes_Scenario4$death_from_other_causes_Scr))), " deaths from other causes between NHM and Scenario 4"))



end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken # print the time taken to run the script

#### Other ####


# Acknowlegdement

#For this work we made use of the template developed by the Decision Analysis in R for Technologies in Health (DARTH) workgroup: <http://darthworkgroup.com>.
#The notation of our code is based on the following provided framework and coding convention: Alarid-Escudero, F., Krijkamp, E., Pechlivanoglou, P. et al. A Need for Change! A Coding Framework for Improving Transparency in Decision Modeling. PharmacoEconomics 37, 1329–1339 (2019). <https://doi.org/10.1007/s40273-019-00837-x>.
#Other work from DARTH can be found on the website: <http://darthworkgroup.com/publications/>