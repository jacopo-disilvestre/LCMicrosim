#### Setup ####

start.time <- Sys.time()

if (!require('pacman')) install.packages('pacman'); library(pacman)
# load (install if required) packages from CRAN
p_load("devtools", "dplyr", "scales", "ellipse", "ggplot2", "lazyeval", "igraph", "truncnorm", "ggraph", "reshape2", "knitr", "markdown", "stringr", "dampack", "msce", "approx", "nloptr")

#### Load and analysis #### 

# load everything that was done before this point 
load("/.../Calibration_Preparation.RData")
# and load targets (M/N_First_Matrices) obtained from NLST
load("/.../MN_Targets.RData")
MN_Target <- cbind(N_Target, M_Target)
MN_Target <- as.matrix(MN_Target)
# load m_Volume_Intervals, is m_G already with size groups defined
load("/.../m_Volume_Intervals.RData")
# Open OC_Mortality which contains other cause mortality probabilities, obtained from the script "Workshop - OC My Mortality.R"
load("/.../OC Mortality - year.RData")

# Initialize these two functions first #

apply_get_state <- function(m_T, m_N, m_M, state_table, detection_probabilities) {
  # Define a local version of get_state inside apply_get_state
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
  
  detected <- rep(FALSE, nrow(m_T))
  last_diagnosed <- rep(NA, nrow(m_T))
  states <- matrix(NA, nrow = nrow(m_T), ncol = ncol(m_T))
  first_detection <- matrix(FALSE, nrow = nrow(m_T), ncol = ncol(m_T)) # Initialize first_detection matrix
  
  for (i in 1:nrow(m_T)) {
    for (j in 1:ncol(m_T)) {
      if (!is.na(last_diagnosed[i])) {
        states[i, j] <- last_diagnosed[i]
      } else {
        states[i, j] <- get_state(m_T[i, j], m_N[i, j], m_M[i, j], detected[i])
        if (!is.na(m_T[i, j]) && m_T[i, j] != "T0" && !detected[i]) { # Check if not previously detected
          detected[i] <- grepl("_d", states[i, j])
          if (detected[i]) {
            last_diagnosed[i] <- states[i, j]
            first_detection[i, j] <- TRUE # Mark the first detection
          }
        }
      }
    }
  }
  
  return(list(states = states, first_detection = first_detection)) # Return both states and first_detection matrices
}

Prob_Death <- function(m_Stages, df_X, OC_Mortality, m_A) {
  
  # Initialize matrices to store death states from other causes
  death_from_other_causes <- matrix(FALSE, nrow = nrow(m_Stages), ncol = ncol(m_Stages))
  
  # Define the function to find age group
  find_age_group <- function(age) {
    age_breaks <- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, Inf)
    age_labels <- c("[0]", "[1-4]", "[5-9]", "[10-14]", "[15-19]", "[20-24]", "[25-29]", "[30-34]", "[35-39]", "[40-44]", "[45-49]", "[50-54]", "[55-59]", "[60-64]", "[65-69]", "[70-74]", "[75-79]", "[80-84]", "[85+]")
    
    age_group_index <- findInterval(age, age_breaks, rightmost.closed = TRUE)
    if (age_group_index > length(age_labels)) {
      return(tail(age_labels, 1))
    } else {
      return(age_labels[age_group_index])
    }
  }
  
  for (i in 1:nrow(m_Stages)) {
    death_encountered <- FALSE
    age_at_death <- NA  # Initialize age at death variable
    
    for (j in 1:ncol(m_Stages)) {
      if (death_encountered) {
        m_Stages[i, j] <- "D"
        m_A[i, j] <- age_at_death  # Apply the captured age for subsequent cycles
        next
      }
      
      health_state <- m_Stages[i, j]
      if (!is.na(health_state) && health_state == "D") {
        death_encountered <- TRUE
        age_at_death <- m_A[i, j]  # Capture the age at the point of death
      }
      
      age_group <- find_age_group(m_A[i, j])
      
      if (age_group %in% rownames(OC_Mortality)) {
        prob_death_other_causes <- OC_Mortality[age_group, paste("OC_Mortality", ifelse(df_X$cigsmok[i] == 1, "S", "ExS"), ifelse(df_X$Gender[i] == "male", "M", "F"), sep = "_")]
        if (runif(1) < prob_death_other_causes) {
          m_Stages[i, j] <- "D"
          death_encountered <- TRUE
          age_at_death <- m_A[i, j]  # Update the age at death here as well
          death_from_other_causes[i, j] <- TRUE
        }
      }
    }
  }
  
  return(list(
    m_Stages = m_Stages,
    death_from_other_causes = death_from_other_causes,
    m_A = m_A
  ))
}


# Making bounds and initial values matrices #

initial_guess     <- matrix(0, nrow = 4, ncol = 4, dimnames = list(row.names(N_Other_Matrices$N0), c("N1", "N2", "N3", "M1")))
upper_bounds      <- matrix(0, nrow = 4, ncol = 4, dimnames = list(row.names(N_Other_Matrices$N0), c("N1", "N2", "N3", "M1")))
lower_bounds      <- matrix(0, nrow = 4, ncol = 4, dimnames = list(row.names(N_Other_Matrices$N0), c("N1", "N2", "N3", "M1")))

# N1 bounds
upper_bounds [,1] <- c(0.03141361, 0.09009009, 0.16888889, 0.19879518)
lower_bounds [,1] <- c(0.001570681, 0.009009009, 0.016888889, 0.019879518)
# N2 bounds
upper_bounds [,2] <- c(0.99, 0.99, 0.99, 0.99)
lower_bounds [,2] <- c(0.8208955, 0.7222222, 0.7579618, 0.8465116)
# N3 bounds
upper_bounds [,3] <- c(0.2948718, 0.2844037, 0.2420382, 0.99)
lower_bounds [,3] <- c(0.02948718, 0.02844037, 0.02420382, 0.2661290)
# M1 bounds
upper_bounds [,4] <- c(0.001021739, 0.001764706, 0.02905759, 0.04855072)
lower_bounds [,4] <- c(0.0005108695, 0.000882353, 0.002905759, 0.004855072)
# Initial guesses are the middle pointa between upper and lower bounds
initial_guess     <- (upper_bounds  + lower_bounds )/2

# Flatten the matrices to create vectors for the optimization function
initial_vector <- as.vector(t(initial_guess))
lower_vector <- as.vector(t(lower_bounds))
upper_vector <- as.vector(t(upper_bounds))

# Define the model output function to get Final_Table
model_output <- function(params, M_Other_Matrices, N_Other_Matrices, m_Volume_Intervals, M_First_Matrix, N_First_Matrix, OC_Mortality, df_X, m_A, m_T) {
  # Unpack parameter values from the optimization vector
  # Here you should unpack params into the specific format used by your matrices
  
  params_matrix <- matrix(params, nrow = 4, byrow = TRUE, 
                          dimnames = list(row.names(N_Other_Matrices$N0), 
                                          c("N1", "N2", "N3", "M1")))
  
  # Update your model's matrices with these new parameters
  M_Other_Matrices$M0[,2] <- params_matrix[,4] 
  N_Other_Matrices$N0[,2] <- params_matrix[,1]
  N_Other_Matrices$N1[,3] <- params_matrix[,2]
  N_Other_Matrices$N2[,4] <- params_matrix[,3]
  
  # Adjust the remaining columns to account for the change in the initial values
  
  M_Other_Matrices$M0[,1] <- 1 - M_Other_Matrices$M0[,2]
  N_Other_Matrices$N0[,1] <- 1 - N_Other_Matrices$N0[,2]
  N_Other_Matrices$N1[,2] <- 1 - N_Other_Matrices$N1[,3]
  N_Other_Matrices$N2[,3] <- 1 - N_Other_Matrices$N2[,4]
  
  
  # Making m_M #
  
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
  
  # m_M was made #
  
  # Making m_N #
  
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
  
  # m_N was made #
  
  # Merging m_M, m_N and m_T to make m_Stages #
  
  # Create the state table
  state_table <- data.frame(
    T0 = c("T1A", "T1B", "T1C", "T2A", "T2B", "T3", "T4", "M1"),
    N0 = c("IA1", "IA2", "IA3", "IB", "IIA", "IIB","IIIA","IVA"),
    N1 = c("IIB","IIB","IIB","IIB","IIB","IIIA","IIIA","IVA"),
    N2 = c("IIIA","IIIA","IIIA","IIIA","IIIA" ,"IIIB" ,"IIIB" ,"IVA"),
    N3 = c("IIIB" ,"IIIB" ,"IIIB" ,"IIIB" , 'IIIB' ,'IIIC',' IIIC',' IVA')
  )
  
  # Get the log-transformed rates, Table 57, Clinical presentation rates with progression heterogeneity
  Snowsill_log_rates <- c(-2.583, -2.120, -1.876, -2.299, -1.302, -0.518, -0.084)
  
  # Convert log-transformed rates to original scale
  Snowsill_original_scale <- exp(Snowsill_log_rates)
  
  detection_probabilities <- c("IA1_u" = Snowsill_original_scale[1], "IA2_u" = Snowsill_original_scale[1], "IA3_u" = Snowsill_original_scale[1], "IB_u" = Snowsill_original_scale[2], "IIA_u" = Snowsill_original_scale[3], "IIB_u" = Snowsill_original_scale[4], "IIIA_u" = Snowsill_original_scale[5], "IIIB_u" = Snowsill_original_scale[6], "IIIC_u" = Snowsill_original_scale[6], "IVA_u" = Snowsill_original_scale[7])
  
  # Apply the function to each corresponding element in m_T, m_N, and m_M
  states_list <- apply_get_state(m_T, m_N, m_M, state_table, detection_probabilities)
  states <- states_list$states  # Extract the states matrix from the list returned by apply_get_state
  first_detection <- states_list$first_detection  # Extract the first_detection matrix from the list returned by apply_get_state
  
  
  # Save the states into a matrix called m_Stages
  m_Stages <- matrix(states, nrow = nrow(m_T), ncol = ncol(m_T))
  m_First_Detection <- matrix(first_detection, nrow = nrow(m_T), ncol = ncol(m_T))
  
  # Applying death #
  
  OC_Mortality$Age_Groups <- c("[0]", "[1-4]", "[5-9]", "[10-14]", "[15-19]", "[20-24]", "[25-29]", "[30-34]", "[35-39]", "[40-44]", "[45-49]", "[50-54]", "[55-59]", "[60-64]", "[65-69]", "[70-74]", "[75-79]", "[80-84]", "[85+]")
  rownames(OC_Mortality) <- OC_Mortality$Age_Groups
  
  
  # Death Function enters the chat
  
  Death_Output <- Prob_Death(m_Stages = m_Stages, 
                             df_X = df_X, 
                             OC_Mortality = OC_Mortality, 
                             m_A = m_A)
  
  m_Stages <- Death_Output$m_Stages
  m_A <- Death_Output$m_A
  
  # New Distributions output by size #
  
  # m_N first #
  
  # Initialize a list to store dataframes for each group
  m_NewCases_N_Groups <- list()
  
  # Volume size intervals
  volume_intervals <- 1:5
  
  for (group in volume_intervals) {
    # Initialize matrix for the current group
    m_NewCases <- matrix(0, nrow = 5, ncol = ncol(m_N))
    rownames(m_NewCases) <- c("N0", "N1", "N2", "N3", "Total")
    
    # Define a mapping from N values to row indices
    N_mapping <- list("N0" = 1, "N1" = 2, "N2" = 3, "N3" = 4)
    
    # Loop over each cycle
    for (cycle in 1:ncol(m_Stages)) {
      # Initialize the total count of newly detected cases for this cycle in the current group
      total_new_cases <- 0
      
      # Loop over each individual
      for (i in 1:nrow(m_Stages)) {
        # Use m_First_Detection to check if the current cycle is the first _d for the individual
        # AND if the individual belongs to the current group
        if (m_First_Detection[i, cycle] == TRUE && m_Volume_Intervals[i, cycle] == group) {
          # Get the corresponding N value
          N_value <- m_N[i, cycle]
          
          # Get the corresponding row index
          row_index <- N_mapping[[N_value]]
          
          # Increment the count in the new matrix for the current group
          m_NewCases[row_index, cycle] <- m_NewCases[row_index, cycle] + 1
          
          # Increment the total count of newly detected cases in the current group
          total_new_cases <- total_new_cases + 1
        }
      }
      
      # Store the total count of newly detected cases for this cycle in the current group
      m_NewCases["Total", cycle] <- total_new_cases
    }
    
    # Convert the matrix to a dataframe
    m_NewCases <- as.data.frame(m_NewCases)
    
    # Calculate Total_N and Proportions
    m_NewCases$Total_N <- rowSums(m_NewCases[, -ncol(m_NewCases)], na.rm = TRUE)
    m_NewCases$Proportions <- m_NewCases[, "Total_N"] / m_NewCases["Total", "Total_N"]
    
    # Store the dataframe for the current group in the list
    m_NewCases_N_Groups[[paste("Group", group)]] <- m_NewCases
  }
  
  
  
  # m_M now #
  
  # Initialize a list to store dataframes for each group
  m_NewCases_M_Groups <- list()
  
  # Volume size intervals
  volume_intervals <- 1:5
  
  
  for (group in volume_intervals) {
    # Initialize the new matrix for M for the current group
    m_NewCases_M <- matrix(0, nrow = 3, ncol = ncol(m_M))
    rownames(m_NewCases_M) <- c("M0", "M1", "Total")
    
    # Define a mapping from M values to row indices
    M_mapping <- list("M0" = 1, "M1" = 2)
    
    # Loop over each cycle
    for (cycle in 1:ncol(m_Stages)) {
      
      # Initialize the total count of newly detected cases for this cycle in the current group
      total_new_cases <- 0
      
      # Loop over each individual
      for (i in 1:nrow(m_Stages)) {
        # Use m_First_Detection to check if the current cycle is the first _d for the individual
        # AND if the individual belongs to the current group
        if (m_First_Detection[i, cycle] == TRUE && m_Volume_Intervals[i, cycle] == group) {
          # Get the corresponding M value
          M_value <- m_M[i, cycle]
          
          # Get the corresponding row index
          row_index <- M_mapping[[M_value]]
          
          # Increment the count in the new matrix for the current group
          m_NewCases_M[row_index, cycle] <- m_NewCases_M[row_index, cycle] + 1
          
          # Increment the total count of newly detected cases in the current group
          total_new_cases <- total_new_cases + 1
        }
      }
      
      # Store the total count of newly detected cases for this cycle in the current group
      m_NewCases_M["Total", cycle] <- total_new_cases
    }
    
    # Convert the matrix to a dataframe
    m_NewCases_M <- as.data.frame(m_NewCases_M)
    
    # Calculate the totals and proportions
    m_NewCases_M$Total_M <- rowSums(m_NewCases_M[ , -ncol(m_NewCases_M)], na.rm = TRUE)
    m_NewCases_M$Proportions <- m_NewCases_M[, "Total_M"] / m_NewCases_M[nrow(m_NewCases_M), "Total_M"]
    
    # Store the dataframe for the current group in the list
    m_NewCases_M_Groups[[paste("Group", group)]] <- m_NewCases_M
  }
  
  # Make Final_Table #
  
  # Assuming m_NewCases_N_Groups and m_NewCases_M_Groups are defined
  
  # Initialize the list to store the combined dataframes
  Calibration_outputs_by_size_intervals <- list()
  
  # Define the new column names
  
  new_column_names <- colnames(m_NewCases_N_Groups[[1]])
  
  new_row_names <- c("N0", "N1", "N2", "N3", "Total_N", "M0", "M1", "Total_M")
  
  # Assuming the groups are the same across both sets and are numerically indexed from 1 to 5
  volume_intervals <- 1:5
  
  for (group in volume_intervals) {
    # Retrieve the dataframes for the current group from both lists
    df_N_Group <- m_NewCases_N_Groups[[group]]  # Assuming these are correctly indexed or named
    df_M_Group <- m_NewCases_M_Groups[[group]]
    
    # Set the column names for both dataframes to the new column names
    # Ensure the dataframes have the correct number of columns
    colnames(df_N_Group) <- new_column_names
    colnames(df_M_Group) <- new_column_names
    
    # Combine the dataframes by stacking one on top of the other
    combined_df <- rbind(df_N_Group, df_M_Group)
    rownames(combined_df) <- new_row_names
    
    # Add the combined dataframe to the list
    Calibration_outputs_by_size_intervals[[paste("Group", group)]] <- combined_df
  }
  
  # Calibration_outputs_by_size_intervals now contains a combined dataframe for each group with uniform column names.
  
  
  # Extract row names from Group 1 dataframe to use as column names for Final_Table
  column_names_for_final_table <- rownames(Calibration_outputs_by_size_intervals[["Group 1"]])
  
  # Initialize Final_Table with appropriate dimensions
  group_names <- paste("Group", 1:length(Calibration_outputs_by_size_intervals))
  Final_Table <- matrix(nrow = length(Calibration_outputs_by_size_intervals), ncol = length(column_names_for_final_table), dimnames = list(group_names, column_names_for_final_table))
  
  # Populate Final_Table
  for(i in seq_along(Calibration_outputs_by_size_intervals)) {
    group_df <- Calibration_outputs_by_size_intervals[[i]]
    for(j in seq_along(column_names_for_final_table)) {
      # Extract the Proportions value, assign NA or 0 if not found
      proportion_value <- group_df$Proportions[rownames(group_df) == column_names_for_final_table[j]]
      if(length(proportion_value) == 0) {
        Final_Table[i, j] <- NA  # Assign NA (or replace NA with 0 if you prefer zeros)
      } else {
        Final_Table[i, j] <- proportion_value
      }
    }
  }
  
  # Convert the matrix to a dataframe
  #Final_Table <- as.data.frame(Final_Table)
  Final_Table <- Final_Table[, c("N0", "N1", "N2", "N3", "M0", "M1")];  Final_Table <- Final_Table[c("Group 1", "Group 2", "Group 3", "Group 4"),]# keep only volume and M/N values
  rownames(Final_Table) <- rownames(MN_Target)
  
  # Return the generated Final_Table
  return(Final_Table)
}
# Define the function to minimize
negative_log_likelihood <- function(Final_Table, MN_Target) {
  # Compute the squared differences
  squared_diffs <- (Final_Table - MN_Target)^2
  
  # Sum up the squared differences
  sum_squared_diffs <- sum(squared_diffs)
  
  # Assuming a normal distribution, the log-likelihood is proportional to the sum of squared differences
  # We negate it because optimization functions typically minimize the function value
  negative_ll <- sum_squared_diffs
  
  
  return(negative_ll)
}
# Makle the likelihood function dependent only from params
make_likelihood_function <- function(MN_Target, M_Other_Matrices, N_Other_Matrices, m_Volume_Intervals, M_First_Matrix, N_First_Matrix, OC_Mortality, df_X, m_A, m_T) {
  # This internal function will be the actual function used by the optimizer
  likelihood_function <- function(params) {
    # Here you can use all the variables from the outer function's scope
    Final_Table <- model_output(params, M_Other_Matrices, N_Other_Matrices, m_Volume_Intervals, M_First_Matrix, N_First_Matrix, OC_Mortality, df_X, m_A, m_T)
    
    neg_ll <- negative_log_likelihood(Final_Table, MN_Target)
    return(neg_ll)
  }
  return(likelihood_function)
}
# Create the likelihood function with all the necessary data
likelihood_function <- make_likelihood_function(MN_Target, M_Other_Matrices, N_Other_Matrices, m_Volume_Intervals, M_First_Matrix, N_First_Matrix, OC_Mortality, df_X, m_A, m_T)


# Run the optimization
result <- nloptr(
  x0 = initial_vector,
  eval_f = likelihood_function,
  lb = lower_vector,
  ub = upper_vector,
  opts = list(
    algorithm = "NLOPT_LN_COBYLA",  # common to find relative minimum
    xtol_rel = 1e-4,  # Less strict than 1e-6
    ftol_rel = 1e-4,  # Also less strict
    maxeval = 100     # Maximum number of evaluations
  )
)


# Interpret results
if(result$status == 0) { # Check if optimization was successful
  optimized_params <- matrix(result$solution, nrow = 4, ncol = 4, byrow = TRUE)
  print(optimized_params)
} else {
  message("Optimization did not converge to a solution.")
}



# extract solutions

param <- matrix(result$solution, nrow = 4, byrow = TRUE, 
                 dimnames = list(row.names(N_Other_Matrices$N0), 
                                 c("N1", "N2", "N3", "M1")))

