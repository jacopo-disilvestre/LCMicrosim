#### Setup ####

list_of_packages <- c("here", "flexsurv", "survminer", "tidyr", "muhaz", "dplyr", "haven", "ggsurvfit", "gtsummary", "bshazard", "openxlsx", "survival", "readxl", "ggplot2", "cowplot")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(list_of_packages, library, character.only = TRUE)

#### Reading and cleaning the data ####
Extrapolated_Times <- read_excel("/.../data_1.xlsx", range = "extrapolation!A14:B735")

KM_I <- read_excel("/.../data_1.xlsx", range = "extrapolation!V14:V75")
KM_II <- read_excel("/.../data_1.xlsx", range = "extrapolation!W14:W75")
KM_III <- read_excel("/.../data_1.xlsx", range = "extrapolation!X14:X75")
KM_IV <- read_excel("/.../data_1.xlsx", range = "extrapolation!Y14:Y75")

# Extend KM to 721 months with NA (correct length)
KM_I <- data.frame(KM = c(KM_I$KM, rep(NA, 721 - nrow(KM_I))))
KM_II <- data.frame(KM = c(KM_II$KM, rep(NA, 721 - nrow(KM_II))))
KM_III <- data.frame(KM = c(KM_III$KM, rep(NA, 721 - nrow(KM_III))))
KM_IV <- data.frame(KM = c(KM_IV$KM, rep(NA, 721 - nrow(KM_IV))))

# Read the survival curves
Extrapolated_Curves_I <- read_excel("/.../data_1.xlsx", range = "extrapolation!C14:F735")
Extrapolated_Curves_II <- read_excel("/.../data_1.xlsx", range = "extrapolation!G14:J735")
Extrapolated_Curves_III <- read_excel("/.../data_1.xlsx", range = "extrapolation!K14:N735")
Extrapolated_Curves_IV <- read_excel("/.../data_1.xlsx", range = "extrapolation!O14:R735")

# Combine the data frames
Extrapolated_Curves_I <- cbind(Extrapolated_Times, Extrapolated_Curves_I)
Extrapolated_Curves_II <- cbind(Extrapolated_Times, Extrapolated_Curves_II)
Extrapolated_Curves_III <- cbind(Extrapolated_Times, Extrapolated_Curves_III)
Extrapolated_Curves_IV <- cbind(Extrapolated_Times, Extrapolated_Curves_IV)

Extrapolated_Curves_I <- cbind(Extrapolated_Curves_I, KM_I)
Extrapolated_Curves_II <- cbind(Extrapolated_Curves_II, KM_II)
Extrapolated_Curves_III <- cbind(Extrapolated_Curves_III, KM_III)
Extrapolated_Curves_IV <- cbind(Extrapolated_Curves_IV, KM_IV)



rm(Extrapolated_Times)
rm(KM_I)
rm(KM_II)
rm(KM_III)
rm(KM_IV)


# Making a dataframe with stage specific survival curves
p_StagesD <- data.frame(Extrapolated_Curves_I$Months, Extrapolated_Curves_I$exponential, Extrapolated_Curves_II$Weibull, Extrapolated_Curves_III$lognormal, Extrapolated_Curves_IV$lognormal)
colnames(p_StagesD) <- c("Months", "Stage I - Exponential", "Stage II - Weibull", "Stage III - Lognormal", "Stage IV - Lognormal")

rm(Extrapolated_Curves_I)
rm(Extrapolated_Curves_II)
rm(Extrapolated_Curves_III)
rm(Extrapolated_Curves_IV)
rm(list_of_packages)
rm(new_packages)


# Calculating hazards, they are correct, decreasing over time after diagnosis and exponential is constant

# 'p_StagesD$Months' is your time variable
t <- p_StagesD$Months
# calculate hazard rates by stage
for (column in c("Stage I - Exponential", "Stage II - Weibull", "Stage III - Lognormal", "Stage IV - Lognormal")) {
  S <- p_StagesD[[column]]
  h <- -diff(log(S)) / diff(t)
  # Append NA to make the length of h same as S
  h <- c(h, NA)
  p_StagesD[[paste(column, ' - Hazard')]] <- h
}

rm(column, S, h, t)
colnames(p_StagesD) <- c("Months", "Stage I - Exponential", "Stage II - Weibull", "Stage III - Lognormal", "Stage IV - Lognormal", "I", "II", "III", "IV")

#### Implementing it in the model #### 
# This part was a first step to implement everything in the model

# Need to open m_Stages and let people die from there, used for tests
load("/.../m_Stages no dying.RData")
# Open OC_Mortality, obtained from the script "Workshop - OC My Mortality.R" from 2022
#load("/.../OC Mortality.RData")
# Open OC_Mortality, obtained from the script "Workshop - OC My Mortality.R" from 2023
load("/.../OC Mortality_2023.RData")

# Open m_A to check the age, used for tests
load("/.../m_A.RData")
# Open df_X for smoking status, used for tests
load("/.../df_X.RData")

#df_X$Status
# "ExS" or "S"

#or

# df_X$cigsmok
# 1 smoker 0 exsmoker

#rm(list=setdiff(ls(), "df_X"))

df_X$Gender <- sample(c("male", "female"), nrow(df_X), replace = TRUE)

  
# Initialize an empty matrix to store the risks and the new health states
risks <- matrix(NA, nrow = nrow(m_Stages), ncol = ncol(m_Stages))

# Define the mapping from stages to columns in p_StagesD
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



find_age_group <- function(age) {
  
  # Define the breaks in months corresponding to the start of each age range, plus an upper limit for the last range
  age_breaks <- c(0, 12, 60, 120, 180, 240, 300, 360, 420, 480, 540, 600, 660, 720, 780, 840, 900, 960, 1020, Inf)
  
  # Define the labels for each age range
  age_labels <- c("[0]", "[12-48]", "[60-108]", "[120-168]", "[180-228]", "[240-288]", "[300-348]",
                  "[360-408]", "[420-468]", "[480-528]", "[540-588]", "[600-648]", "[660-708]",
                  "[720-768]", "[780-828]", "[840-888]", "[900-948]", "[960-1008]", "[1020+]")
  
  # Find the index of the age break that contains the age
  age_group_index <- findInterval(age, age_breaks, rightmost.closed = TRUE)
  
  # Return the corresponding label
  if (age_group_index > length(age_labels)) {
    return(tail(age_labels, 1))  # In case age is beyond the specified ranges
  } else {
    return(age_labels[age_group_index])
  }
}


# Initialize a matrix to keep track of the time since diagnosis for each stage for each individual
time_since_diagnosis <- matrix(0, nrow = nrow(m_Stages), ncol = length(stage_mapping))
death_from_disease <- matrix(FALSE, nrow = nrow(m_Stages), ncol = ncol(m_Stages))
death_from_other_causes <- matrix(FALSE, nrow = nrow(m_Stages), ncol = ncol(m_Stages))

# Unhide print comments to check that the probability is drawn correctly, it is

# Iterate over the individuals
for (i in 1:nrow(m_Stages)) {
  # Flag to check if "D" state is encountered
  death_encountered <- FALSE
  
  # Extract smoking status and gender for the individual
  smoking_status <- ifelse(df_X$cigsmok[i] == 1, "S", "ExS")
  gender <- ifelse(df_X$Gender[i] == "male", "M", "F")
  
  # Determine the OC_Mortality column to use
  mortality_col <- paste("OC_Mortality", smoking_status, gender, sep = "_")
  
  
  # Print smoking status, gender, and the selected mortality column for the individual, check
  #print(paste("Individual", i, ":", smoking_status, gender, "Column:", mortality_col))
  
  
  
  # Iterate over the time points
  for (j in 1:ncol(m_Stages)) {
    # If "D" state is encountered, change all the following health states to "D" and skip mortality check
    if (death_encountered) {
      m_Stages[i, j] <- "D"
      next
    }
    
    # Check the health state
    health_state <- m_Stages[i, j]
    
    # If the individual is dead or health_state is NA, skip to the next iteration
    if (!is.na(health_state) && health_state == "D") {
      death_encountered <- TRUE
      next # might try to hide this, it was not there before
    }
    
    # Determine age group from m_A
    age <- m_A[i, j]
    age_group <- find_age_group(age)  # Assume find_age_group is a function that returns the correct age group string
    
    # Print the age group for the current cycle
    # print(paste("Cycle", j, ": Age Group:", age_group)), check
    
    
    # Draw the probability of dying from other causes
    if (age_group %in% rownames(OC_Mortality)) {
      prob_death_other_causes <- OC_Mortality[age_group, mortality_col]
      
      # Print the drawn probability, check
      #print(paste("Cycle", j, ": Drawn Probability of Death from Other Causes:", prob_death_other_causes))
      
      # Check for death due to other causes
      if (runif(1) < prob_death_other_causes) {
        m_Stages[i, j] <- "D"
        death_encountered <- TRUE
        death_from_other_causes[i, j] <- TRUE  # Marking death from other causes
      }
    }
    
    
    # If the health state ends with "_d" and is not NA
    if (!is.na(health_state) && grepl("_d$", health_state)) {
      # Increment the time since diagnosis for the current stage
      time_since_diagnosis[i, which(names(stage_mapping) == health_state)] <- time_since_diagnosis[i, which(names(stage_mapping) == health_state)] + 1
      
      # Check if the time_since_diagnosis and health_state are valid indices/column in p_StagesD
      if (time_since_diagnosis[i, which(names(stage_mapping) == health_state)] <= nrow(p_StagesD) && stage_mapping[[health_state]] %in% colnames(p_StagesD)) {
        # Draw the risk from the corresponding column in p_StagesD
        risks[i, j] <- p_StagesD[[stage_mapping[[health_state]]]][time_since_diagnosis[i, which(names(stage_mapping) == health_state)]]
        
        # If the risk of dying occurs (you can define this condition), change the health state to "D"
        if (runif(1) < risks[i, j]) {
          m_Stages[i, j] <- "D"
          death_encountered <- TRUE
          death_from_disease[i, j] <- TRUE  # Marking death due to disease
        }
      } 
    }
  }
}



rm(risks)
rm(stage_mapping)
rm(time_since_diagnosis)

