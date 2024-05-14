#### Saving CEA results in lists ####



# Open the results from Jacopo11.3 - Microsim - GitHub and save the important files in lists so that you do not have to open that heavy file everytime

# I could have made a function to do everything but eh, it works

#rm(outcomes_Scenario5, v_C_Scenario5, v_E_Scenario5, df_cea_Scenario5)



CEA_SoC <- list(
  m_Stages = outcomes_SoC$m_Stages,
  m_ScrStages = outcomes_SoC$m_ScrStages,
  Total_Costs_NHM = outcomes_SoC$Total_Costs_NHM, 
  Total_Costs_Scr = outcomes_SoC$Total_Costs_Scr, 
  m_E_NHM = outcomes_SoC$m_E_NHM,
  m_E_Scr = outcomes_SoC$m_E_Scr,
  tc_NHM = outcomes_SoC$tc_NHM,
  tc_Scr = outcomes_SoC$tc_Scr,
  te_NHM = outcomes_SoC$te_NHM,
  te_Scr = outcomes_SoC$te_Scr,
  tc_Scr_hat = outcomes_SoC$tc_Scr_hat,
  te_Scr_hat = outcomes_SoC$te_Scr_hat,
  tc_NHM_hat = outcomes_SoC$tc_NHM_hat,
  te_NHM_hat = outcomes_SoC$te_NHM_hat,
  m_Stages = outcomes_SoC$m_Stages,
  m_ScrStages = outcomes_SoC$m_ScrStages
)


CEA_Scenario1 <- list(
  m_Stages = outcomes_Scenario1$m_Stages,
  m_StagesScr = outcomes_Scenario1$m_ScrStages,
  Total_Costs_NHM = outcomes_Scenario1$Total_Costs_NHM, 
  Total_Costs_Scr = outcomes_Scenario1$Total_Costs_Scr,
  m_E_NHM = outcomes_Scenario1$m_E_NHM,
  m_E_Scr = outcomes_Scenario1$m_E_Scr,
  tc_NHM = outcomes_Scenario1$tc_NHM,
  tc_Scr = outcomes_Scenario1$tc_Scr,
  te_NHM = outcomes_Scenario1$te_NHM,
  te_Scr = outcomes_Scenario1$te_Scr,
  tc_Scr_hat = outcomes_Scenario1$tc_Scr_hat,
  te_Scr_hat = outcomes_Scenario1$te_Scr_hat,
  tc_NHM_hat = outcomes_Scenario1$tc_NHM_hat,
  te_NHM_hat = outcomes_Scenario1$te_NHM_hat,
  m_Stages = outcomes_Scenario1$m_Stages,
  m_ScrStages = outcomes_Scenario1$m_ScrStages
  
)


CEA_Scenario2 <- list(
  m_Stages = outcomes_Scenario2$m_Stages,
  m_StagesScr = outcomes_Scenario2$m_ScrStages,
  Total_Costs_NHM = outcomes_Scenario2$Total_Costs_NHM, 
  Total_Costs_Scr = outcomes_Scenario2$Total_Costs_Scr,
  m_E_NHM = outcomes_Scenario2$m_E_NHM,
  m_E_Scr = outcomes_Scenario2$m_E_Scr,
  tc_NHM = outcomes_Scenario2$tc_NHM,
  tc_Scr = outcomes_Scenario2$tc_Scr,
  te_NHM = outcomes_Scenario2$te_NHM,
  te_Scr = outcomes_Scenario2$te_Scr,
  tc_Scr_hat = outcomes_Scenario2$tc_Scr_hat,
  te_Scr_hat = outcomes_Scenario2$te_Scr_hat,
  tc_NHM_hat = outcomes_Scenario2$tc_NHM_hat,
  te_NHM_hat = outcomes_Scenario2$te_NHM_hat,
  m_Stages = outcomes_Scenario2$m_Stages,
  m_ScrStages = outcomes_Scenario2$m_ScrStages
)


CEA_Scenario3 <- list(
  m_Stages = outcomes_Scenario3$m_Stages,
  m_StagesScr = outcomes_Scenario3$m_ScrStages,
  Total_Costs_NHM = outcomes_Scenario3$Total_Costs_NHM, 
  Total_Costs_Scr = outcomes_Scenario3$Total_Costs_Scr,
  m_E_NHM = outcomes_Scenario3$m_E_NHM,
  m_E_Scr = outcomes_Scenario3$m_E_Scr,
  tc_NHM = outcomes_Scenario3$tc_NHM,
  tc_Scr = outcomes_Scenario3$tc_Scr,
  te_NHM = outcomes_Scenario3$te_NHM,
  te_Scr = outcomes_Scenario3$te_Scr,
  tc_Scr_hat = outcomes_Scenario3$tc_Scr_hat,
  te_Scr_hat = outcomes_Scenario3$te_Scr_hat,
  tc_NHM_hat = outcomes_Scenario3$tc_NHM_hat,
  te_NHM_hat = outcomes_Scenario3$te_NHM_hat,
  m_Stages = outcomes_Scenario3$m_Stages,
  m_ScrStages = outcomes_Scenario3$m_ScrStages
)


CEA_Scenario4 <- list(
  m_Stages = outcomes_Scenario4$m_Stages,
  m_StagesScr = outcomes_Scenario4$m_ScrStages,
  Total_Costs_NHM = outcomes_Scenario4$Total_Costs_NHM, 
  Total_Costs_Scr = outcomes_Scenario4$Total_Costs_Scr,
  m_E_NHM = outcomes_Scenario4$m_E_NHM,
  m_E_Scr = outcomes_Scenario4$m_E_Scr,
  tc_NHM = outcomes_Scenario4$tc_NHM,
  tc_Scr = outcomes_Scenario4$tc_Scr,
  te_NHM = outcomes_Scenario4$te_NHM,
  te_Scr = outcomes_Scenario4$te_Scr,
  tc_Scr_hat = outcomes_Scenario4$tc_Scr_hat,
  te_Scr_hat = outcomes_Scenario4$te_Scr_hat,
  tc_NHM_hat = outcomes_Scenario4$tc_NHM_hat,
  te_NHM_hat = outcomes_Scenario4$te_NHM_hat,
  m_Stages = outcomes_Scenario4$m_Stages,
  m_ScrStages = outcomes_Scenario4$m_ScrStages
)


CEA_Scenario5 <- list(
  m_Stages = outcomes_Scenario5$m_Stages,
  m_StagesScr = outcomes_Scenario5$m_ScrStages,
  Total_Costs_NHM = outcomes_Scenario5$Total_Costs_NHM, 
  Total_Costs_Scr = outcomes_Scenario5$Total_Costs_Scr,
  m_E_NHM = outcomes_Scenario5$m_E_NHM,
  m_E_Scr = outcomes_Scenario5$m_E_Scr,
  tc_NHM = outcomes_Scenario5$tc_NHM,
  tc_Scr = outcomes_Scenario5$tc_Scr,
  te_NHM = outcomes_Scenario5$te_NHM,
  te_Scr = outcomes_Scenario5$te_Scr,
  tc_Scr_hat = outcomes_Scenario5$tc_Scr_hat,
  te_Scr_hat = outcomes_Scenario5$te_Scr_hat,
  tc_NHM_hat = outcomes_Scenario5$tc_NHM_hat,
  te_NHM_hat = outcomes_Scenario5$te_NHM_hat,
  m_Stages = outcomes_Scenario5$m_Stages,
  m_ScrStages = outcomes_Scenario5$m_ScrStages
)

CEA_Sensitivity1 <- list(
  m_Stages = outcomes_Sensitivity1$m_Stages,
  m_StagesScr = outcomes_Sensitivity1$m_ScrStages,
  Total_Costs_NHM = outcomes_Sensitivity1$Total_Costs_NHM, 
  Total_Costs_Scr = outcomes_Sensitivity1$Total_Costs_Scr,
  m_E_NHM = outcomes_Sensitivity1$m_E_NHM,
  m_E_Scr = outcomes_Sensitivity1$m_E_Scr,
  tc_NHM = outcomes_Sensitivity1$tc_NHM,
  tc_Scr = outcomes_Sensitivity1$tc_Scr,
  te_NHM = outcomes_Sensitivity1$te_NHM,
  te_Scr = outcomes_Sensitivity1$te_Scr,
  tc_Scr_hat = outcomes_Sensitivity1$tc_Scr_hat,
  te_Scr_hat = outcomes_Sensitivity1$te_Scr_hat,
  tc_NHM_hat = outcomes_Sensitivity1$tc_NHM_hat,
  te_NHM_hat = outcomes_Sensitivity1$te_NHM_hat,
  m_Stages = outcomes_Sensitivity1$m_Stages,
  m_ScrStages = outcomes_Sensitivity1$m_ScrStages
)

CEA_Sensitivity2 <- list(
  m_Stages = outcomes_Sensitivity2$m_Stages,
  m_StagesScr = outcomes_Sensitivity2$m_ScrStages,
  Total_Costs_NHM = outcomes_Sensitivity2$Total_Costs_NHM, 
  Total_Costs_Scr = outcomes_Sensitivity2$Total_Costs_Scr,
  m_E_NHM = outcomes_Sensitivity2$m_E_NHM,
  m_E_Scr = outcomes_Sensitivity2$m_E_Scr,
  tc_NHM = outcomes_Sensitivity2$tc_NHM,
  tc_Scr = outcomes_Sensitivity2$tc_Scr,
  te_NHM = outcomes_Sensitivity2$te_NHM,
  te_Scr = outcomes_Sensitivity2$te_Scr,
  tc_Scr_hat = outcomes_Sensitivity2$tc_Scr_hat,
  te_Scr_hat = outcomes_Sensitivity2$te_Scr_hat,
  tc_NHM_hat = outcomes_Sensitivity2$tc_NHM_hat,
  te_NHM_hat = outcomes_Sensitivity2$te_NHM_hat,
  m_Stages = outcomes_Sensitivity2$m_Stages,
  m_ScrStages = outcomes_Sensitivity2$m_ScrStages
)

CEA_Sensitivity3 <- list(
  m_Stages = outcomes_Sensitivity3$m_Stages,
  m_StagesScr = outcomes_Sensitivity3$m_ScrStages,
  Total_Costs_NHM = outcomes_Sensitivity3$Total_Costs_NHM, 
  Total_Costs_Scr = outcomes_Sensitivity3$Total_Costs_Scr,
  m_E_NHM = outcomes_Sensitivity3$m_E_NHM,
  m_E_Scr = outcomes_Sensitivity3$m_E_Scr,
  tc_NHM = outcomes_Sensitivity3$tc_NHM,
  tc_Scr = outcomes_Sensitivity3$tc_Scr,
  te_NHM = outcomes_Sensitivity3$te_NHM,
  te_Scr = outcomes_Sensitivity3$te_Scr,
  tc_Scr_hat = outcomes_Sensitivity3$tc_Scr_hat,
  te_Scr_hat = outcomes_Sensitivity3$te_Scr_hat,
  tc_NHM_hat = outcomes_Sensitivity3$tc_NHM_hat,
  te_NHM_hat = outcomes_Sensitivity3$te_NHM_hat,
  m_Stages = outcomes_Sensitivity3$m_Stages,
  m_ScrStages = outcomes_Sensitivity3$m_ScrStages
)

CEA_Sensitivity4 <- list(
  m_Stages = outcomes_Sensitivity4$m_Stages,
  m_StagesScr = outcomes_Sensitivity4$m_ScrStages,
  Total_Costs_NHM = outcomes_Sensitivity4$Total_Costs_NHM, 
  Total_Costs_Scr = outcomes_Sensitivity4$Total_Costs_Scr,
  m_E_NHM = outcomes_Sensitivity4$m_E_NHM,
  m_E_Scr = outcomes_Sensitivity4$m_E_Scr,
  tc_NHM = outcomes_Sensitivity4$tc_NHM,
  tc_Scr = outcomes_Sensitivity4$tc_Scr,
  te_NHM = outcomes_Sensitivity4$te_NHM,
  te_Scr = outcomes_Sensitivity4$te_Scr,
  tc_Scr_hat = outcomes_Sensitivity4$tc_Scr_hat,
  te_Scr_hat = outcomes_Sensitivity4$te_Scr_hat,
  tc_NHM_hat = outcomes_Sensitivity4$tc_NHM_hat,
  te_NHM_hat = outcomes_Sensitivity4$te_NHM_hat,
  m_Stages = outcomes_Sensitivity4$m_Stages,
  m_ScrStages = outcomes_Sensitivity4$m_ScrStages
)





#outcomes_SoC <- CEA_SoC
#outcomes_Scenario1 <- CEA_Scenario1
#outcomes_Scenario2 <- CEA_Scenario2
#outcomes_Scenario3 <- CEA_Scenario3
#outcomes_Scenario4 <- CEA_Scenario4
#outcomes_Scenario5 <- CEA_Scenario5
#rm(CEA_SoC, CEA_Scenario1, CEA_Scenario2, CEA_Scenario3, CEA_Scenario4, CEA_Scenario5, CEA_Sensitivity1, CEA_Sensitivity2, CEA_Sensitivity3, CEA_Sensitivity4)

#outcomes_Sensitivity1 <- CEA_Sensitivity1
#outcomes_Sensitivity2 <- CEA_Sensitivity2
#outcomes_Sensitivity3 <- CEA_Sensitivity3
#outcomes_Sensitivity4 <- CEA_Sensitivity4
#rm(CEA_Sensitivity1, CEA_Sensitivity2, CEA_Sensitivity3, CEA_Sensitivity4)

#rm(list=setdiff(ls(), c("CEA_SoC", "CEA_Scenario1", "CEA_Scenario2", "CEA_Scenario3", "CEA_Scenario4", "CEA_Scenario5")))

#rm(list=setdiff(ls(), c("CEA_Sensitivity1", "CEA_Sensitivity2", "CEA_Sensitivity3", "CEA_Sensitivity4")))


# Cost-effectiveness analysis 



#### QALY ICER discounted ####

str_names_baseline   <- c("NHM", "Screening - Baseline")
str_names_scenario1  <- c("NHM", "Screening - Scenario 1") # change the screening intervals from 2 to 3 years
str_names_scenario2  <- c("NHM", "Screening - Scenario 2") # pkyrs from 35 to 40
str_names_scenario3  <- c("NHM", "Screening - Scenario 3") # pkyrs from 35 to 50
str_names_scenario4  <- c("NHM", "Screening - Scenario 4") # age from 79 to 70 tops (948 to 840)
str_names_scenario5  <- c("NHM", "Screening - Scenario 5") # screening interval from 24 to 12 months
str_names_sensitivity1 <- c("NHM", "Screening - Sensitivity 1") # sensitivity of 0.5
str_names_sensitivity2 <- c("NHM", "Screening - Sensitivity 2") # sensitivity of 0.75
str_names_sensitivity3 <- c("NHM", "Screening - Sensitivity 3") # sensitivity of 1.25
str_names_sensitivity4 <- c("NHM", "Screening - Sensitivity 4") # sensitivity of 1.5


dataceas <- c("outcomes_SoC", "outcomes_Scenario1", "outcomes_Scenario2", "outcomes_Scenario3", "outcomes_Scenario4", "outcomes_Scenario5", "outcomes_Sensitivity1", "outcomes_Sensitivity2", "outcomes_Sensitivity3", "outcomes_Sensitivity4")


### Discounting factors 
d_c <- 0.04 # annual discount rate for costs 
d_e <- 0.04 # annual discount rate for QALYs

n_cycles       <- 720
cycle_length   <- if(!exists("cycle_length")) { cycle_length <- 1/12 } else { cycle_length }     

### Discount weight for costs and effects 
v_dwc   <- 1 / ((1 + (d_e * cycle_length)) ^ (0:n_cycles)) # discount weight for costs
v_dwe   <- 1 / ((1 + (d_c * cycle_length)) ^ (0:n_cycles)) # discount weight for effects


#gen_wcc functions is from DARTH

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

n_cycles       <- 720
v_wcc    <- gen_wcc(n_cycles = n_cycles, method = "Simpson1/3") # vector of wcc

# needed just to fix a name mistake when saving CEAS into lists
outcomes_SoC$m_StagesScr <- outcomes_SoC$m_ScrStages

for (i in 1:length(dataceas)){
  data <- get(dataceas[i])
  
  
  data$m_Stages_LY <- ifelse(data$m_Stages == "D", 0, 1)
  data$m_ScrStages_LY <- ifelse(data$m_StagesScr == "D", 0, 1)
  
  
  
  # Assign the modified dataset back to the global environment
  assign(dataceas[i], data)
}


for (i in 1:length(dataceas)){
  data <- get(dataceas[i])
  
  #QALY
  # Undiscounted
  
  
  data$tc_NHM_und      <- data$Total_Costs_NHM %*% ( v_wcc)  # total (discounted and cycle corrected) cost per individual
  data$tc_Scr_und      <- data$Total_Costs_Scr %*% ( v_wcc)  # total (discounted and cycle corrected) cost per individual
  
  data$te_NHM_und      <- data$m_E_NHM %*% ( v_wcc)  # total (discounted and cycle corrected) QALYs per individual 
  data$te_Scr_und      <- data$m_E_Scr %*% ( v_wcc)  # total (discounted and cycle corrected) QALYs per individual
  
  data$tc_NHM_hat_und <- mean(data$tc_NHM_und)  # average (discounted and cycle corrected) cost
  data$te_NHM_hat_und <- mean(data$te_NHM_und)  # average (discounted and cycle corrected) QALY
  
  data$tc_Scr_hat_und <- mean(data$tc_Scr_und)  # average (discounted and cycle corrected) cost
  data$te_Scr_hat_und <- mean(data$te_Scr_und)  # average (discounted and cycle corrected) QALY
  
  
  
  
  #LY
  # Undiscouted
  data$te_NHM_LY_und      <- data$m_Stages_LY %*% ( v_wcc)  # total (discounted and cycle corrected) QALYs per individual 
  data$te_Scr_LY_und      <- data$m_ScrStages_LY %*% ( v_wcc)  # total (discounted and cycle corrected) QALYs per individual
  data$te_NHM_hat_LY_und <- mean(data$te_NHM_LY_und)  # average (discounted and cycle corrected) QALY
  data$te_Scr_hat_LY_und <- mean(data$te_Scr_LY_und)  # average (discounted and cycle corrected) QALY
  
  #Discounted
  data$te_NHM_LY      <- data$m_Stages_LY %*% ( v_dwe * v_wcc)  # total (discounted and cycle corrected) QALYs per individual 
  data$te_Scr_LY      <- data$m_ScrStages_LY %*% ( v_dwe * v_wcc)  # total (discounted and cycle corrected) QALYs per individual
  data$te_NHM_hat_LY <- mean(data$te_NHM_LY)  # average (discounted and cycle corrected) QALY
  data$te_Scr_hat_LY <- mean(data$te_Scr_LY)  # average (discounted and cycle corrected) QALY
  
  
  # Assign the modified dataset back to the global environment
  assign(dataceas[i], data)
}


# store the mean costs and QALYs of each scenario 
# Base case scenario
v_C_basecase <- c(outcomes_SoC$tc_NHM_hat, outcomes_SoC$tc_Scr_hat)
v_E_basecase <- c(outcomes_SoC$te_NHM_hat, outcomes_SoC$te_Scr_hat)

# Scenario 1
v_C_Scenario1 <- c(outcomes_Scenario1$tc_NHM_hat, outcomes_Scenario1$tc_Scr_hat)
v_E_Scenario1 <- c(outcomes_Scenario1$te_NHM_hat, outcomes_Scenario1$te_Scr_hat)

# Scenario 2
v_C_Scenario2 <- c(outcomes_Scenario2$tc_NHM_hat, outcomes_Scenario2$tc_Scr_hat)
v_E_Scenario2 <- c(outcomes_Scenario2$te_NHM_hat, outcomes_Scenario2$te_Scr_hat)

# Scenario 3
v_C_Scenario3 <- c(outcomes_Scenario3$tc_NHM_hat, outcomes_Scenario3$tc_Scr_hat)
v_E_Scenario3 <- c(outcomes_Scenario3$te_NHM_hat, outcomes_Scenario3$te_Scr_hat)

# Scenario 4
v_C_Scenario4 <- c(outcomes_Scenario4$tc_NHM_hat, outcomes_Scenario4$tc_Scr_hat)
v_E_Scenario4 <- c(outcomes_Scenario4$te_NHM_hat, outcomes_Scenario4$te_Scr_hat)

# Scenario 5
v_C_Scenario5 <- c(outcomes_Scenario5$tc_NHM_hat, outcomes_Scenario5$tc_Scr_hat)
v_E_Scenario5 <- c(outcomes_Scenario5$te_NHM_hat, outcomes_Scenario5$te_Scr_hat)



# Sensitivity 1
v_C_Sensitivity1 <- c(outcomes_Sensitivity1$tc_NHM_hat, outcomes_Sensitivity1$tc_Scr_hat)
v_E_Sensitivity1 <- c(outcomes_Sensitivity1$te_NHM_hat, outcomes_Sensitivity1$te_Scr_hat)

# Sensitivity 2
v_C_Sensitivity2 <- c(outcomes_Sensitivity2$tc_NHM_hat, outcomes_Sensitivity2$tc_Scr_hat)
v_E_Sensitivity2 <- c(outcomes_Sensitivity2$te_NHM_hat, outcomes_Sensitivity2$te_Scr_hat)

# Sensitivity 3
v_C_Sensitivity3 <- c(outcomes_Sensitivity3$tc_NHM_hat, outcomes_Sensitivity3$tc_Scr_hat)
v_E_Sensitivity3 <- c(outcomes_Sensitivity3$te_NHM_hat, outcomes_Sensitivity3$te_Scr_hat)

# Sensitivity 4
v_C_Sensitivity4 <- c(outcomes_Sensitivity4$tc_NHM_hat, outcomes_Sensitivity4$tc_Scr_hat)
v_E_Sensitivity4 <- c(outcomes_Sensitivity4$te_NHM_hat, outcomes_Sensitivity4$te_Scr_hat)



# use dampack to calculate the ICER
# get independence from dampack!!
df_cea_basecase  <- calculate_icers(cost       = v_C_basecase,
                                    effect     = v_E_basecase,
                                    strategies = str_names_baseline)

df_cea_Scenario1 <- calculate_icers(cost       = v_C_Scenario1,
                                    effect     = v_E_Scenario1,
                                    strategies = str_names_scenario1)

df_cea_Scenario2 <- calculate_icers(cost       = v_C_Scenario2,
                                    effect     = v_E_Scenario2,
                                    strategies = str_names_scenario2)

df_cea_Scenario3 <- calculate_icers(cost       = v_C_Scenario3,
                                    effect     = v_E_Scenario3,
                                    strategies = str_names_scenario3)

df_cea_Scenario4 <- calculate_icers(cost       = v_C_Scenario4,
                                    effect     = v_E_Scenario4,
                                    strategies = str_names_scenario4)

df_cea_Scenario5 <- calculate_icers(cost       = v_C_Scenario5,
                                    effect     = v_E_Scenario5,
                                    strategies = str_names_scenario5)



df_cea_Sensitivity1 <- calculate_icers(cost       = v_C_Sensitivity1,
                                       effect     = v_E_Sensitivity1,
                                       strategies = str_names_sensitivity1)


df_cea_Sensitivity2 <- calculate_icers(cost       = v_C_Sensitivity2,
                                       effect     = v_E_Sensitivity2,
                                       strategies = str_names_sensitivity2)


df_cea_Sensitivity3 <- calculate_icers(cost       = v_C_Sensitivity3,
                                       effect     = v_E_Sensitivity3,
                                       strategies = str_names_sensitivity3)

df_cea_Sensitivity4 <- calculate_icers(cost       = v_C_Sensitivity4,
                                       effect     = v_E_Sensitivity4,
                                       strategies = str_names_sensitivity4)



df_cea_Scenarios <- calculate_icers(cost       = c(v_C_basecase[1], v_C_basecase[2], v_C_Scenario1[2], v_C_Scenario2[2], v_C_Scenario3[2], v_C_Scenario4[2], v_C_Scenario5[2]),
                                    effect     = c(v_E_basecase[1], v_E_basecase[2], v_E_Scenario1[2], v_E_Scenario2[2], v_E_Scenario3[2], v_E_Scenario4[2], v_E_Scenario5[2]),
                                    strategies = c("No Screening", "Screening - Baseline", "Screening - Scenario 1", "Screening - Scenario 2", "Screening - Scenario 3", "Screening - Scenario 4", "Screening - Scenario 5"))


df_cea_Sensitivities <- df_cea_Sensitivity1
df_cea_Sensitivities <- rbind(df_cea_Sensitivities, df_cea_Sensitivity2[2,])
df_cea_Sensitivities <- rbind(df_cea_Sensitivities, df_cea_Sensitivity3[2,])
df_cea_Sensitivities <- rbind(df_cea_Sensitivities, df_cea_Sensitivity4[2,])



Data_Sensitivities <- df_cea_Sensitivities
Data_Primary <- df_cea_basecase
Data_Scenarios <- df_cea_Scenarios



#### QALY ICER undiscounted ####


# QALY undiscounted, change outcomes_Sensitivity4 with every scenario ecc by hand and run for each

# Discounted total expected QALYs and Costs per strategy and apply cycle correction





#

# Cost-effectiveness analysis 

# store the mean costs and QALYs of each scenario 
# Base case scenario
v_C_basecase_und <- c(outcomes_SoC$tc_NHM_hat_und, outcomes_SoC$tc_Scr_hat_und)
v_E_basecase_und <- c(outcomes_SoC$te_NHM_hat_und, outcomes_SoC$te_Scr_hat_und)

# Scenario 1
v_C_Scenario1_und <- c(outcomes_Scenario1$tc_NHM_hat_und, outcomes_Scenario1$tc_Scr_hat_und)
v_E_Scenario1_und <- c(outcomes_Scenario1$te_NHM_hat_und, outcomes_Scenario1$te_Scr_hat_und)

# Scenario 2
v_C_Scenario2_und <- c(outcomes_Scenario2$tc_NHM_hat_und, outcomes_Scenario2$tc_Scr_hat_und)
v_E_Scenario2_und <- c(outcomes_Scenario2$te_NHM_hat_und, outcomes_Scenario2$te_Scr_hat_und)

# Scenario 3
v_C_Scenario3_und <- c(outcomes_Scenario3$tc_NHM_hat_und, outcomes_Scenario3$tc_Scr_hat_und)
v_E_Scenario3_und <- c(outcomes_Scenario3$te_NHM_hat_und, outcomes_Scenario3$te_Scr_hat_und)

# Scenario 4
v_C_Scenario4_und <- c(outcomes_Scenario4$tc_NHM_hat_und, outcomes_Scenario4$tc_Scr_hat_und)
v_E_Scenario4_und <- c(outcomes_Scenario4$te_NHM_hat_und, outcomes_Scenario4$te_Scr_hat_und)

# Scenario 5
v_C_Scenario5_und <- c(outcomes_Scenario5$tc_NHM_hat_und, outcomes_Scenario5$tc_Scr_hat_und)
v_E_Scenario5_und <- c(outcomes_Scenario5$te_NHM_hat_und, outcomes_Scenario5$te_Scr_hat_und)


#rm(outcomes_Scenario5, v_C_Scenario5_und, v_E_Scenario5_und, df_cea_Scenario5_und)

# Sensitivity 1
v_C_Sensitivity1_und <- c(outcomes_Sensitivity1$tc_NHM_hat_und, outcomes_Sensitivity1$tc_Scr_hat_und)
v_E_Sensitivity1_und <- c(outcomes_Sensitivity1$te_NHM_hat_und, outcomes_Sensitivity1$te_Scr_hat_und)

# Sensitivity 2
v_C_Sensitivity2_und <- c(outcomes_Sensitivity2$tc_NHM_hat_und, outcomes_Sensitivity2$tc_Scr_hat_und)
v_E_Sensitivity2_und <- c(outcomes_Sensitivity2$te_NHM_hat_und, outcomes_Sensitivity2$te_Scr_hat_und)

# Sensitivity 3
v_C_Sensitivity3_und <- c(outcomes_Sensitivity3$tc_NHM_hat_und, outcomes_Sensitivity3$tc_Scr_hat_und)
v_E_Sensitivity3_und <- c(outcomes_Sensitivity3$te_NHM_hat_und, outcomes_Sensitivity3$te_Scr_hat_und)

# Sensitivity 4
v_C_Sensitivity4_und <- c(outcomes_Sensitivity4$tc_NHM_hat_und, outcomes_Sensitivity4$tc_Scr_hat_und)
v_E_Sensitivity4_und <- c(outcomes_Sensitivity4$te_NHM_hat_und, outcomes_Sensitivity4$te_Scr_hat_und)


df_cea_basecase_und  <- calculate_icers(cost       = v_C_basecase_und,
                                        effect     = v_E_basecase_und,
                                        strategies = str_names_baseline)

df_cea_Scenario1_und <- calculate_icers(cost       = v_C_Scenario1_und,
                                        effect     = v_E_Scenario1_und,
                                        strategies = str_names_scenario1)

df_cea_Scenario2_und <- calculate_icers(cost       = v_C_Scenario2_und,
                                        effect     = v_E_Scenario2_und,
                                        strategies = str_names_scenario2)

df_cea_Scenario3_und <- calculate_icers(cost       = v_C_Scenario3_und,
                                        effect     = v_E_Scenario3_und,
                                        strategies = str_names_scenario3)

df_cea_Scenario4_und <- calculate_icers(cost       = v_C_Scenario4_und,
                                        effect     = v_E_Scenario4_und,
                                        strategies = str_names_scenario4)

df_cea_Scenario5_und <- calculate_icers(cost       = v_C_Scenario5_und,
                                        effect     = v_E_Scenario5_und,
                                        strategies = str_names_scenario5)

df_cea_Sensitivity1_und <- calculate_icers(cost       = v_C_Sensitivity1_und,
                                           effect     = v_E_Sensitivity1_und,
                                           strategies = str_names_sensitivity1)


df_cea_Sensitivity2_und <- calculate_icers(cost       = v_C_Sensitivity2_und,
                                           effect     = v_E_Sensitivity2_und,
                                           strategies = str_names_sensitivity2)


df_cea_Sensitivity3_und <- calculate_icers(cost       = v_C_Sensitivity3_und,
                                           effect     = v_E_Sensitivity3_und,
                                           strategies = str_names_sensitivity3)

df_cea_Sensitivity4_und <- calculate_icers(cost       = v_C_Sensitivity4_und,
                                           effect     = v_E_Sensitivity4_und,
                                           strategies = str_names_sensitivity4)






df_cea_Scenarios_und <- calculate_icers(cost       = c(v_C_basecase_und[1], v_C_basecase_und[2], v_C_Scenario1_und[2], v_C_Scenario2_und[2], v_C_Scenario3_und[2], v_C_Scenario4_und[2], v_C_Scenario5_und[2]),
                                        effect     = c(v_E_basecase_und[1], v_E_basecase_und[2], v_E_Scenario1_und[2], v_E_Scenario2_und[2], v_E_Scenario3_und[2], v_E_Scenario4_und[2], v_E_Scenario5_und[2]),
                                        strategies = c("No Screening_und", "Screening - Baseline_und", "Screening - Scenario 1_und", "Screening - Scenario 2_und", "Screening - Scenario 3_und", "Screening - Scenario 4_und", "Screening - Scenario 5_und"))


df_cea_Sensitivities_und <- df_cea_Sensitivity1_und
df_cea_Sensitivities_und <- rbind(df_cea_Sensitivities_und, df_cea_Sensitivity2_und[2,])
df_cea_Sensitivities_und <- rbind(df_cea_Sensitivities_und, df_cea_Sensitivity3_und[2,])
df_cea_Sensitivities_und <- rbind(df_cea_Sensitivities_und, df_cea_Sensitivity4_und[2,])



Data_Sensitivities_und <- df_cea_Sensitivities_und
Data_Primary_und <- df_cea_basecase_und
Data_Scenarios_und <- df_cea_Scenarios_und


#### LY ICER discounted ####







v_C_basecase <- c(outcomes_SoC$tc_NHM_hat, outcomes_SoC$tc_Scr_hat)
v_E_basecase <- c(outcomes_SoC$te_NHM_hat_LY, outcomes_SoC$te_Scr_hat_LY)

# Scenario 1
v_C_Scenario1 <- c(outcomes_Scenario1$tc_NHM_hat, outcomes_Scenario1$tc_Scr_hat)
v_E_Scenario1 <- c(outcomes_Scenario1$te_NHM_hat_LY, outcomes_Scenario1$te_Scr_hat_LY)

# Scenario 2
v_C_Scenario2 <- c(outcomes_Scenario2$tc_NHM_hat, outcomes_Scenario2$tc_Scr_hat)
v_E_Scenario2 <- c(outcomes_Scenario2$te_NHM_hat_LY, outcomes_Scenario2$te_Scr_hat_LY)

# Scenario 3
v_C_Scenario3 <- c(outcomes_Scenario3$tc_NHM_hat, outcomes_Scenario3$tc_Scr_hat)
v_E_Scenario3 <- c(outcomes_Scenario3$te_NHM_hat_LY, outcomes_Scenario3$te_Scr_hat_LY)

# Scenario 4
v_C_Scenario4 <- c(outcomes_Scenario4$tc_NHM_hat, outcomes_Scenario4$tc_Scr_hat)
v_E_Scenario4 <- c(outcomes_Scenario4$te_NHM_hat_LY, outcomes_Scenario4$te_Scr_hat_LY)

# Scenario 5
v_C_Scenario5 <- c(outcomes_Scenario5$tc_NHM_hat, outcomes_Scenario5$tc_Scr_hat)
v_E_Scenario5 <- c(outcomes_Scenario5$te_NHM_hat_LY, outcomes_Scenario5$te_Scr_hat_LY)



# Sensitivity 1
v_C_Sensitivity1 <- c(outcomes_Sensitivity1$tc_NHM_hat, outcomes_Sensitivity1$tc_Scr_hat)
v_E_Sensitivity1 <- c(outcomes_Sensitivity1$te_NHM_hat_LY, outcomes_Sensitivity1$te_Scr_hat_LY)

# Sensitivity 2
v_C_Sensitivity2 <- c(outcomes_Sensitivity2$tc_NHM_hat, outcomes_Sensitivity2$tc_Scr_hat)
v_E_Sensitivity2 <- c(outcomes_Sensitivity2$te_NHM_hat_LY, outcomes_Sensitivity2$te_Scr_hat_LY)

# Sensitivity 3
v_C_Sensitivity3 <- c(outcomes_Sensitivity3$tc_NHM_hat, outcomes_Sensitivity3$tc_Scr_hat)
v_E_Sensitivity3 <- c(outcomes_Sensitivity3$te_NHM_hat_LY, outcomes_Sensitivity3$te_Scr_hat_LY)

# Sensitivity 4
v_C_Sensitivity4 <- c(outcomes_Sensitivity4$tc_NHM_hat, outcomes_Sensitivity4$tc_Scr_hat)
v_E_Sensitivity4 <- c(outcomes_Sensitivity4$te_NHM_hat_LY, outcomes_Sensitivity4$te_Scr_hat_LY)












df_cea_basecase  <- calculate_icers(cost       = v_C_basecase,
                                    effect     = v_E_basecase,
                                    strategies = str_names_baseline)

df_cea_Scenario1 <- calculate_icers(cost       = v_C_Scenario1,
                                    effect     = v_E_Scenario1,
                                    strategies = str_names_scenario1)

df_cea_Scenario2 <- calculate_icers(cost       = v_C_Scenario2,
                                    effect     = v_E_Scenario2,
                                    strategies = str_names_scenario2)

df_cea_Scenario3 <- calculate_icers(cost       = v_C_Scenario3,
                                    effect     = v_E_Scenario3,
                                    strategies = str_names_scenario3)

df_cea_Scenario4 <- calculate_icers(cost       = v_C_Scenario4,
                                    effect     = v_E_Scenario4,
                                    strategies = str_names_scenario4)

df_cea_Scenario5 <- calculate_icers(cost       = v_C_Scenario5,
                                    effect     = v_E_Scenario5,
                                    strategies = str_names_scenario5)



df_cea_Sensitivity1 <- calculate_icers(cost       = v_C_Sensitivity1,
                                       effect     = v_E_Sensitivity1,
                                       strategies = str_names_sensitivity1)


df_cea_Sensitivity2 <- calculate_icers(cost       = v_C_Sensitivity2,
                                       effect     = v_E_Sensitivity2,
                                       strategies = str_names_sensitivity2)


df_cea_Sensitivity3 <- calculate_icers(cost       = v_C_Sensitivity3,
                                       effect     = v_E_Sensitivity3,
                                       strategies = str_names_sensitivity3)

df_cea_Sensitivity4 <- calculate_icers(cost       = v_C_Sensitivity4,
                                       effect     = v_E_Sensitivity4,
                                       strategies = str_names_sensitivity4)



df_cea_Scenarios <- calculate_icers(cost       = c(v_C_basecase[1], v_C_basecase[2], v_C_Scenario1[2], v_C_Scenario2[2], v_C_Scenario3[2], v_C_Scenario4[2], v_C_Scenario5[2]),
                                    effect     = c(v_E_basecase[1], v_E_basecase[2], v_E_Scenario1[2], v_E_Scenario2[2], v_E_Scenario3[2], v_E_Scenario4[2], v_E_Scenario5[2]),
                                    strategies = c("No Screening", "Screening - Baseline", "Screening - Scenario 1", "Screening - Scenario 2", "Screening - Scenario 3", "Screening - Scenario 4", "Screening - Scenario 5"))


df_cea_Sensitivities <- df_cea_Sensitivity1
df_cea_Sensitivities <- rbind(df_cea_Sensitivities, df_cea_Sensitivity2[2,])
df_cea_Sensitivities <- rbind(df_cea_Sensitivities, df_cea_Sensitivity3[2,])
df_cea_Sensitivities <- rbind(df_cea_Sensitivities, df_cea_Sensitivity4[2,])



Data_Sensitivities_LY <- df_cea_Sensitivities
Data_Primary_LY <- df_cea_basecase
Data_Scenarios_LY <- df_cea_Scenarios

























#### LY ICER undiscounted #### 
# Base case scenario
v_C_basecase <- c(outcomes_SoC$tc_NHM_hat, outcomes_SoC$tc_Scr_hat)
v_E_basecase <- c(outcomes_SoC$te_NHM_hat_LY_und, outcomes_SoC$te_Scr_hat_LY_und)

# Scenario 1
v_C_Scenario1 <- c(outcomes_Scenario1$tc_NHM_hat, outcomes_Scenario1$tc_Scr_hat)
v_E_Scenario1 <- c(outcomes_Scenario1$te_NHM_hat_LY_und, outcomes_Scenario1$te_Scr_hat_LY_und)

# Scenario 2
v_C_Scenario2 <- c(outcomes_Scenario2$tc_NHM_hat, outcomes_Scenario2$tc_Scr_hat)
v_E_Scenario2 <- c(outcomes_Scenario2$te_NHM_hat_LY_und, outcomes_Scenario2$te_Scr_hat_LY_und)

# Scenario 3
v_C_Scenario3 <- c(outcomes_Scenario3$tc_NHM_hat, outcomes_Scenario3$tc_Scr_hat)
v_E_Scenario3 <- c(outcomes_Scenario3$te_NHM_hat_LY_und, outcomes_Scenario3$te_Scr_hat_LY_und)

# Scenario 4
v_C_Scenario4 <- c(outcomes_Scenario4$tc_NHM_hat, outcomes_Scenario4$tc_Scr_hat)
v_E_Scenario4 <- c(outcomes_Scenario4$te_NHM_hat_LY_und, outcomes_Scenario4$te_Scr_hat_LY_und)

# Scenario 5
v_C_Scenario5 <- c(outcomes_Scenario5$tc_NHM_hat, outcomes_Scenario5$tc_Scr_hat)
v_E_Scenario5 <- c(outcomes_Scenario5$te_NHM_hat_LY_und, outcomes_Scenario5$te_Scr_hat_LY_und)


#rm(outcomes_Scenario5, v_C_Scenario5, v_E_Scenario5, df_cea_Scenario5)

# Sensitivity 1
v_C_Sensitivity1 <- c(outcomes_Sensitivity1$tc_NHM_hat, outcomes_Sensitivity1$tc_Scr_hat)
v_E_Sensitivity1 <- c(outcomes_Sensitivity1$te_NHM_hat_LY_und, outcomes_Sensitivity1$te_Scr_hat_LY_und)

# Sensitivity 2
v_C_Sensitivity2 <- c(outcomes_Sensitivity2$tc_NHM_hat, outcomes_Sensitivity2$tc_Scr_hat)
v_E_Sensitivity2 <- c(outcomes_Sensitivity2$te_NHM_hat_LY_und, outcomes_Sensitivity2$te_Scr_hat_LY_und)

# Sensitivity 3
v_C_Sensitivity3 <- c(outcomes_Sensitivity3$tc_NHM_hat, outcomes_Sensitivity3$tc_Scr_hat)
v_E_Sensitivity3 <- c(outcomes_Sensitivity3$te_NHM_hat_LY_und, outcomes_Sensitivity3$te_Scr_hat_LY_und)

# Sensitivity 4
v_C_Sensitivity4 <- c(outcomes_Sensitivity4$tc_NHM_hat, outcomes_Sensitivity4$tc_Scr_hat)
v_E_Sensitivity4 <- c(outcomes_Sensitivity4$te_NHM_hat_LY_und, outcomes_Sensitivity4$te_Scr_hat_LY_und)





# use dampack to calculate the ICER
# get independence from dampack!!
df_cea_basecase  <- calculate_icers(cost       = v_C_basecase,
                                    effect     = v_E_basecase,
                                    strategies = str_names_baseline)

df_cea_Scenario1 <- calculate_icers(cost       = v_C_Scenario1,
                                    effect     = v_E_Scenario1,
                                    strategies = str_names_scenario1)

df_cea_Scenario2 <- calculate_icers(cost       = v_C_Scenario2,
                                    effect     = v_E_Scenario2,
                                    strategies = str_names_scenario2)

df_cea_Scenario3 <- calculate_icers(cost       = v_C_Scenario3,
                                    effect     = v_E_Scenario3,
                                    strategies = str_names_scenario3)

df_cea_Scenario4 <- calculate_icers(cost       = v_C_Scenario4,
                                    effect     = v_E_Scenario4,
                                    strategies = str_names_scenario4)

df_cea_Scenario5 <- calculate_icers(cost       = v_C_Scenario5,
                                    effect     = v_E_Scenario5,
                                    strategies = str_names_scenario5)



df_cea_Sensitivity1 <- calculate_icers(cost       = v_C_Sensitivity1,
                                       effect     = v_E_Sensitivity1,
                                       strategies = str_names_sensitivity1)


df_cea_Sensitivity2 <- calculate_icers(cost       = v_C_Sensitivity2,
                                       effect     = v_E_Sensitivity2,
                                       strategies = str_names_sensitivity2)


df_cea_Sensitivity3 <- calculate_icers(cost       = v_C_Sensitivity3,
                                       effect     = v_E_Sensitivity3,
                                       strategies = str_names_sensitivity3)

df_cea_Sensitivity4 <- calculate_icers(cost       = v_C_Sensitivity4,
                                       effect     = v_E_Sensitivity4,
                                       strategies = str_names_sensitivity4)



df_cea_Scenarios <- calculate_icers(cost       = c(v_C_basecase[1], v_C_basecase[2], v_C_Scenario1[2], v_C_Scenario2[2], v_C_Scenario3[2], v_C_Scenario4[2], v_C_Scenario5[2]),
                                    effect     = c(v_E_basecase[1], v_E_basecase[2], v_E_Scenario1[2], v_E_Scenario2[2], v_E_Scenario3[2], v_E_Scenario4[2], v_E_Scenario5[2]),
                                    strategies = c("No Screening", "Screening - Baseline", "Screening - Scenario 1", "Screening - Scenario 2", "Screening - Scenario 3", "Screening - Scenario 4", "Screening - Scenario 5"))


df_cea_Sensitivities <- df_cea_Sensitivity1
df_cea_Sensitivities <- rbind(df_cea_Sensitivities, df_cea_Sensitivity2[2,])
df_cea_Sensitivities <- rbind(df_cea_Sensitivities, df_cea_Sensitivity3[2,])
df_cea_Sensitivities <- rbind(df_cea_Sensitivities, df_cea_Sensitivity4[2,])



Data_Sensitivities_LY_und <- df_cea_Sensitivities
Data_Primary_LY_und <- df_cea_basecase
Data_Scenarios_LY_und <- df_cea_Scenarios







#### Putting together #### 


rm(list=setdiff(ls(), c("Data_Primary_LY", "Data_Primary_LY_und", "Data_Primary_und", 
"Data_Sensitivities_LY_und", "Data_Sensitivities_und", "Data_Sensitivities_LY",
"Data_Scenarios_LY_und", "Data_Scenarios_und", "Data_Scenarios_LY",
"Data_Scenarios", "Data_Sensitivities", "Data_Primary"
)))


Primary_QALY <- rbind(Data_Primary, Data_Primary_und)
Primary_LY <- rbind(Data_Primary_LY, Data_Primary_LY_und)

Scenarios_QALY <- rbind(Data_Scenarios, Data_Scenarios_und)
Scenarios_LY <- rbind(Data_Scenarios_LY, Data_Scenarios_LY_und)

Sensitivity_QALY <- rbind(Data_Sensitivities, Data_Sensitivities_und)
Sensitivity_LY <- rbind(Data_Sensitivities_LY, Data_Sensitivities_LY_und)



# export to latex

library(stargazer)

stargazer(Primary_QALY, type = "latex", title="Primary Analysis QALY", summary = FALSE)
stargazer(Primary_LY, type = "latex", title="Primary Analysis LY", summary = FALSE)

stargazer(Scenarios_QALY, type = "latex", title="Scenario Analysis QALY", summary = FALSE)
stargazer(Scenarios_LY, type = "latex", title="Scenario Analysis LY", summary = FALSE)

stargazer(Sensitivity_QALY, type = "latex", title="Sensitivity Analysis QALY", summary = FALSE)
stargazer(Sensitivity_LY, type = "latex", title="Sensitivity Analysis LY", summary = FALSE)








