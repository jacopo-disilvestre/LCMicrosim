#### Setup ####

rm(list=ls(all=TRUE))	# clear workspace

list_of_packages <- c("here", "flexsurv", "survminer", "tidyr", "muhaz", "dplyr", "haven", "ggsurvfit", "gtsummary", "bshazard", "openxlsx", "survival", "readxl", "ggplot2", "cowplot")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(list_of_packages, library, character.only = TRUE)

#### Analysis ####
# Importing the extracted data

Extraction_Stage_I <- read.csv("/.../Extraction_Stage_I_raw.csv", header = FALSE, sep = ",")
Extraction_Stage_II <- read.csv("/.../Extraction_Stage_II_raw.csv", header = FALSE, sep = ",")
Extraction_Stage_III <- read.csv("/.../Extraction_Stage_III_raw.csv", header = FALSE, sep = ",")
Extraction_Stage_IV <- read.csv("/.../Extraction_Stage_IV_raw.csv", header = FALSE, sep = ",")


colnames(Extraction_Stage_I) <- c("Time", "Survival")
colnames(Extraction_Stage_II) <- c("Time", "Survival")
colnames(Extraction_Stage_III) <- c("Time", "Survival")
colnames(Extraction_Stage_IV) <- c("Time", "Survival")

# Converting to probabilities
Extraction_Stage_I$Survival <- Extraction_Stage_I$Survival/100
Extraction_Stage_II$Survival <- Extraction_Stage_II$Survival/100
Extraction_Stage_III$Survival <- Extraction_Stage_III$Survival/100
Extraction_Stage_IV$Survival <- Extraction_Stage_IV$Survival/100

# Converting time to months
Extraction_Stage_I$Time <- Extraction_Stage_I$Time*12
Extraction_Stage_II$Time <- Extraction_Stage_II$Time*12
Extraction_Stage_III$Time <- Extraction_Stage_III$Time*12
Extraction_Stage_IV$Time <- Extraction_Stage_IV$Time*12



# Define the function to get monthly values
process_data <- function(df) {
  df$Time <- floor(df$Time)
  new_df <- aggregate(df$Survival, by=list(Time=df$Time), FUN=mean)
  colnames(new_df) <- c("Time", "Survival")
  complete_df <- data.frame(Time = 0:60)
  df <- merge(complete_df, new_df, all.x = TRUE)
  df$Survival[1] <- 1
  return(df)
}

# Apply the function to each dataframe
Extraction_Stage_I <- process_data(Extraction_Stage_I)
Extraction_Stage_II <- process_data(Extraction_Stage_II)
Extraction_Stage_III <- process_data(Extraction_Stage_III)
Extraction_Stage_IV <- process_data(Extraction_Stage_IV)

# Export the data
write.xlsx(Extraction_Stage_I, "/.../Extraction_Stage_I.csv")
write.xlsx(Extraction_Stage_II, "/.../Extraction_Stage_II.csv")
write.xlsx(Extraction_Stage_III, "/.../Extraction_Stage_III.csv")
write.xlsx(Extraction_Stage_IV, "/.../Extraction_Stage_IV.csv")


# Importing back the data from Hoyle_and_Henley_I-IV files to extrapolate curves

rm(list=ls(all=TRUE))	# clear workspace

# Specify the path
setwd("/.../")

# Read files in that path, one at the time
Hoyle_and_Henley_I <- read_excel("Hoyle_and_Henley_I.xlsm", range = "R data!A1:F61")
Hoyle_and_Henley_II <- read_excel("Hoyle_and_Henley_II.xlsm", range = "R data!A1:F61")
Hoyle_and_Henley_III <- read_excel("Hoyle_and_Henley_III.xlsm", range = "R data!A1:F61")
Hoyle_and_Henley_IV <- read_excel("Hoyle_and_Henley_IV.xlsm", range = "R data!A1:F61")


attach(Hoyle_and_Henley_I)
attach(Hoyle_and_Henley_II)
attach(Hoyle_and_Henley_III)
attach(Hoyle_and_Henley_IV)


#'"R" code to fit survival curves to the estimated individual patient data


times_start <-c(  rep(start_time_censor, n_censors), rep(start_time_event, n_events) )
times_end <-c(  rep(end_time_censor, n_censors), rep(end_time_event, n_events)  )


#  adding times for patients at risk at last time point
# Stage I
times_start <- c(times_start, rep(60,518))
times_end <- c(times_end, rep(10000,518))
# Stage II
times_start <- c(times_start, rep(60,132))
times_end <- c(times_end, rep(10000,132))
# Stage III
times_start <- c(times_start, rep(60,136))
times_end <- c(times_end, rep(10000,136))
# Stage IV
times_start <- c(times_start, rep(60,83))
times_end <- c(times_end, rep(10000,83))



#   one of these function forms can be used
model_exp <- survreg(Surv(times_start, times_end, type="interval2")~1, dist="exponential")   # Exponential function, interval censoring
model_wei <- survreg(Surv(times_start, times_end, type="interval2")~1, dist="weibull")   # Weibull function, interval censoring
model_logn <- survreg(Surv(times_start, times_end, type="interval2")~1, dist="lognormal")   # Lognormal function, interval censoring
model_logl <- survreg(Surv(times_start, times_end, type="interval2")~1, dist="loglogistic")   # Loglogistic function, interval censoring

#   Compare AIC values
AIC_exp<- -2*summary(model_exp)$loglik[1] + 2*1   #  AIC for exponential distribution
AIC_exp
AIC_wei<--2*summary(model_wei)$loglik[1] + 2*2   #  AIC for Weibull, which is a 2-parameter distribution
AIC_wei
AIC_logn<--2*summary(model_logn)$loglik[1] + 2*2   #  AIC for lognormal, which is a 2-parameter distribution
AIC_logn
AIC_logl<--2*summary(model_logl)$loglik[1] + 2*2   #  AIC for log-logistic, which is a 2-parameter distribution
AIC_logl


#  Intercept and logscale parameters
intercept_exp <- summary(model_exp)$table[1]   # intercept parameter for exponential
intercept_exp  
intercept_wei <- summary(model_wei)$table[1]   # intercept parameter for Weibull
log_scale_wei <- summary(model_wei)$table[2]   # log scale parameter for Weibull
intercept_wei
log_scale_wei 

intercept_logn <- summary(model_logn)$table[1]   # intercept parameter for lognormal
log_scale_logn <- summary(model_logn)$table[2]   # log scale parameter for lognormal
intercept_logn
log_scale_logn
intercept_logl <- summary(model_logl)$table[1]   # intercept parameter for loglogistic
log_scale_logl <- summary(model_logl)$table[2]   # log scale parameter for loglogistic
intercept_logl  
log_scale_logl  

#  For the Probabilistic Sensitivity Analysis, we need the Cholesky matrix, which captures the variance and covariance of parameters
cholesky_exp<-t(chol(summary(model_exp)$var))    #  Cholesky matrix for exponential
cholesky_exp
cholesky_wei<-t(chol(summary(model_wei)$var))    #  Cholesky matrix for weibull
cholesky_wei
cholesky_logn<-t(chol(summary(model_logn)$var))    #  Cholesky matrix for lognormal
cholesky_logn
cholesky_logl<-t(chol(summary(model_logl)$var))    #  Cholesky matrix for loglogistic
cholesky_logl


Extraction_Stage_I0 <- c(AIC_exp, AIC_wei, AIC_logn, AIC_logl, intercept_exp, intercept_wei, intercept_logn, intercept_logl, 0, log_scale_wei, log_scale_logn, log_scale_logl)
Extraction_Stage_I0 <- matrix(Extraction_Stage_I0,nrow=3,ncol=4, byrow=TRUE)
rownames <- c("AIC", "intercept", "log(scale)")
Extraction_Stage_I0 <- data.frame(Extraction_Stage_I0, rowNames = rownames)
colnames(Extraction_Stage_I0) <- c("exponential", "Weibull", "lognormal", "loglogistic")
# Stage I
write.xlsx(Extraction_Stage_I0, "HH_Hoyle_and_Henley_I_output.xlsx", rowNames=TRUE)
# Stage II
write.xlsx(Extraction_Stage_I0, "HH_Hoyle_and_Henley_II_output.xlsx", rowNames=TRUE)
# Stage III
write.xlsx(Extraction_Stage_I0, "HH_Hoyle_and_Henley_III_output.xlsx", rowNames=TRUE)
# Stage IV
write.xlsx(Extraction_Stage_I0, "HH_Hoyle_and_Henley_IV_output.xlsx", rowNames=TRUE)

#copy Cholskey matrices
Extraction_Stage_I1 <- data.frame(cholesky_exp)
Extraction_Stage_I2 <- data.frame(cholesky_logl)
Extraction_Stage_I3 <- data.frame(cholesky_logn)
Extraction_Stage_I4 <- data.frame(cholesky_wei)

all_tabs <- list("exp"=Extraction_Stage_I1,"logl"=Extraction_Stage_I2, "logn"=Extraction_Stage_I3, "wei"=Extraction_Stage_I4)
# Stage I
write.xlsx(all_tabs, "HH_Hoyle_and_Henley_I.xlsx", rowNames=TRUE)
# Stage II
write.xlsx(all_tabs, "HH_Hoyle_and_Henley_II.xlsx", rowNames=TRUE)
# Stage III
write.xlsx(all_tabs, "HH_Hoyle_and_Henley_III.xlsx", rowNames=TRUE)
# Stage IV
write.xlsx(all_tabs, "HH_Hoyle_and_Henley_IV.xlsx", rowNames=TRUE)

# Export to Excel and make calculations there

# Import the extrapolated curves back from Excel and make nice graphs
rm(list=ls(all=TRUE))	# clear workspace

# Specify the path
setwd("/.../")


Extrapolated_Times <- read_excel("data_1.xlsx", range = "extrapolation!A14:B735")

KM_I <- read_excel("data_1.xlsx", range = "extrapolation!V14:V75")
KM_II <- read_excel("data_1.xlsx", range = "extrapolation!W14:W75")
KM_III <- read_excel("data_1.xlsx", range = "extrapolation!X14:X75")
KM_IV <- read_excel("data_1.xlsx", range = "extrapolation!Y14:Y75")

# Extend KM to 721 with NA
KM_I <- data.frame(KM = c(KM_I$KM, rep(NA, 721 - nrow(KM_I))))
KM_II <- data.frame(KM = c(KM_II$KM, rep(NA, 721 - nrow(KM_II))))
KM_III <- data.frame(KM = c(KM_III$KM, rep(NA, 721 - nrow(KM_III))))
KM_IV <- data.frame(KM = c(KM_IV$KM, rep(NA, 721 - nrow(KM_IV))))


Extrapolated_Curves_I <- read_excel("data_1.xlsx", range = "extrapolation!C14:F735")
Extrapolated_Curves_II <- read_excel("data_1.xlsx", range = "extrapolation!G14:J735")
Extrapolated_Curves_III <- read_excel("data_1.xlsx", range = "extrapolation!K14:N735")
Extrapolated_Curves_IV <- read_excel("data_1.xlsx", range = "extrapolation!O14:R735")

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



p_ID <- Extrapolated_Curves_I[c("Months", "exponential")]
p_IID <- Extrapolated_Curves_II[c("Months", "Weibull")]
p_IIID <- Extrapolated_Curves_III[c("Months", "lognormal")]
p_IVD <- Extrapolated_Curves_IV[c("Months", "lognormal")]





# Potting extrapolated survival curves


# Assuming dataframes are named Extrapolated_Curves_I, Extrapolated_Curves_II, Extrapolated_Curves_III, and Extrapolated_Curves_IV
df_list <- list(Extrapolated_Curves_I, Extrapolated_Curves_II, Extrapolated_Curves_III, Extrapolated_Curves_IV)

# Create a list to store the plots
plot_list <- list()

# Loop through each dataframe and create a plot
for(i in 1:length(df_list)) {
  df <- df_list[[i]]
  df1 <- df[1:61, ]
  df2 <- df[1:121, ]
  
  p <- ggplot() +
    geom_line(data = df1, aes(x = Months, y = KM, color = "KM")) +
    geom_line(data = df2, aes(x = Months, y = exponential, color = "exponential")) +
    geom_line(data = df2, aes(x = Months, y = Weibull, color = "Weibull")) +
    geom_line(data = df2, aes(x = Months, y = lognormal, color = "lognormal")) +
    geom_line(data = df2, aes(x = Months, y = loglogistic, color = "loglogistic")) +
    labs(x = "Time in Months", y = "LC Overall Survival", 
         title = paste("Lung Cancer Stage", i, "| Predictions")) +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_color_manual(values = c("blue", "red", "green", "purple", "orange"),
                       name = "Legend",
                       breaks = c("KM", "exponential", "Weibull", "lognormal", "loglogistic"),
                       labels = c("KM", "Exponential", "Weibull", "Log-normal", "Log-logistic"))
  
  # Add the plot to the list
  plot_list[[i]] <- p
}

# Extract the legend
common_legend <- cowplot::get_legend(p + theme(legend.position="bottom", legend.justification = "center", legend.margin=margin(-10, -10, -10, -10)))

# Arrange the plots in a grid
p_grid <- plot_grid(plotlist = plot_list, ncol = 2)

# Combine the plots and the legend
final_plot <- plot_grid(p_grid, common_legend, ncol = 1, rel_heights = c(1, .1))

# Print the final plot
print(final_plot)

# Export the plot in .pdf to be read in LaTeX
ggsave(filename = "final_survival_plot.pdf", plot = final_plot, device = "pdf", width = 12, height = 7, units = "in")




# AIC table #

## Load packages ##

if (!require('pacman')) install.packages('pacman'); library(pacman)
# load (install if required) packages from CRAN
p_load("devtools", "dplyr", "scales", "ellipse", "ggplot2", "lazyeval", "igraph", "truncnorm", "ggraph", "reshape2", "knitr", "markdown", "stringr", "dampack", "msce", "approx", "psych", "stargazer", "xtable")


# Import the extrapolated curves back and make nice graphs
rm(list=ls(all=TRUE))	# clear workspace

# Specify the path
setwd("/.../")


AIC_Values <- read_excel("AIC values.xlsx", range = "AIC!A1:E5")

# Use stargazer to create a table from your dataframe


# Assuming 'df' is your dataframe
latex_table <- xtable(AIC_Values)

# Print the table in LaTeX syntax
print(latex_table, type = "latex") #this latex code was then modified, as it was not working properly
# the final code is already on Overleaf









