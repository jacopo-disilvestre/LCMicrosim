#### Setup ####

rm(list=ls(all=TRUE))	# clear workspace

list_of_packages <- c("here", "flexsurv", "survminer", "tidyr", "muhaz", "dplyr", "haven", "ggsurvfit", "gtsummary", "bshazard", "openxlsx", "survival", "readxl", "ggplot2", "cowplot")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(list_of_packages, library, character.only = TRUE)


#### Preparing the data ####

# Data I have to use and which format I have:
# Age specific Lung Cancer (LC) Mortality in age groups 
# Age specific All-cause (AC) Mortality per year
# LC HR by status, one for all ages
# All-cause Hazard Ratios (HR) by status, in age groups


# LC HRs 

# LC HR Smokers/ExSmokers vs never smokers male
LC_HR_S_M <- 20.05 # lung cancer mortality hazard ratio of current male smokers compared to never smokers
LC_HR_ExS_M <- 4.08 # lung cancer mortality hazard ratio of former male smokers compared to never smokers
# LC HR Smokers/ExSmokers vs never smokers female
LC_HR_S_F <- 13.97 # lung cancer mortality hazard ratio of current female smokers compared to never smokers
LC_HR_ExS_F <- 2.71 # lung cancer mortality hazard ratio of former female smokers compared to never smokers

# AC HRs

# Hazard ratios by age groups 
# 45-49, 50-54, 55-59, 60-64, 65-69, 70-74, 75-79, 80-84, 85-89, 90-99
# AC HR Smokers vs never smokers male
AC_HR_S_M <- c(3.19, 3.24, 3.71, 4, 4.6, 4.24, 2.45, 2.48, 1.31, 0.9) # males # hazard ratio for death from any cause among males who smoke or smoked compared to never smokers
# AC HR Smokers vs never smokers female
AC_HR_S_F <- c(3.53, 3.74, 3.91, 3.72, 3.38, 3.58, 2.56, 1.99, 1.27, 1.26) # females # hazard ratio for death from any cause among females who smoke or smoked compared to never smokers


# Corrected for these age groups
# 45-49, 50-54, 55-59, 60-64, 65-69, 70-74, 75-79, 80-84, 85+
# AC HR Smokers vs never smokers male
# AC HR Smokers vs never smokers male, adjusted for the right age groups
AC_HR_S_M <- c(3.19, 3.24, 3.71, 4, 4.6, 4.24, 2.45, 2.48, (1.31+0.9)/2) # males # hazard ratio for death from any cause among males who smoke or smoked compared to never smokers
# AC HR Smokers vs never smokers female, adjusted for the right age groups
AC_HR_S_F <- c(3.53, 3.74, 3.91, 3.72, 3.38, 3.58, 2.56, 1.99, (1.27+1.26)/2) # females # hazard ratio for death from any cause among females who smoke or smoked compared to never smokers
# AC HR ExSmokers vs never smokers male
AC_HR_ExS_M <- AC_HR_S_M # assuming that the hazard ratio for male former smokers is the same as for current smokers
# AC HR ExSmokers vs never smokers female
AC_HR_ExS_F <- AC_HR_S_F # assuming that the hazard ratio for female former smokers is the same as for current smokers



# LC mortality

LC_data <- read.csv("/.../LC_data.csv")

# Age groups of LC Mortality: "[0]" "[1-4]" "[5-9]" "[10-14]" "[15-19]" "[20-24]" "[25-29]" "[30-34]" "[35-39]" "[40-44]" "[45-49]" "[50-54]" "[55-59]" "[60-64]" "[65-69]" "[70-74]" "[75-79]" "[80-84]" "[85+]"

LC_mortality_M <- data.frame("Age_Groups" = LC_data[1:19,"Age.Group"], Mortality = LC_data[1:19,"Death.rate.per.100.000.population"])
LC_mortality_F <- data.frame("Age_Groups" = LC_data[21:39,"Age.Group"], Mortality = LC_data[21:39,"Death.rate.per.100.000.population"])

LC_mortality_M$Mortality <- LC_mortality_M$Mortality/100000 # making it per person since it's reported to be every 100000
LC_mortality_F$Mortality <- LC_mortality_F$Mortality/100000 # making it per person since it's reported to be every 100000

LC_mortality_M$Mortality <- LC_mortality_M$Mortality/12 # making it per month since it's reported to be per year
LC_mortality_F$Mortality <- LC_mortality_F$Mortality/12 # making it per month since it's reported to be per year

rm(LC_data, list_of_packages, new_packages)


# All-cause mortality

AC_mortality_M <-read_excel("/.../males.xlsx", range = "Dodssannsynlighet!A5:B112")
colnames(AC_mortality_M) <- c("Age", "Mortality")
AC_mortality_F <-read_excel("/.../females.xlsx", range = "Dodssannsynlighet!A5:B112")
colnames(AC_mortality_F) <- c("Age", "Mortality")

AC_mortality_M$Mortality <- AC_mortality_M$Mortality/1000 # making it per person since it's reported to be every 1000
AC_mortality_F$Mortality <- AC_mortality_F$Mortality/1000 # making it per person since it's reported to be every 1000

AC_mortality_M$Mortality <- AC_mortality_M$Mortality/12 # making it per month since it's reported to be per year
AC_mortality_F$Mortality <- AC_mortality_F$Mortality/12 # making it per month since it's reported to be per year

AC_mortality_M$Age <- seq(0,106,1) # All-cause mortality rate is from 0 to 106 years
AC_mortality_F$Age <- seq(0,106,1) # All-cause mortality rate is from 0 to 106 years



# Get mortality from AC_Mortality in age groups

# Define age groups
age_breaks <- c(-1, 0, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, Inf)
age_labels <- c("[0]", "[1-4]", "[5-9]", "[10-14]", "[15-19]", "[20-24]", "[25-29]", "[30-34]", 
                "[35-39]", "[40-44]", "[45-49]", "[50-54]", "[55-59]", "[60-64]", 
                "[65-69]", "[70-74]", "[75-79]", "[80-84]", "[85+]")

# Categorize ages into age groups
AC_mortality_M$Age_Groups <- cut(AC_mortality_M$Age, breaks = age_breaks, labels = age_labels, right = TRUE)

# Calculate average mortality for each age group
AC_mortality_M_Groups <- AC_mortality_M %>%
  group_by(Age_Groups) %>%
  summarize(Mortality = mean(Mortality))

# Categorize ages into age groups
AC_mortality_F$Age_Groups <- cut(AC_mortality_F$Age, breaks = age_breaks, labels = age_labels, right = TRUE)

# Calculate average mortality for each age group
AC_mortality_F_Groups <- AC_mortality_F %>%
  group_by(Age_Groups) %>%
  summarize(Mortality = mean(Mortality)) 

rm(AC_mortality_M, AC_mortality_F, age_breaks, age_labels)



#### Calculating OC Mortality ####


# Preparing the vectors for the multiplication
# AC Mortality has 10 age groups, use HR for those I have and for the others we just keep the normal value


length_diff <- length(AC_mortality_M_Groups$Mortality) - length(AC_HR_S_M)

AC_HR_S_M <- c(rep(1, length_diff), AC_HR_S_M)
AC_HR_S_F <- c(rep(1, length_diff), AC_HR_S_F)
AC_HR_ExS_M <- c(rep(1, length_diff), AC_HR_ExS_M)
AC_HR_ExS_F <- c(rep(1, length_diff), AC_HR_ExS_F)

rm(length_diff)


# Calculating the AC Mortality by status and gender, 19 age groups
# Age groups of AC Mortality: "[0]" "[1-4]" "[5-9]" "[10-14]" "[15-19]" "[20-24]" "[25-29]" "[30-34]" "[35-39]" "[40-44]" "[45-49]" "[50-54]" "[55-59]" "[60-64]" "[65-69]" "[70-74]" "[75-79]" "[80-84]" "[85+]"
AC_Mortality_S_M <- AC_mortality_M_Groups$Mortality * AC_HR_S_M
AC_Mortality_ExS_M <- AC_mortality_M_Groups$Mortality * AC_HR_ExS_M
AC_Mortality_S_F <- AC_mortality_F_Groups$Mortality * AC_HR_S_F
AC_Mortality_ExS_F <- AC_mortality_F_Groups$Mortality * AC_HR_ExS_F


# Calculating the LC Mortality by status and gender, 19 age groups
# Age groups of LC Mortality: "[0]" "[1-4]" "[5-9]" "[10-14]" "[15-19]" "[20-24]" "[25-29]" "[30-34]" "[35-39]" "[40-44]" "[45-49]" "[50-54]" "[55-59]" "[60-64]" "[65-69]" "[70-74]" "[75-79]" "[80-84]" "[85+]"

LC_Mortality_S_M <- LC_mortality_M$Mortality * LC_HR_S_M
LC_Mortality_ExS_M <- LC_mortality_M$Mortality * LC_HR_ExS_M
LC_Mortality_S_F <- LC_mortality_F$Mortality * LC_HR_S_F
LC_Mortality_ExS_F <- LC_mortality_F$Mortality * LC_HR_ExS_F


# Final calculation of OC Mortality
# Subtracting LC Mortality from AC Mortality

OC_Mortality_S_M <- AC_Mortality_S_M - LC_Mortality_S_M

OC_Mortality_ExS_M <- AC_Mortality_ExS_M - LC_Mortality_ExS_M

OC_Mortality_S_F <- AC_Mortality_S_F - LC_Mortality_S_F

OC_Mortality_ExS_F <- AC_Mortality_ExS_F - LC_Mortality_ExS_F

# Check if there are negative values, all falses
any(OC_Mortality_S_M < 0) 
any(OC_Mortality_ExS_M < 0)
any(OC_Mortality_S_F < 0)
any(OC_Mortality_ExS_F < 0)

OC_Mortality <- data.frame("Age_Groups" = LC_mortality_M$Age_Groups, "OC_Mortality_S_M" = OC_Mortality_S_M, "OC_Mortality_ExS_M" = OC_Mortality_ExS_M, "OC_Mortality_S_F" = OC_Mortality_S_F, "OC_Mortality_ExS_F" = OC_Mortality_ExS_F)

OC_Mortality$Age_Groups <- c("[0]", "[1-48]", "[49-108]", "[109-168]", "[169-228]", "[229-288]", "[289-348]", "[349-408]", "[409-468]", "[469-528]", "[529-588]", "[589-648]", "[649-708]", "[709-768]", "[769-828]", "[829-888]", "[889-948]", "[949-1008]", "[1009+]")


rownames(OC_Mortality) <- OC_Mortality$Age_Groups

#rm(list=setdiff(ls(), "OC_Mortality"))

rm(AC_HR_S_M, AC_HR_S_F, AC_HR_ExS_M, AC_HR_ExS_F, LC_HR_S_M, LC_HR_ExS_M, LC_HR_S_F, LC_HR_ExS_F, AC_Mortality_S_M, AC_Mortality_ExS_M, AC_Mortality_S_F, AC_Mortality_ExS_F, LC_Mortality_S_M, LC_Mortality_ExS_M, LC_Mortality_S_F, LC_Mortality_ExS_F, OC_Mortality_S_M, OC_Mortality_ExS_M, OC_Mortality_S_F, OC_Mortality_ExS_F)
rm(AC_mortality_M_Groups, AC_mortality_F_Groups, LC_mortality_M, LC_mortality_F)
