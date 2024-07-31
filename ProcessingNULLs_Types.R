### Processing data types, NULLs and missing values

# Import libraries
library(tidyverse)
library(rstatix)
library(dplyr)
library(ggplot2)
library(janitor)
library(tidyr)
library(lubridate)

library(bcv)
library(stats)
library(corrplot)
library(gridExtra)

library(cluster)
library(fpc)
library(infotheo)
library(FactoMineR)
library(factoextra)
library(clustMixType)
library(ltm)
library(car)
library(mclust)
library(Rtsne)
library(ggforce)
library(hclust1d)
library(NbClust)
library(pracma)

library(table1)
library(gtsummary)

library(survival)
library(survminer)
library(ranger)
library(ggfortify)
library(ggpubr)

library(clue)

library(psych)
library(irr)

library(stats)
library(Matrix)
library(xgboost)
library(caret)

library(pROC)
library(reshape2)

# Import data
final_dataset <- read.csv('C:/Users/sgsgu/OneDrive - University of Cambridge/Documents/Work/MSt in Healthcare Data/Unit 6 - Dissertation/Dissertation/Heart Failure Optimisation/Data Component/Output Data/MasterTable_SQLExp_202406241218_NoDups.csv')
final_dataset <- data.frame(final_dataset)

## Process NULLs
# Comorbidities and medications convert NULLS to 0 as an absence of the condition or the medication
final_dataset <- final_dataset %>% 
  mutate(across(AF:Nitrates, ~ifelse(. == "null", 0,
                                     ifelse(. == "Y", 1, .))))
final_dataset <- final_dataset %>% 
  mutate(across(AF:Nitrates, as.numeric))

# Observations and blood tests convert NULLS to NA as an absence is an absence of a measured value
final_dataset <- final_dataset %>% 
  mutate(across(c(ADMID:DISCH_DATETIME, O2SAT:DOD),
                ~ifelse(. == "null", NA, .)))

# Remove the greater and less than signs
final_dataset <- final_dataset %>% 
  mutate(across(c(ADMID:DISCH_DATETIME, O2SAT:DOD),
                ~gsub("[<>]", "", .)))

# Remove dash filler
final_dataset <- final_dataset %>% 
  mutate(across(c(ADMID:DISCH_DATETIME, O2SAT:DOD),
                ~gsub("----", NA, .)))

# Set numeric columns as numeric
final_dataset <- final_dataset %>% 
  mutate(across(c(O2SAT:BNP, ADM_RR, X24HR_RR, DISCH_RR, ADM_WEIGHT, X24HR_WEIGHT, DISCH_WEIGHT, TOTAL_INPUT:FLUID_BALANCE, TIME_TO_NEXT),
                as.numeric))

## Processing data types
# Convert all datetimes to datetimes using lubridate
# Add time to DOD - 2359 chosen as end of day
final_dataset$DOD <- ifelse(is.na(final_dataset$DOD) == FALSE, paste(final_dataset$DOD, "23:59"), NA)
# Convert
final_dataset <- final_dataset %>% 
  mutate(across(c(ADM_DATETIME:DISCH_DATETIME, BNP_DATETIME, ITU_ADMDATETIME:ITU_DISCHDATETIME,
                  ADM_RR_DATETIME, X24HR_RR_DATETIME, DISCH_RR_DATETIME, ADM_WEIGHT_DATETIME, 
                  X24HR_WEIGHT_DATETIME, DISCH_WEIGHT_DATETIME, DOD), ~dmy_hm(.)))

# Convert age_admission to a factor field
unique(final_dataset$AGE_ADMISSION)
final_dataset <- final_dataset %>% 
  mutate(AGE_ADMISSION = factor(AGE_ADMISSION, levels = c("20 - 24",
                                                          "25 - 29",
                                                          "30 - 34",
                                                          "35 - 39",
                                                          "40 - 44",
                                                          "45 - 49", 
                                                          "50 - 54", 
                                                          "55 - 59",
                                                          "60 - 64", 
                                                          "65 - 69", 
                                                          "70 - 74", 
                                                          "75 - 79", 
                                                          "80 - 84",
                                                          "85 - 89", 
                                                          "90 - 94", 
                                                          "95 - 99", 
                                                          "100 - 104")))

### IF I DECIDE TO DO PATIENTS RATHER THAN ADMISSIONS - RUN THE FOLLOWING CODE TO LINE 114
### IF I DECIDE TO DO ADMISSIONS RATHER THAN PATIENTS - COMMENT THE FOLLOWING CODE TO LINE 114
# Remove any duplicates
final_dataset <- final_dataset %>%
  arrange(PATIENTID, ADM_DATETIME) %>%
  filter(duplicated(PATIENTID) == FALSE)

## Remove weight values with the same date as the admission weight
# RR
final_dataset$X24HR_RR <- ifelse(final_dataset$X24HR_RR_DATETIME == final_dataset$ADM_RR_DATETIME, NA, final_dataset$X24HR_RR)
final_dataset$DISCH_RR <- ifelse(final_dataset$DISCH_RR_DATETIME == final_dataset$X24HR_RR_DATETIME, NA,
                                 ifelse(final_dataset$DISCH_RR_DATETIME == final_dataset$ADM_RR_DATETIME, NA, final_dataset$DISCH_RR))

# Weight
final_dataset$X24HR_WEIGHT <- ifelse(final_dataset$X24HR_WEIGHT_DATETIME == final_dataset$ADM_WEIGHT_DATETIME, NA, final_dataset$X24HR_WEIGHT)
final_dataset$DISCH_WEIGHT <- ifelse(final_dataset$DISCH_WEIGHT_DATETIME == final_dataset$X24HR_WEIGHT_DATETIME, NA,
                                     ifelse(final_dataset$DISCH_WEIGHT_DATETIME == final_dataset$ADM_WEIGHT_DATETIME, NA, final_dataset$DISCH_WEIGHT))