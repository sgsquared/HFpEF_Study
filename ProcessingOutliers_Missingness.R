### Processing missingness and outliers

# Create dataset for missingness and imputation
imputed_dataset <- final_dataset

# Calculate amount of missingness
column_missingness <- imputed_dataset %>%
  summarise_all(~ mean(is.na(.)))
column_missingness <- column_missingness %>% 
  mutate_all(.funs = ~ . * 100)
print(column_missingness)
# Processing missingness
# - Remove columns with >50% missing data
# - Impute the rest with SVDimpute (see 150 ref 24) - note, if using XGBoost, imputation not required
# Relies on the assumption of missing data at random

# Drop >50% missingness columns
# BNP kept despite a high level of missingness
imputed_dataset <- imputed_dataset %>% 
  dplyr::select(-INR, -Troponin)

## Prior to imputation, identified possible erroneous outliers

# Identify outliers
# Summary stats for each of the continuous variables
summary_stats <- imputed_dataset %>%
  summarise(
    across(
      c("O2SAT", "RR", "SBP", "DBP", "HR", "Weight", "Height", "WCC", "Hb", "Plts", "Na", "K", "Cr", "Ur", "CRP", "Alb", "Glucose", "Lactate", "BNP"),
      list(
        var = ~var(., na.rm = TRUE),
        min = ~min(., na.rm = TRUE),
        Q1 = ~quantile(., 0.25, na.rm = TRUE),
        median = ~median(., na.rm = TRUE),
        Q3 = ~quantile(., 0.75, na.rm = TRUE),
        max = ~max(., na.rm = TRUE),
        mean = ~mean(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE)
      )
    )
  )

summary_stats_long <- summary_stats %>%
  pivot_longer(
    everything(),
    names_to = c("variable", "stat"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  )

## Identify outliers one variable at a time - Hampel filter used and subsequently individual values assessed clinically for imputation or not
#O2SAT
lower_bound_O2SAT <- median(imputed_dataset$O2SAT, na.rm = TRUE) - 3 * mad(imputed_dataset$O2SAT, constant = 1, na.rm = TRUE)
lower_bound_O2SAT
upper_bound_O2SAT <- median(imputed_dataset$O2SAT, na.rm = TRUE) + 3 * mad(imputed_dataset$O2SAT, constant = 1, na.rm = TRUE)
upper_bound_O2SAT
outlier_ind_O2SAT <- which(imputed_dataset$O2SAT < lower_bound_O2SAT | imputed_dataset$O2SAT > upper_bound_O2SAT)
outlier_ind_O2SAT

outliersO2SAT <- imputed_dataset[outlier_ind_O2SAT, ]

#RR
lower_bound_RR <- median(imputed_dataset$RR, na.rm = TRUE) - 3 * mad(imputed_dataset$RR, constant = 1, na.rm = TRUE)
lower_bound_RR
upper_bound_RR <- median(imputed_dataset$RR, na.rm = TRUE) + 3 * mad(imputed_dataset$RR, constant = 1, na.rm = TRUE)
upper_bound_RR
outlier_ind_RR <- which(imputed_dataset$RR < lower_bound_RR | imputed_dataset$RR > upper_bound_RR)
outlier_ind_RR

outliersRR <- imputed_dataset[outlier_ind_RR, ]

#SBP
lower_bound_SBP <- median(imputed_dataset$SBP, na.rm = TRUE) - 3 * mad(imputed_dataset$SBP, constant = 1, na.rm = TRUE)
lower_bound_SBP
upper_bound_SBP <- median(imputed_dataset$SBP, na.rm = TRUE) + 3 * mad(imputed_dataset$SBP, constant = 1, na.rm = TRUE)
upper_bound_SBP
outlier_ind_SBP <- which(imputed_dataset$SBP < lower_bound_SBP | imputed_dataset$SBP > upper_bound_SBP)
outlier_ind_SBP

outliersSBP <- imputed_dataset[outlier_ind_SBP, ]

#DBP
lower_bound_DBP <- median(imputed_dataset$DBP, na.rm = TRUE) - 3 * mad(imputed_dataset$DBP, constant = 1, na.rm = TRUE)
lower_bound_DBP
upper_bound_DBP <- median(imputed_dataset$DBP, na.rm = TRUE) + 3 * mad(imputed_dataset$DBP, constant = 1, na.rm = TRUE)
upper_bound_DBP
outlier_ind_DBP <- which(imputed_dataset$DBP < lower_bound_DBP | imputed_dataset$DBP > upper_bound_DBP)
outlier_ind_DBP

outliersDBP <- imputed_dataset[outlier_ind_DBP, ]

#HR
lower_bound_HR <- median(imputed_dataset$HR, na.rm = TRUE) - 3 * mad(imputed_dataset$HR, constant = 1, na.rm = TRUE)
lower_bound_HR
upper_bound_HR <- median(imputed_dataset$HR, na.rm = TRUE) + 3 * mad(imputed_dataset$HR, constant = 1, na.rm = TRUE)
upper_bound_HR
outlier_ind_HR <- which(imputed_dataset$HR < lower_bound_HR | imputed_dataset$HR > upper_bound_HR)
outlier_ind_HR

outliersHR <- imputed_dataset[outlier_ind_HR, ]

#Weight
lower_bound_Weight <- median(imputed_dataset$Weight, na.rm = TRUE) - 3 * mad(imputed_dataset$Weight, constant = 1, na.rm = TRUE)
lower_bound_Weight
upper_bound_Weight <- median(imputed_dataset$Weight, na.rm = TRUE) + 3 * mad(imputed_dataset$Weight, constant = 1, na.rm = TRUE)
upper_bound_Weight
outlier_ind_Weight <- which(imputed_dataset$Weight < lower_bound_Weight | imputed_dataset$Weight > upper_bound_Weight)
outlier_ind_Weight

outliersWeight <- imputed_dataset[outlier_ind_Weight, ]

#Height
lower_bound_Height <- median(imputed_dataset$Height, na.rm = TRUE) - 3 * mad(imputed_dataset$Height, constant = 1, na.rm = TRUE)
lower_bound_Height
upper_bound_Height <- median(imputed_dataset$Height, na.rm = TRUE) + 3 * mad(imputed_dataset$Height, constant = 1, na.rm = TRUE)
upper_bound_Height
outlier_ind_Height <- which(imputed_dataset$Height < lower_bound_Height | imputed_dataset$Height > upper_bound_Height)
outlier_ind_Height

outliersHeight <- imputed_dataset[outlier_ind_Height, ]

#WCC
lower_bound_WCC <- median(imputed_dataset$WCC, na.rm = TRUE) - 3 * mad(imputed_dataset$WCC, constant = 1, na.rm = TRUE)
lower_bound_WCC
upper_bound_WCC <- median(imputed_dataset$WCC, na.rm = TRUE) + 3 * mad(imputed_dataset$WCC, constant = 1, na.rm = TRUE)
upper_bound_WCC
outlier_ind_WCC <- which(imputed_dataset$WCC < lower_bound_WCC | imputed_dataset$WCC > upper_bound_WCC)
outlier_ind_WCC

outliersWCC <- imputed_dataset[outlier_ind_WCC, ]

#Hb
lower_bound_Hb <- median(imputed_dataset$Hb, na.rm = TRUE) - 3 * mad(imputed_dataset$Hb, constant = 1, na.rm = TRUE)
lower_bound_Hb
upper_bound_Hb <- median(imputed_dataset$Hb, na.rm = TRUE) + 3 * mad(imputed_dataset$Hb, constant = 1, na.rm = TRUE)
upper_bound_Hb
outlier_ind_Hb <- which(imputed_dataset$Hb < lower_bound_Hb | imputed_dataset$Hb > upper_bound_Hb)
outlier_ind_Hb

outliersHb <- imputed_dataset[outlier_ind_Hb, ]
# Take only value of 0 and impute

#Plts
lower_bound_Plts <- median(imputed_dataset$Plts, na.rm = TRUE) - 3 * mad(imputed_dataset$Plts, constant = 1, na.rm = TRUE)
lower_bound_Plts
upper_bound_Plts <- median(imputed_dataset$Plts, na.rm = TRUE) + 3 * mad(imputed_dataset$Plts, constant = 1, na.rm = TRUE)
upper_bound_Plts
outlier_ind_Plts <- which(imputed_dataset$Plts < lower_bound_Plts | imputed_dataset$Plts > upper_bound_Plts)
outlier_ind_Plts

outliersPlts <- imputed_dataset[outlier_ind_Plts, ]

#Na
lower_bound_Na <- median(imputed_dataset$Na, na.rm = TRUE) - 3 * mad(imputed_dataset$Na, constant = 1, na.rm = TRUE)
lower_bound_Na
upper_bound_Na <- median(imputed_dataset$Na, na.rm = TRUE) + 3 * mad(imputed_dataset$Na, constant = 1, na.rm = TRUE)
upper_bound_Na
outlier_ind_Na <- which(imputed_dataset$Na < lower_bound_Na | imputed_dataset$Na > upper_bound_Na)
outlier_ind_Na

outliersNa <- imputed_dataset[outlier_ind_Na, ]

#K
lower_bound_K <- median(imputed_dataset$K, na.rm = TRUE) - 3 * mad(imputed_dataset$K, constant = 1, na.rm = TRUE)
lower_bound_K
upper_bound_K <- median(imputed_dataset$K, na.rm = TRUE) + 3 * mad(imputed_dataset$K, constant = 1, na.rm = TRUE)
upper_bound_K
outlier_ind_K <- which(imputed_dataset$K < lower_bound_K | imputed_dataset$K > upper_bound_K)
outlier_ind_K

outliersK <- imputed_dataset[outlier_ind_K, ]

#Cr
lower_bound_Cr <- median(imputed_dataset$Cr, na.rm = TRUE) - 3 * mad(imputed_dataset$Cr, constant = 1, na.rm = TRUE)
lower_bound_Cr
upper_bound_Cr <- median(imputed_dataset$Cr, na.rm = TRUE) + 3 * mad(imputed_dataset$Cr, constant = 1, na.rm = TRUE)
upper_bound_Cr
outlier_ind_Cr <- which(imputed_dataset$Cr < lower_bound_Cr | imputed_dataset$Cr > upper_bound_Cr)
outlier_ind_Cr

outliersCr <- imputed_dataset[outlier_ind_Cr, ]
# Impute values <10

#Ur
lower_bound_Ur <- median(imputed_dataset$Ur, na.rm = TRUE) - 3 * mad(imputed_dataset$Ur, constant = 1, na.rm = TRUE)
lower_bound_Ur
upper_bound_Ur <- median(imputed_dataset$Ur, na.rm = TRUE) + 3 * mad(imputed_dataset$Ur, constant = 1, na.rm = TRUE)
upper_bound_Ur
outlier_ind_Ur <- which(imputed_dataset$Ur < lower_bound_Ur | imputed_dataset$Ur > upper_bound_Ur)
outlier_ind_Ur

outliersUr <- imputed_dataset[outlier_ind_Ur, ]

#CRP
lower_bound_CRP <- median(imputed_dataset$CRP, na.rm = TRUE) - 3 * mad(imputed_dataset$CRP, constant = 1, na.rm = TRUE)
lower_bound_CRP
upper_bound_CRP <- median(imputed_dataset$CRP, na.rm = TRUE) + 3 * mad(imputed_dataset$CRP, constant = 1, na.rm = TRUE)
upper_bound_CRP
outlier_ind_CRP <- which(imputed_dataset$CRP < lower_bound_CRP | imputed_dataset$CRP > upper_bound_CRP)
outlier_ind_CRP

outliersCRP <- imputed_dataset[outlier_ind_CRP, ]

#Alb
lower_bound_Alb <- median(imputed_dataset$Alb, na.rm = TRUE) - 3 * mad(imputed_dataset$Alb, constant = 1, na.rm = TRUE)
lower_bound_Alb
upper_bound_Alb <- median(imputed_dataset$Alb, na.rm = TRUE) + 3 * mad(imputed_dataset$Alb, constant = 1, na.rm = TRUE)
upper_bound_Alb
outlier_ind_Alb <- which(imputed_dataset$Alb < lower_bound_Alb | imputed_dataset$Alb > upper_bound_Alb)
outlier_ind_Alb

outliersAlb <- imputed_dataset[outlier_ind_Alb, ]

#Glucose
lower_bound_Glucose <- median(imputed_dataset$Glucose, na.rm = TRUE) - 3 * mad(imputed_dataset$Glucose, constant = 1, na.rm = TRUE)
lower_bound_Glucose
upper_bound_Glucose <- median(imputed_dataset$Glucose, na.rm = TRUE) + 3 * mad(imputed_dataset$Glucose, constant = 1, na.rm = TRUE)
upper_bound_Glucose
outlier_ind_Glucose <- which(imputed_dataset$Glucose < lower_bound_Glucose | imputed_dataset$Glucose > upper_bound_Glucose)
outlier_ind_Glucose

outliersGlucose <- imputed_dataset[outlier_ind_Glucose, ]

#Lactate
lower_bound_Lactate <- median(imputed_dataset$Lactate, na.rm = TRUE) - 3 * mad(imputed_dataset$Lactate, constant = 1, na.rm = TRUE)
lower_bound_Lactate
upper_bound_Lactate <- median(imputed_dataset$Lactate, na.rm = TRUE) + 3 * mad(imputed_dataset$Lactate, constant = 1, na.rm = TRUE)
upper_bound_Lactate
outlier_ind_Lactate <- which(imputed_dataset$Lactate < lower_bound_Lactate | imputed_dataset$Lactate > upper_bound_Lactate)
outlier_ind_Lactate

outliersLactate <- imputed_dataset[outlier_ind_Lactate, ]

#BNP
lower_bound_BNP <- median(imputed_dataset$BNP, na.rm = TRUE) - 3 * mad(imputed_dataset$BNP, constant = 1, na.rm = TRUE)
lower_bound_BNP
upper_bound_BNP <- median(imputed_dataset$BNP, na.rm = TRUE) + 3 * mad(imputed_dataset$BNP, constant = 1, na.rm = TRUE)
upper_bound_BNP
outlier_ind_BNP <- which(imputed_dataset$BNP < lower_bound_BNP | imputed_dataset$BNP > upper_bound_BNP)
outlier_ind_BNP

outliersBNP <- imputed_dataset[outlier_ind_BNP, ]

# Summary
# WCC >50 - impute
# Hb < 30 - impute
# Cr <10 - impute

# Remove outliers
imputed_dataset$Hb <- ifelse(imputed_dataset$Hb < 30, NA, imputed_dataset$Hb)
imputed_dataset$WCC <- ifelse(imputed_dataset$WCC >50, NA, imputed_dataset$WCC)
imputed_dataset$Cr <- ifelse(imputed_dataset$Cr <10, NA, imputed_dataset$Cr)

# Impute all NULLs
columns_impute <- imputed_dataset %>% 
  dplyr::select(O2SAT:BNP)

# Imputation
imputed_matrix <- impute.svd(as.matrix(columns_impute), maxit = 10000)

# Reinsert imputed values into dataframe
imputed_columns <- as.data.frame(imputed_matrix)
colnames(imputed_columns) <- colnames(columns_impute)

# Remove end columns (think I will need these back to be honest) - these offer some sort of statistical testing
imputed_columns <- imputed_columns[, -c(20,21)]
imputed_dataset[, c("O2SAT", "RR", "SBP", "DBP", "HR", "Weight", "Height", 
                    "WCC", "Hb", "Plts", "Na", "K", "Cr", "Ur", "CRP", "Alb",
                    "Glucose", "Lactate", "BNP")] <- imputed_columns

# remove unneccessary datasets
remove(imputed_matrix)
remove(imputed_columns)
remove(columns_impute)

remove(column_missingness)

remove(outliersAlb, 
       outliersBNP, 
       outliersCr, 
       outliersCRP, 
       outliersDBP, 
       outliersGlucose, 
       outliersHb, 
       outliersHeight, 
       outliersHR, 
       outliersK, 
       outliersLactate, 
       outliersNa, 
       outliersO2SAT, 
       outliersPlts, 
       outliersRR, 
       outliersSBP, 
       outliersUr, 
       outliersWCC, 
       outliersWeight)

remove(summary_stats)

remove(lower_bound_Alb,
       lower_bound_BNP,
       lower_bound_Cr,
       lower_bound_CRP,
       lower_bound_DBP,
       lower_bound_Glucose,
       lower_bound_Hb,
       lower_bound_Height,
       lower_bound_HR,
       lower_bound_K,
       lower_bound_Lactate,
       lower_bound_Na,
       lower_bound_O2SAT,
       lower_bound_Plts,
       lower_bound_RR,
       lower_bound_SBP,
       lower_bound_Ur,
       lower_bound_WCC,
       lower_bound_Weight,
       upper_bound_Alb,
       upper_bound_BNP,
       upper_bound_Cr,
       upper_bound_CRP,
       upper_bound_DBP,
       upper_bound_Glucose,
       upper_bound_Hb,
       upper_bound_Height,
       upper_bound_HR,
       upper_bound_K,
       upper_bound_Lactate,
       upper_bound_Na,
       upper_bound_O2SAT,
       upper_bound_Plts,
       upper_bound_RR,
       upper_bound_SBP,
       upper_bound_Ur,
       upper_bound_WCC,
       upper_bound_Weight,
       outlier_ind_Alb,
       outlier_ind_BNP,
       outlier_ind_Cr,
       outlier_ind_CRP,
       outlier_ind_DBP,
       outlier_ind_Glucose,
       outlier_ind_Hb,
       outlier_ind_Height,
       outlier_ind_HR,
       outlier_ind_K,
       outlier_ind_Lactate,
       outlier_ind_Na,
       outlier_ind_O2SAT,
       outlier_ind_Plts,
       outlier_ind_RR,
       outlier_ind_SBP,
       outlier_ind_Ur,
       outlier_ind_WCC,
       outlier_ind_Weight)

remove(summary_stats_long)
