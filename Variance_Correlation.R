### Checking data quality - distribution, collinearity

## Check distribution
# qqplot
qqo2sat <- qqPlot(imputed_dataset$O2SAT)
qqrr <- qqPlot(imputed_dataset$RR)
qqsbp <- qqPlot(imputed_dataset$SBP)
qqdbp <- qqPlot(imputed_dataset$DBP)
qqhr <- qqPlot(imputed_dataset$HR)
qqweight <- qqPlot(imputed_dataset$Weight)
qqheight <- qqPlot(imputed_dataset$Height)
qqwcc <- qqPlot(imputed_dataset$WCC)
qqhb <- qqPlot(imputed_dataset$Hb)
qqplts <- qqPlot(imputed_dataset$Plts)
qqna <- qqPlot(imputed_dataset$Na)
qqk <- qqPlot(imputed_dataset$K)
qqcr <- qqPlot(imputed_dataset$Cr)
qqur <- qqPlot(imputed_dataset$Ur)
qqcrp <- qqPlot(imputed_dataset$CRP)
qqalb <- qqPlot(imputed_dataset$Alb)
qqglucose <- qqPlot(imputed_dataset$Glucose)
qqlactate <- qqPlot(imputed_dataset$Lactate)
qqbnp <- qqPlot(imputed_dataset$BNP)

qq_o2sat <- ggqqplot(imputed_dataset$O2SAT, title = "Oxygen Saturations", ylab = "", xlab = "", ggtheme = theme_minimal()) +
  theme(plot.title = element_text(hjust = 0.5))
qq_rr <- ggqqplot(imputed_dataset$RR, title = "Respiratory Rate", ylab = "", xlab = "", ggtheme = theme_minimal()) +
  theme(plot.title = element_text(hjust = 0.5))
qq_sbp <- ggqqplot(imputed_dataset$SBP, title = "Systolic Blood Pressure", ylab = "", xlab = "", ggtheme = theme_minimal()) +
  theme(plot.title = element_text(hjust = 0.5))
qq_dbp <- ggqqplot(imputed_dataset$DBP, title = "Diastolic Blood Pressure", ylab = "", xlab = "", ggtheme = theme_minimal()) +
  theme(plot.title = element_text(hjust = 0.5))
qq_hr <- ggqqplot(imputed_dataset$HR, title = "Heart Rate", ylab = "", xlab = "", ggtheme = theme_minimal()) +
  theme(plot.title = element_text(hjust = 0.5))
qq_weight <- ggqqplot(imputed_dataset$Weight, title = "Weight", ylab = "", xlab = "", ggtheme = theme_minimal()) +
  theme(plot.title = element_text(hjust = 0.5))
qq_height <- ggqqplot(imputed_dataset$Height, title = "Height", ylab = "", xlab = "", ggtheme = theme_minimal()) +
  theme(plot.title = element_text(hjust = 0.5))
qq_wcc <- ggqqplot(imputed_dataset$WCC, title = "White Cell Count", ylab = "", xlab = "", ggtheme = theme_minimal()) +
  theme(plot.title = element_text(hjust = 0.5))
qq_hb <- ggqqplot(imputed_dataset$Hb, title = "Haemoglobin", ylab = "", xlab = "", ggtheme = theme_minimal()) +
  theme(plot.title = element_text(hjust = 0.5))
qq_plts <- ggqqplot(imputed_dataset$Plts, title = "Platelet Count", ylab = "", xlab = "", ggtheme = theme_minimal()) +
  theme(plot.title = element_text(hjust = 0.5))
qq_na <- ggqqplot(imputed_dataset$Na, title = "Sodium", ylab = "", xlab = "", ggtheme = theme_minimal()) +
  theme(plot.title = element_text(hjust = 0.5))
qq_k <- ggqqplot(imputed_dataset$K, title = "Potassium", ylab = "", xlab = "", ggtheme = theme_minimal()) +
  theme(plot.title = element_text(hjust = 0.5))
qq_cr <- ggqqplot(imputed_dataset$Cr, title = "Creatinine", ylab = "", xlab = "", ggtheme = theme_minimal()) +
  theme(plot.title = element_text(hjust = 0.5))
qq_ur <- ggqqplot(imputed_dataset$Ur, title = "Urea", ylab = "", xlab = "", ggtheme = theme_minimal()) +
  theme(plot.title = element_text(hjust = 0.5))
qq_crp <- ggqqplot(imputed_dataset$CRP, title = "C-Reactive Protein", ylab = "", xlab = "", ggtheme = theme_minimal()) +
  theme(plot.title = element_text(hjust = 0.5))
qq_alb <- ggqqplot(imputed_dataset$Alb, title = "Albumin", ylab = "", xlab = "", ggtheme = theme_minimal()) +
  theme(plot.title = element_text(hjust = 0.5))
qq_glucose <- ggqqplot(imputed_dataset$Glucose, title = "Glucose", ylab = "", xlab = "", ggtheme = theme_minimal()) +
  theme(plot.title = element_text(hjust = 0.5))
qq_lactate <- ggqqplot(imputed_dataset$Lactate, title = "Lactate", ylab = "", xlab = "", ggtheme = theme_minimal()) +
  theme(plot.title = element_text(hjust = 0.5))
qq_bnp <- ggqqplot(imputed_dataset$BNP, title = "B-Natriuretic Peptide", ylab = "", xlab = "", ggtheme = theme_minimal()) +
  theme(plot.title = element_text(hjust = 0.5))


grid.arrange(qq_bnp, qq_lactate, qq_glucose, qq_alb, qq_crp, qq_ur, qq_cr, qq_k, 
             qq_na, qq_plts, qq_hb, qq_wcc, qq_weight, qq_hr, qq_dbp, qq_rr, 
             qq_o2sat, ncol = 4)

# Change labels
Hmisc::label(imputed_dataset$GENDER) <- "Gender"
Hmisc::label(imputed_dataset$ETHNICITY) <- "Ethnicity"
Hmisc::label(imputed_dataset$AF) <- "Atrial Fibrillation"
Hmisc::label(imputed_dataset$CAD) <- "Coronary Artery Disease"
Hmisc::label(imputed_dataset$Hypertension) <- "Hypertension"
Hmisc::label(imputed_dataset$Heart_Failure) <- "Heart Failure"
Hmisc::label(imputed_dataset$Valvular_Heart_Disease) <- "Valvular Heart Disease"
Hmisc::label(imputed_dataset$Pacemaker) <- "Pacemaker"
Hmisc::label(imputed_dataset$CVA) <- "Cerebrovascular Accident"
Hmisc::label(imputed_dataset$PVD) <- "Peripheral Vascular Disease"
Hmisc::label(imputed_dataset$Anaemia) <- "Anaemia"
Hmisc::label(imputed_dataset$Asthma) <- "Asthma"
Hmisc::label(imputed_dataset$COPD) <- "Chronic Obstructive Pulmonary Disease"
Hmisc::label(imputed_dataset$Diabetes) <- "Diabetes"
Hmisc::label(imputed_dataset$CKD) <- "Chronic Kidney Disease"
Hmisc::label(imputed_dataset$Thyroid_Disease) <- "Thyroid Dysfunction"
Hmisc::label(imputed_dataset$Rheumatological_Disease) <- "Rheumatological Disease"
Hmisc::label(imputed_dataset$VTE) <- "Venous Thromboembolism"
Hmisc::label(imputed_dataset$Mental_Health) <- "Mental Health Diagnosis"
Hmisc::label(imputed_dataset$ACEi) <- "ACEi"
Hmisc::label(imputed_dataset$ARB) <- "ARB"
Hmisc::label(imputed_dataset$MRA) <- "MRA"
Hmisc::label(imputed_dataset$SacubitrilValsartan) <- "Sacubitril/Valsartan"
Hmisc::label(imputed_dataset$Thiazide) <- "Thiazide Diuretic"
Hmisc::label(imputed_dataset$LoopDiuretic) <- "Loop Diuretic"
Hmisc::label(imputed_dataset$Amiloride) <- "Amiloride"
Hmisc::label(imputed_dataset$BBlocker) <- "Beta-Blocker"
Hmisc::label(imputed_dataset$Statin) <- "Statin"
Hmisc::label(imputed_dataset$Nitrates) <- "Nitrates"
Hmisc::label(imputed_dataset$AGE_ADMISSION) <- "Age Category on Admission"

Hmisc::label(imputed_dataset$O2SAT) <- "Oxygen Saturations"
Hmisc::label(imputed_dataset$RR) <- "Respiratory Rate"
Hmisc::label(imputed_dataset$SBP) <- "Systolic Blood Pressure"
Hmisc::label(imputed_dataset$DBP) <- "Diastolic Blood Pressure"
Hmisc::label(imputed_dataset$HR) <- "Heart Rate"
Hmisc::label(imputed_dataset$Weight) <- "Weight"
Hmisc::label(imputed_dataset$Height) <- "Height"
Hmisc::label(imputed_dataset$WCC) <- "White Cell Count"
Hmisc::label(imputed_dataset$Hb) <- "Haemoglobin"
Hmisc::label(imputed_dataset$Plts) <- "Platelet Count"
Hmisc::label(imputed_dataset$Na) <- "Sodium"
Hmisc::label(imputed_dataset$K) <- "Potassium"
Hmisc::label(imputed_dataset$Cr) <- "Creatinine"
Hmisc::label(imputed_dataset$Ur) <- "Urea"
Hmisc::label(imputed_dataset$CRP) <- "C-Reactive Protein"
Hmisc::label(imputed_dataset$Alb) <- "Albumin"
Hmisc::label(imputed_dataset$Glucose) <- "Glucose"
Hmisc::label(imputed_dataset$Lactate) <- "Lactate"
Hmisc::label(imputed_dataset$BNP) <- "B-Natriuretic Peptide"

## Visualise/summarise data
# Viusalise all continuous variables and check for outliers - blood tests, observations
# Histogram for Oxygen Saturations
hist_o2sat <- ggplot(imputed_dataset, aes(x = O2SAT)) +
  geom_histogram(fill = "salmon", color = "black", bins = 50) +
  labs(title = label(imputed_dataset$O2SAT), y = "", x = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram for Respiratory Rate
hist_rr <- ggplot(imputed_dataset, aes(x = RR)) +
  geom_histogram(fill = "salmon", color = "black", bins = 50) +
  labs(title = label(imputed_dataset$RR), y = "", x = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram for Systolic Blood Pressure
hist_sbp <- ggplot(imputed_dataset, aes(x = SBP)) +
  geom_histogram(fill = "salmon", color = "black", bins = 50) +
  labs(title = label(imputed_dataset$SBP), y = "", x = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram for Diastolic Blood Pressure
hist_dbp <- ggplot(imputed_dataset, aes(x = DBP)) +
  geom_histogram(fill = "salmon", color = "black", bins = 50) +
  labs(title = label(imputed_dataset$DBP), y = "", x = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram for Heart Rate
hist_hr <- ggplot(imputed_dataset, aes(x = HR)) +
  geom_histogram(fill = "salmon", color = "black", bins = 50) +
  labs(title = label(imputed_dataset$HR), y = "", x = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram for Weight
hist_weight <- ggplot(imputed_dataset, aes(x = Weight)) +
  geom_histogram(fill = "salmon", color = "black", bins = 50) +
  labs(title = label(imputed_dataset$Weight), y = "", x = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram for Height
hist_height <- ggplot(imputed_dataset, aes(x = Height)) +
  geom_histogram(fill = "salmon", color = "black", bins = 50) +
  labs(title = label(imputed_dataset$Height), y = "", x = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram for White Cell Count
hist_wcc <- ggplot(imputed_dataset, aes(x = WCC)) +
  geom_histogram(fill = "salmon", color = "black", bins = 50) +
  labs(title = label(imputed_dataset$WCC), y = "", x = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram for Haemoglobin
hist_hb <- ggplot(imputed_dataset, aes(x = Hb)) +
  geom_histogram(fill = "salmon", color = "black", bins = 50) +
  labs(title = label(imputed_dataset$Hb), y = "", x = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram for Platelet Count
hist_plts <- ggplot(imputed_dataset, aes(x = Plts)) +
  geom_histogram(fill = "salmon", color = "black", bins = 50) +
  labs(title = label(imputed_dataset$Plts), y = "", x = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram for Sodium
hist_na <- ggplot(imputed_dataset, aes(x = Na)) +
  geom_histogram(fill = "salmon", color = "black", bins = 50) +
  labs(title = label(imputed_dataset$Na), y = "", x = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram for Potassium
hist_k <- ggplot(imputed_dataset, aes(x = K)) +
  geom_histogram(fill = "salmon", color = "black", bins = 50) +
  labs(title = label(imputed_dataset$K), y = "", x = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram for Creatinine
hist_cr <- ggplot(imputed_dataset, aes(x = Cr)) +
  geom_histogram(fill = "salmon", color = "black", bins = 50) +
  labs(title = label(imputed_dataset$Cr), y = "", x = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram for Urea
hist_ur <- ggplot(imputed_dataset, aes(x = Ur)) +
  geom_histogram(fill = "salmon", color = "black", bins = 50) +
  labs(title = label(imputed_dataset$Ur), y = "", x = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram for C-Reactive Protein
hist_crp <- ggplot(imputed_dataset, aes(x = CRP)) +
  geom_histogram(fill = "salmon", color = "black", bins = 50) +
  labs(title = label(imputed_dataset$CRP), y = "", x = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram for Albumin
hist_alb <- ggplot(imputed_dataset, aes(x = Alb)) +
  geom_histogram(fill = "salmon", color = "black", bins = 50) +
  labs(title = label(imputed_dataset$Alb), y = "", x = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram for Glucose
hist_glucose <- ggplot(imputed_dataset, aes(x = Glucose)) +
  geom_histogram(fill = "salmon", color = "black", bins = 50) +
  labs(title = label(imputed_dataset$Glucose), y = "", x = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram for Lactate
hist_lactate <- ggplot(imputed_dataset, aes(x = Lactate)) +
  geom_histogram(fill = "salmon", color = "black", bins = 50) +
  labs(title = label(imputed_dataset$Lactate), y = "", x = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram for B-Natriuretic Peptide
hist_bnp <- ggplot(imputed_dataset, aes(x = BNP)) +
  geom_histogram(fill = "salmon", color = "black", bins = 50) +
  labs(title = label(imputed_dataset$BNP), y = "", x = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Shapiro-Wilk
shapiro.test(imputed_dataset$O2SAT)
shapiro.test(imputed_dataset$RR)
shapiro.test(imputed_dataset$SBP)
shapiro.test(imputed_dataset$DBP)
shapiro.test(imputed_dataset$HR)
shapiro.test(imputed_dataset$Weight)
shapiro.test(imputed_dataset$Height)
shapiro.test(imputed_dataset$WCC)
shapiro.test(imputed_dataset$Hb)
shapiro.test(imputed_dataset$Plts)
shapiro.test(imputed_dataset$Na)
shapiro.test(imputed_dataset$K)
shapiro.test(imputed_dataset$Cr)
shapiro.test(imputed_dataset$Ur)
shapiro.test(imputed_dataset$CRP)
shapiro.test(imputed_dataset$Alb)
shapiro.test(imputed_dataset$Glucose)
shapiro.test(imputed_dataset$Lactate)
shapiro.test(imputed_dataset$BNP)

# Plts and Hb are the only variables which meet SW test for normality
# In summary report all as median [IQR]

# Drop columns with no positive values
summary(imputed_dataset)
imputed_dataset <- imputed_dataset %>% 
  dplyr::select(-Smoker, -OSA, -OHS, -Obesity, -Inflammatory_Bowel_Disease, -Malignancy, -SGLT2i, -Ivabradine, -Hydralazine)

# Encode ordinal variables
# Age at admission
imputed_dataset <- imputed_dataset %>%
  mutate(AGE_ADMISSION = as.numeric(factor(AGE_ADMISSION, levels = c("20 - 24",
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
                                                                             "100 - 104"), ordered = TRUE)))

imputed_dataset <- imputed_dataset %>%
  mutate(GENDER = as.numeric(factor(GENDER)))

# NULL is encoded as 1 - perhaps NA has some relevance
imputed_dataset <- imputed_dataset %>%
  mutate(ETHNICITY = as.numeric(factor(ETHNICITY)))

# Cannot include admission number as no censorship date
# imputed_dataset <- imputed_dataset %>%
#   mutate(ADMISSION_NUMBER = as.numeric(ADMISSION_NUMBER))

## Correlation and collinearity of data
# Correlation matrix
correlated_columns <- imputed_dataset %>% 
  dplyr::select(AGE_ADMISSION, ETHNICITY, GENDER, AF:BNP)

# Treating boolean as numerical data fields
# Generate correlation matrix
correlation_matrix <- cor(correlated_columns, method = "pearson")
correlation_dataframe <- as.data.frame(correlation_matrix)

Hmisc::label(correlated_columns$GENDER) <- "Gender"
Hmisc::label(correlated_columns$ETHNICITY) <- "Ethnicity"
Hmisc::label(correlated_columns$AGE_ADMISSION) <- "Age Category on Admission"

# Extract labels and set them as the row and column names of the correlation matrix
labels <- sapply(correlated_columns, label)
rownames(correlation_matrix) <- labels
colnames(correlation_matrix) <- labels

# generate correlation plot
corrplot(correlation_matrix, method = "color", type = "lower", tl.cex = 0.9, tl.col = "black", mar = c(0, 0, 0, 0))

# list correlations over 0.6
correlation_dataframe <- correlation_dataframe %>%
  rownames_to_column()
sig_cor_list <- correlation_dataframe %>% 
  gather(-rowname, key = "colname", value = "cor") %>% 
  filter(rowname != colname, abs(cor) > 0.6)
print(sig_cor_list)

# Need to use a variable selection method to choose between:-
# - Height
# - Gender
# - SBP
# - DBP

### Create panel of histograms and qqplots for final variables
# [1] "ETHNICITY"     "GENDER"        "AGE_ADMISSION" "Nitrates"      "Statin"        "BBlocker"      "LoopDiuretic"  "ACEi"          "Hypertension"  "AF"            "BNP"           "Lactate"       "Glucose"      
# [14] "Alb"           "CRP"           "Ur"            "Cr"            "K"             "Na"            "Plts"          "Hb"            "WCC"           "Weight"        "HR"            "DBP"           "RR"           
# [27] "O2SAT"  
# Continuous = BNP, lactate, glucose, albumin, CRP, urea, creatinine, potassium, sodium, platelets, haemoglobin, WCC, weight, HR, DBP, RR, O2sat

grid.arrange(hist_bnp, hist_lactate, hist_glucose, hist_alb, hist_crp, hist_ur, hist_cr, hist_k, 
             hist_na, hist_plts, hist_hb, hist_wcc, hist_weight, hist_hr, hist_dbp, hist_rr, 
             hist_o2sat, ncol = 4)


remove(sig_cor_list, correlation_matrix)
remove(correlated_columns, correlation_dataframe, final_dataset)

