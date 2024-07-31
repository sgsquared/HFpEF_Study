hc.w_results_test_tbl1 <- hc.w_results_test
# Process all variables in cluster results - need to be factors
hc.w_results_test_tbl1$GENDER <- factor(hc.w_results_test_tbl1$GENDER,
                                        levels = c(1,0),
                                        labels = c("Male",
                                                   "Female"))
Hmisc::label(hc.w_results_test_tbl1$GENDER) <- "Gender"

# https://www.datadictionary.nhs.uk/data_elements/ethnic_category.html
hc.w_results_test_tbl1$ETHNICITY <- factor(hc.w_results_test_tbl1$ETHNICITY,
                                           levels = c(1,2,3,4,5,6,7,8),
                                           labels = c("Not Provided",
                                                      "White - British",
                                                      "White - Irish",
                                                      "White - Any Other Background",
                                                      "Asian or Asian British - Indian",
                                                      "Asian or Asian British - Any Other Background",
                                                      "Black or Black British - Caribbean",
                                                      "Not Stated"))
Hmisc::label(hc.w_results_test_tbl1$ETHNICITY) <- "Ethnicity"

hc.w_results_test_tbl1$AF <- factor(hc.w_results_test_tbl1$AF,
                                    levels = c(1,0),
                                    labels = c("Present",
                                               "Absent"))
Hmisc::label(hc.w_results_test_tbl1$AF) <- "Atrial Fibrillation"

hc.w_results_test_tbl1$CAD <- factor(hc.w_results_test_tbl1$CAD,
                                     levels = c(1,0),
                                     labels = c("Present",
                                                "Absent"))
Hmisc::label(hc.w_results_test_tbl1$CAD) <- "Coronary Artery Disease"

hc.w_results_test_tbl1$Hypertension <- factor(hc.w_results_test_tbl1$Hypertension,
                                              levels = c(1,0),
                                              labels = c("Present",
                                                         "Absent"))
Hmisc::label(hc.w_results_test_tbl1$Hypertension) <- "Hypertension"

hc.w_results_test_tbl1$Heart_Failure <- factor(hc.w_results_test_tbl1$Heart_Failure,
                                               levels = c(1,0),
                                               labels = c("Present",
                                                          "Absent"))
Hmisc::label(hc.w_results_test_tbl1$Heart_Failure) <- "Heart Failure"

hc.w_results_test_tbl1$Valvular_Heart_Disease <- factor(hc.w_results_test_tbl1$Valvular_Heart_Disease,
                                                        levels = c(1,0),
                                                        labels = c("Present",
                                                                   "Absent"))
Hmisc::label(hc.w_results_test_tbl1$Valvular_Heart_Disease) <- "Valvular Heart Disease"

hc.w_results_test_tbl1$Pacemaker <- factor(hc.w_results_test_tbl1$Pacemaker,
                                           levels = c(1,0),
                                           labels = c("Present",
                                                      "Absent"))
Hmisc::label(hc.w_results_test_tbl1$Pacemaker) <- "Pacemaker"

hc.w_results_test_tbl1$CVA <- factor(hc.w_results_test_tbl1$CVA,
                                     levels = c(1,0),
                                     labels = c("Present",
                                                "Absent"))
Hmisc::label(hc.w_results_test_tbl1$CVA) <- "Cerebrovascular Accident"

hc.w_results_test_tbl1$PVD <- factor(hc.w_results_test_tbl1$PVD,
                                     levels = c(1,0),
                                     labels = c("Present",
                                                "Absent"))
Hmisc::label(hc.w_results_test_tbl1$PVD) <- "Peripheral Vascular Disease"

hc.w_results_test_tbl1$Anaemia <- factor(hc.w_results_test_tbl1$Anaemia,
                                         levels = c(1,0),
                                         labels = c("Present",
                                                    "Absent"))
Hmisc::label(hc.w_results_test_tbl1$Anaemia) <- "Anaemia"

hc.w_results_test_tbl1$Asthma <- factor(hc.w_results_test_tbl1$Asthma,
                                        levels = c(1,0),
                                        labels = c("Present",
                                                   "Absent"))
Hmisc::label(hc.w_results_test_tbl1$Asthma) <- "Asthma"

hc.w_results_test_tbl1$COPD <- factor(hc.w_results_test_tbl1$COPD,
                                      levels = c(1,0),
                                      labels = c("Present",
                                                 "Absent"))
Hmisc::label(hc.w_results_test_tbl1$COPD) <- "Chronic Obstructive Pulmonary Disease"

hc.w_results_test_tbl1$Diabetes <- factor(hc.w_results_test_tbl1$Diabetes,
                                          levels = c(1,0),
                                          labels = c("Present",
                                                     "Absent"))
Hmisc::label(hc.w_results_test_tbl1$Diabetes) <- "Diabetes"

hc.w_results_test_tbl1$CKD <- factor(hc.w_results_test_tbl1$CKD,
                                     levels = c(1,0),
                                     labels = c("Present",
                                                "Absent"))
Hmisc::label(hc.w_results_test_tbl1$CKD) <- "Chronic Kidney Disease"

hc.w_results_test_tbl1$Thyroid_Disease <- factor(hc.w_results_test_tbl1$Thyroid_Disease,
                                                 levels = c(1,0),
                                                 labels = c("Present",
                                                            "Absent"))
Hmisc::label(hc.w_results_test_tbl1$Thyroid_Disease) <- "Thyroid Dysfunction"

hc.w_results_test_tbl1$Rheumatological_Disease <- factor(hc.w_results_test_tbl1$Rheumatological_Disease,
                                                         levels = c(1,0),
                                                         labels = c("Present",
                                                                    "Absent"))
Hmisc::label(hc.w_results_test_tbl1$Rheumatological_Disease) <- "Rheumatological Disease"

hc.w_results_test_tbl1$VTE <- factor(hc.w_results_test_tbl1$VTE,
                                     levels = c(1,0),
                                     labels = c("Present",
                                                "Absent"))
Hmisc::label(hc.w_results_test_tbl1$VTE) <- "Venous Thromboembolism"

hc.w_results_test_tbl1$Mental_Health <- factor(hc.w_results_test_tbl1$Mental_Health,
                                               levels = c(1,0),
                                               labels = c("Present",
                                                          "Absent"))
Hmisc::label(hc.w_results_test_tbl1$Mental_Health) <- "Mental Health Diagnosis"

hc.w_results_test_tbl1$ACEi <- factor(hc.w_results_test_tbl1$ACEi,
                                      levels = c(1,0),
                                      labels = c("Prescribed",
                                                 "Not Prescribed"))
Hmisc::label(hc.w_results_test_tbl1$ACEi) <- "ACEi"

hc.w_results_test_tbl1$ARB <- factor(hc.w_results_test_tbl1$ARB,
                                     levels = c(1,0),
                                     labels = c("Prescribed",
                                                "Not Prescribed"))
Hmisc::label(hc.w_results_test_tbl1$ARB) <- "ARB"

hc.w_results_test_tbl1$MRA <- factor(hc.w_results_test_tbl1$MRA,
                                     levels = c(1,0),
                                     labels = c("Prescribed",
                                                "Not Prescribed"))
Hmisc::label(hc.w_results_test_tbl1$MRA) <- "MRA"

hc.w_results_test_tbl1$SacubitrilValsartan <- factor(hc.w_results_test_tbl1$SacubitrilValsartan,
                                                     levels = c(1,0),
                                                     labels = c("Prescribed",
                                                                "Not Prescribed"))
Hmisc::label(hc.w_results_test_tbl1$SacubitrilValsartan) <- "Sacubitril/Valsartan"

hc.w_results_test_tbl1$Thiazide <- factor(hc.w_results_test_tbl1$Thiazide,
                                          levels = c(1,0),
                                          labels = c("Prescribed",
                                                     "Not Prescribed"))
Hmisc::label(hc.w_results_test_tbl1$Thiazide) <- "Thiazide Diuretic"

hc.w_results_test_tbl1$LoopDiuretic <- factor(hc.w_results_test_tbl1$LoopDiuretic,
                                              levels = c(1,0),
                                              labels = c("Prescribed",
                                                         "Not Prescribed"))
Hmisc::label(hc.w_results_test_tbl1$LoopDiuretic) <- "Loop Diuretic"

hc.w_results_test_tbl1$Amiloride <- factor(hc.w_results_test_tbl1$Amiloride,
                                           levels = c(1,0),
                                           labels = c("Prescribed",
                                                      "Not Prescribed"))
Hmisc::label(hc.w_results_test_tbl1$Amiloride) <- "Amiloride"

hc.w_results_test_tbl1$BBlocker <- factor(hc.w_results_test_tbl1$BBlocker,
                                          levels = c(1,0),
                                          labels = c("Prescribed",
                                                     "Not Prescribed"))
Hmisc::label(hc.w_results_test_tbl1$BBlocker) <- "Beta-Blocker"

hc.w_results_test_tbl1$Statin <- factor(hc.w_results_test_tbl1$Statin,
                                        levels = c(1,0),
                                        labels = c("Prescribed",
                                                   "Not Prescribed"))
Hmisc::label(hc.w_results_test_tbl1$Statin) <- "Statin"

hc.w_results_test_tbl1$Nitrates <- factor(hc.w_results_test_tbl1$Nitrates,
                                          levels = c(1,0),
                                          labels = c("Prescribed",
                                                     "Not Prescribed"))
Hmisc::label(hc.w_results_test_tbl1$Nitrates) <- "Nitrates"

hc.w_results_test_tbl1$Died_During_Admission <- factor(hc.w_results_test_tbl1$Died_During_Admission,
                                                       levels = c(1,0),
                                                       labels = c("Yes",
                                                                  "No"))
Hmisc::label(hc.w_results_test_tbl1$Died_During_Admission) <- "Died During Admission"

# 6 clusters
# hc.w_results_test_tbl1$HC_Cluster <- factor(hc.w_results_test_tbl1$HC_Cluster,
#                                                 levels = c(1,2,3,4,5,6),
#                                                 labels = c("Cluster 1",
#                                                            "Cluster 2",
#                                                            "Cluster 3",
#                                                            "Cluster 4",
#                                                            "Cluster 5",
#                                                            "Cluster 6"))
# Hmisc::label(hc.w_results_test_tbl1$HC_Cluster) <- "Hierarchical Clusters"
# 3 clusters
hc.w_results_test_tbl1$Cluster <- factor(hc.w_results_test_tbl1$Cluster,
                                            levels = c(1,2,3),
                                            labels = c("Cluster 1",
                                                       "Cluster 2",
                                                       "Cluster 3"))
Hmisc::label(hc.w_results_test_tbl1$Cluster) <- "Hierarchical Clusters"

hc.w_results_test_tbl1$AGE_ADMISSION <- as.numeric(hc.w_results_test_tbl1$AGE_ADMISSION)
hc.w_results_test_tbl1$AGE_ADMISSION <- factor(hc.w_results_test_tbl1$AGE_ADMISSION,
                                               levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17),
                                               labels = c("20 - 24",
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
                                                          "100 - 104"), ordered = TRUE)
Hmisc::label(hc.w_results_test_tbl1$AGE_ADMISSION) <- "Age Category on Admission"

# 1 = Demographics, PMH, medications
test_results_table <- hc.w_results_test_tbl1[, c(4:5, 7, 13:39, 89)]

median_ordinal <- function(x) {
  levels(x)[median(as.numeric(x))]
}

# Compute median for AGE_ADMISSION
median_values_test <- hc.w_results_test_tbl1 %>%
  group_by(Cluster) %>%
  summarize(median_age = median_ordinal(AGE_ADMISSION)) %>% 
  ungroup() %>%
  pivot_wider(names_from = Cluster, values_from = median_age)
median_values_test <- as.tibble(median_values_test)

median_value_overall_test <- hc.w_results_test_tbl1 %>% 
  summarize(median_age = median_ordinal(AGE_ADMISSION))
median_value_overall_test <- as.tibble(median_value_overall_test)

test_results_table$AGE_ADMISSION <- as.numeric(test_results_table$AGE_ADMISSION)
Hmisc::label(test_results_table$AGE_ADMISSION) <- "Age Category on Admission (Median)"

# https://stackoverflow.com/questions/65665465/grouping-rows-in-gtsummary
# https://stackoverflow.com/questions/68278736/formatting-group-header-text-when-using-tbl-stack-from-gtsummary
test_results_table <- tbl_summary(test_results_table, 
                       by = Cluster,
                       percent = "column",
                       type = list(c(GENDER, AF, CAD, Hypertension, Heart_Failure, Valvular_Heart_Disease, Pacemaker, CVA,
                                     PVD, Anaemia, Asthma, COPD, Diabetes, CKD, Thyroid_Disease, Rheumatological_Disease, VTE,
                                     Mental_Health, ACEi, ARB, MRA, SacubitrilValsartan, Thiazide, LoopDiuretic, Amiloride, BBlocker,
                                     Statin, Nitrates) ~ 'dichotomous',
                                   c(ETHNICITY) ~ 'categorical',
                                   AGE_ADMISSION ~ 'continuous'),
                       value = list(GENDER = "Male",
                                    AF = "Present",
                                    CAD = "Present",
                                    Hypertension = "Present",
                                    Heart_Failure = "Present",
                                    Valvular_Heart_Disease = "Present",
                                    Pacemaker = "Present",
                                    CVA = "Present",
                                    PVD = "Present",
                                    Anaemia = "Present",
                                    Asthma = "Present",
                                    COPD = "Present",
                                    Diabetes = "Present",
                                    CKD = "Present",
                                    Thyroid_Disease = "Present",
                                    Rheumatological_Disease = "Present",
                                    VTE = "Present",
                                    Mental_Health = "Present",
                                    ACEi = "Prescribed",
                                    ARB = "Prescribed",
                                    MRA = "Prescribed",
                                    SacubitrilValsartan = "Prescribed",
                                    Thiazide = "Prescribed",
                                    LoopDiuretic = "Prescribed",
                                    Amiloride = "Prescribed",
                                    BBlocker = "Prescribed",
                                    Statin = "Prescribed",
                                    Nitrates = "Prescribed"),
                       label = list(GENDER ~ "Gender (Male)"),
                       statistic =  list(c(GENDER, AF, CAD, Hypertension, Heart_Failure, Valvular_Heart_Disease, Pacemaker, CVA,
                                           PVD, Anaemia, Asthma, COPD, Diabetes, CKD, Thyroid_Disease, Rheumatological_Disease, VTE,
                                           Mental_Health, ACEi, ARB, MRA, SacubitrilValsartan, Thiazide, LoopDiuretic, Amiloride, BBlocker,
                                           Statin, Nitrates) ~ "{p}% [{n}]",
                                         AGE_ADMISSION ~ "{median}")) %>% 
  add_p() %>% 
  add_overall() %>% 
  modify_spanning_header(c(stat_1, stat_2, stat_3) ~ "**Hierarchical Clusters**") %>% 
  modify_table_body(~ .x %>%
                      dplyr::mutate(stat_1 = ifelse(variable == "AGE_ADMISSION", median_values$`Cluster 1`, stat_1),
                                    stat_2 = ifelse(variable == "AGE_ADMISSION", median_values$`Cluster 2`, stat_2),
                                    stat_3 = ifelse(variable == "AGE_ADMISSION", median_values$`Cluster 3`, stat_3),
                                    # stat_4 = ifelse(variable == "AGE_ADMISSION", median_values$`Cluster 4`, stat_4),
                                    # stat_5 = ifelse(variable == "AGE_ADMISSION", median_values$`Cluster 5`, stat_5),
                                    # stat_6 = ifelse(variable == "AGE_ADMISSION", median_values$`Cluster 6`, stat_6),
                                    stat_0 = ifelse(variable == "AGE_ADMISSION", median_value_overall, stat_0)
                      )) %>% 
  modify_table_body(mutate,
                    groupname_col = case_when(variable %in% c("AGE_ADMISSION", "GENDER", "ETHNICITY") ~ "Demographics",
                                              variable %in% c("AF", "CAD", "Hypertension", "Heart_Failure", "Valvular_Heart_Disease", "Pacemaker", "CVA",
                                                              "PVD", "Anaemia", "Asthma", "COPD", "Diabetes", "CKD", "Thyroid_Disease", "Rheumatological_Disease", "VTE",
                                                              "Mental_Health") ~ "Comorbidities",
                                              variable %in% c("ACEi", "ARB", "MRA", "SacubitrilValsartan", "Thiazide", "LoopDiuretic", "Amiloride", "BBlocker",
                                                              "Statin", "Nitrates") ~ "Medication")
  ) %>% 
  as_gt() %>%
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_row_groups(groups = everything())
  )

test_results_table
gt::gtsave(test_results_table, "Test_Characteristics.html")

hc.w_results_test_tbl1$ITU_Admission_YorN <- factor(hc.w_results_test_tbl1$ITU_Admission_YorN,
                                                    levels = c(1,0),
                                                    labels = c("Yes",
                                                               "No"))
Hmisc::label(hc.w_results_test_tbl1$ITU_Admission_YorN) <- "ITU Admission"

# 2 = Outcomes - RR, Weight, Fluid Balance, LoS, Number of Admissions, I/P death
test_results_table_outcomes <- hc.w_results_test_tbl1[, c(77, 79, 82, 83, 84, 85, 86, 87, 88, 89)]

test_results_table_outcomes <- tbl_summary(test_results_table_outcomes, 
                       by = Cluster,
                       percent = "column",
                       type = list(FLUID_BALANCE ~ 'continuous2',
                                   NUMBER_OF_ADMISSIONS_IN_1YR ~ 'continuous2',
                                   Length_of_Stay ~ 'continuous2',
                                   RR_Diff_24HR ~ 'continuous2',
                                   RR_Diff_Disch ~ 'continuous2',
                                   Weight_Diff_24HR ~ 'continuous2',
                                   Weight_Diff_Disch ~ 'continuous2',
                                   ITU_Admission_YorN ~ 'categorical',
                                   Died_During_Admission ~ 'categorical'),
                       value = list(ITU_Admission_YorN = "Yes",
                                    Died_During_Admission = "Yes"),
                       label = list(FLUID_BALANCE ~ "Fluid Balance Across Admission",
                                    NUMBER_OF_ADMISSIONS_IN_1YR ~ "Number of Admissions in 1 Year",
                                    Length_of_Stay ~ "Length of Stay",
                                    RR_Diff_24HR ~ "Change in RR at 24 Hours",
                                    RR_Diff_Disch ~ "Change in RR at Discharge",
                                    Weight_Diff_24HR ~ "Change in Weight at 24 Hours",
                                    Weight_Diff_Disch ~ "Change in Weight at Discharge",
                                    Died_During_Admission ~ "Died During Admission",
                                    ITU_Admission_YorN ~ "ITU Admission"),
                       statistic =  list(c(ITU_Admission_YorN, Died_During_Admission) ~ "{p}% [{n}]",
                                         c(FLUID_BALANCE, NUMBER_OF_ADMISSIONS_IN_1YR, Length_of_Stay,
                                           RR_Diff_24HR, RR_Diff_Disch, Weight_Diff_24HR, Weight_Diff_Disch) ~ c("{mean} \u00b1 {sd}",
                                                                                                                 "{median} [{p25}, {p75}]"))) %>% 
  add_p() %>% 
  modify_spanning_header(c(stat_1, stat_2, stat_3) ~ "**Hierarchical Clusters**")

test_results_table_outcomes
gt::gtsave(as_gt(test_results_table_outcomes), "Test_Outcomes.html")

# 3 = Outcomes - Observations, Laboratory Tests
test_results_table_obs <- hc.w_results_test_tbl1[, c(40:58, 89)]

test_results_table_obs <- tbl_summary(test_results_table_obs, 
                       by = Cluster,
                       percent = "column",
                       type = list(c(O2SAT, RR, SBP, DBP, HR, Weight, Height, WCC, Hb, Plts, Na, K, Cr, Ur, CRP, Alb, Glucose, Lactate, BNP) ~ 'continuous2'),
                       label = list(O2SAT ~ "Oxygen Saturations",
                                    RR ~ "Respiratory Rate",
                                    SBP ~ "Systolic Blood Pressure",
                                    DBP ~ "Diastolic Blood Pressure",
                                    HR ~ "Heart Rate",
                                    Weight ~ "Weight",
                                    Height ~ "Height",
                                    WCC ~ "White Cell Count",
                                    Hb ~ "Haemoglobin",
                                    Plts ~ "Platelets",
                                    Na ~ "Sodium",
                                    K ~ "Potassium",
                                    Cr ~ "Creatinine",
                                    Ur ~ "Urea",
                                    CRP ~ "C-Reactive Protein",
                                    Alb ~ "Albumin",
                                    Glucose ~ "Glucose",
                                    Lactate ~ "Lactate",
                                    BNP ~ "Brain Natriuretic Peptide"),
                       statistic =  list(c(O2SAT, RR, SBP, DBP, HR, Weight, Height, WCC, Hb, Plts, Na, K, Cr, Ur, CRP, Alb, Glucose, Lactate, BNP) ~ c("{mean} \u00b1 {sd}",
                                                                                                                                                       "{median} [{p25}, {p75}]"))) %>% 
  add_p() %>% 
  add_overall %>% 
  modify_spanning_header(c(stat_1, stat_2, stat_3) ~ "**Hierarchical Clusters**") %>% 
  modify_table_body(mutate,
                    groupname_col = case_when(variable %in% c("O2SAT", "RR", "SBP", "DBP", "HR", "Weight", "Height") ~ "Admission Observations",
                                              variable %in% c("WCC", "Hb", "Plts", "Na", "K", "Cr", "Ur", "CRP", "Alb", "Glucose", "Lactate", "BNP") ~ "Admission Blood Tests")
  ) %>% 
  as_gt() %>%
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_row_groups(groups = everything())
  )


test_results_table_obs
gt::gtsave(test_results_table_obs, "Test_obsandbloods.html")
