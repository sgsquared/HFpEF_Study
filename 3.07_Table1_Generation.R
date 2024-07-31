# Generating Table 1

cluster_results_table1 <- cluster_results
cluster_results_table1 <- ungroup(cluster_results_table1)

####################################### Hierarchical Clustering ##############################################
# Process all variables in cluster results - need to be factors
cluster_results_table1$GENDER <- factor(cluster_results_table1$GENDER,
                                   levels = c(1,0),
                                   labels = c("Male",
                                              "Female"))
Hmisc::label(cluster_results_table1$GENDER) <- "Gender"

# https://www.datadictionary.nhs.uk/data_elements/ethnic_category.html
cluster_results_table1$ETHNICITY <- factor(cluster_results_table1$ETHNICITY,
                                 levels = c(1,2,3,4,5,6,7,8),
                                 labels = c("Not Provided",
                                            "White - British",
                                            "White - Irish",
                                            "White - Any Other Background",
                                            "Asian or Asian British - Indian",
                                            "Asian or Asian British - Any Other Background",
                                            "Black or Black British - Caribbean",
                                            "Not Stated"))
Hmisc::label(cluster_results_table1$ETHNICITY) <- "Ethnicity"

cluster_results_table1$AF <- factor(cluster_results_table1$AF,
                                 levels = c(1,0),
                                 labels = c("Present",
                                            "Absent"))
Hmisc::label(cluster_results_table1$AF) <- "Atrial Fibrillation"

cluster_results_table1$CAD <- factor(cluster_results_table1$CAD,
                             levels = c(1,0),
                             labels = c("Present",
                                        "Absent"))
Hmisc::label(cluster_results_table1$CAD) <- "Coronary Artery Disease"

cluster_results_table1$Hypertension <- factor(cluster_results_table1$Hypertension,
                             levels = c(1,0),
                             labels = c("Present",
                                        "Absent"))
Hmisc::label(cluster_results_table1$Hypertension) <- "Hypertension"

cluster_results_table1$Heart_Failure <- factor(cluster_results_table1$Heart_Failure,
                             levels = c(1,0),
                             labels = c("Present",
                                        "Absent"))
Hmisc::label(cluster_results_table1$Heart_Failure) <- "Heart Failure"

cluster_results_table1$Valvular_Heart_Disease <- factor(cluster_results_table1$Valvular_Heart_Disease,
                             levels = c(1,0),
                             labels = c("Present",
                                        "Absent"))
Hmisc::label(cluster_results_table1$Valvular_Heart_Disease) <- "Valvular Heart Disease"

cluster_results_table1$Pacemaker <- factor(cluster_results_table1$Pacemaker,
                             levels = c(1,0),
                             labels = c("Present",
                                        "Absent"))
Hmisc::label(cluster_results_table1$Pacemaker) <- "Pacemaker"

cluster_results_table1$CVA <- factor(cluster_results_table1$CVA,
                             levels = c(1,0),
                             labels = c("Present",
                                        "Absent"))
Hmisc::label(cluster_results_table1$CVA) <- "Cerebrovascular Accident"

cluster_results_table1$PVD <- factor(cluster_results_table1$PVD,
                             levels = c(1,0),
                             labels = c("Present",
                                        "Absent"))
Hmisc::label(cluster_results_table1$PVD) <- "Peripheral Vascular Disease"

cluster_results_table1$Anaemia <- factor(cluster_results_table1$Anaemia,
                             levels = c(1,0),
                             labels = c("Present",
                                        "Absent"))
Hmisc::label(cluster_results_table1$Anaemia) <- "Anaemia"

cluster_results_table1$Asthma <- factor(cluster_results_table1$Asthma,
                             levels = c(1,0),
                             labels = c("Present",
                                        "Absent"))
Hmisc::label(cluster_results_table1$Asthma) <- "Asthma"

cluster_results_table1$COPD <- factor(cluster_results_table1$COPD,
                             levels = c(1,0),
                             labels = c("Present",
                                        "Absent"))
Hmisc::label(cluster_results_table1$COPD) <- "Chronic Obstructive Pulmonary Disease"

cluster_results_table1$Diabetes <- factor(cluster_results_table1$Diabetes,
                             levels = c(1,0),
                             labels = c("Present",
                                        "Absent"))
Hmisc::label(cluster_results_table1$Diabetes) <- "Diabetes"

cluster_results_table1$CKD <- factor(cluster_results_table1$CKD,
                             levels = c(1,0),
                             labels = c("Present",
                                        "Absent"))
Hmisc::label(cluster_results_table1$CKD) <- "Chronic Kidney Disease"

cluster_results_table1$Thyroid_Disease <- factor(cluster_results_table1$Thyroid_Disease,
                             levels = c(1,0),
                             labels = c("Present",
                                        "Absent"))
Hmisc::label(cluster_results_table1$Thyroid_Disease) <- "Thyroid Dysfunction"

cluster_results_table1$Rheumatological_Disease <- factor(cluster_results_table1$Rheumatological_Disease,
                             levels = c(1,0),
                             labels = c("Present",
                                        "Absent"))
Hmisc::label(cluster_results_table1$Rheumatological_Disease) <- "Rheumatological Disease"

cluster_results_table1$VTE <- factor(cluster_results_table1$VTE,
                             levels = c(1,0),
                             labels = c("Present",
                                        "Absent"))
Hmisc::label(cluster_results_table1$VTE) <- "Venous Thromboembolism"

cluster_results_table1$Mental_Health <- factor(cluster_results_table1$Mental_Health,
                             levels = c(1,0),
                             labels = c("Present",
                                        "Absent"))
Hmisc::label(cluster_results_table1$Mental_Health) <- "Mental Health Diagnosis"

cluster_results_table1$ACEi <- factor(cluster_results_table1$ACEi,
                             levels = c(1,0),
                             labels = c("Prescribed",
                                        "Not Prescribed"))
Hmisc::label(cluster_results_table1$ACEi) <- "ACEi"

cluster_results_table1$ARB <- factor(cluster_results_table1$ARB,
                               levels = c(1,0),
                               labels = c("Prescribed",
                                          "Not Prescribed"))
Hmisc::label(cluster_results_table1$ARB) <- "ARB"

cluster_results_table1$MRA <- factor(cluster_results_table1$MRA,
                               levels = c(1,0),
                               labels = c("Prescribed",
                                          "Not Prescribed"))
Hmisc::label(cluster_results_table1$MRA) <- "MRA"

cluster_results_table1$SacubitrilValsartan <- factor(cluster_results_table1$SacubitrilValsartan,
                               levels = c(1,0),
                               labels = c("Prescribed",
                                          "Not Prescribed"))
Hmisc::label(cluster_results_table1$SacubitrilValsartan) <- "Sacubitril/Valsartan"

cluster_results_table1$Thiazide <- factor(cluster_results_table1$Thiazide,
                               levels = c(1,0),
                               labels = c("Prescribed",
                                          "Not Prescribed"))
Hmisc::label(cluster_results_table1$Thiazide) <- "Thiazide Diuretic"

cluster_results_table1$LoopDiuretic <- factor(cluster_results_table1$LoopDiuretic,
                               levels = c(1,0),
                               labels = c("Prescribed",
                                          "Not Prescribed"))
Hmisc::label(cluster_results_table1$LoopDiuretic) <- "Loop Diuretic"

cluster_results_table1$Amiloride <- factor(cluster_results_table1$Amiloride,
                               levels = c(1,0),
                               labels = c("Prescribed",
                                          "Not Prescribed"))
Hmisc::label(cluster_results_table1$Amiloride) <- "Amiloride"

cluster_results_table1$BBlocker <- factor(cluster_results_table1$BBlocker,
                               levels = c(1,0),
                               labels = c("Prescribed",
                                          "Not Prescribed"))
Hmisc::label(cluster_results_table1$BBlocker) <- "Beta-Blocker"

cluster_results_table1$Statin <- factor(cluster_results_table1$Statin,
                               levels = c(1,0),
                               labels = c("Prescribed",
                                          "Not Prescribed"))
Hmisc::label(cluster_results_table1$Statin) <- "Statin"

cluster_results_table1$Nitrates <- factor(cluster_results_table1$Nitrates,
                               levels = c(1,0),
                               labels = c("Prescribed",
                                          "Not Prescribed"))
Hmisc::label(cluster_results_table1$Nitrates) <- "Nitrates"

cluster_results_table1$Died_During_Admission <- factor(cluster_results_table1$Died_During_Admission,
                                   levels = c(1,0),
                                   labels = c("Yes",
                                              "No"))
Hmisc::label(cluster_results_table1$Died_During_Admission) <- "Died During Admission"

# 6 clusters
# cluster_results_table1$HC_Cluster <- factor(cluster_results_table1$HC_Cluster,
#                                                 levels = c(1,2,3,4,5,6),
#                                                 labels = c("Cluster 1",
#                                                            "Cluster 2",
#                                                            "Cluster 3",
#                                                            "Cluster 4",
#                                                            "Cluster 5",
#                                                            "Cluster 6"))
# Hmisc::label(cluster_results_table1$HC_Cluster) <- "Hierarchical Clusters"
# 3 clusters
cluster_results_table1$HC_Cluster <- factor(cluster_results_table1$HC_Cluster,
                                            levels = c(1,2,3),
                                            labels = c("Cluster 1",
                                                       "Cluster 2",
                                                       "Cluster 3"))
Hmisc::label(cluster_results_table1$HC_Cluster) <- "Hierarchical Clusters"

cluster_results_table1$AGE_ADMISSION <- as.numeric(cluster_results_table1$AGE_ADMISSION)
cluster_results_table1$AGE_ADMISSION <- factor(cluster_results_table1$AGE_ADMISSION,
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
Hmisc::label(cluster_results_table1$AGE_ADMISSION) <- "Age Category on Admission"

# 1 = Demographics, PMH, medications
hc_tbl1 <- cluster_results_table1[, c(4:5, 7, 13:39, 89)]

median_ordinal <- function(x) {
  levels(x)[median(as.numeric(x))]
}

# Compute median for AGE_ADMISSION
median_values <- cluster_results_table1 %>%
 group_by(HC_Cluster) %>%
 summarize(median_age = median_ordinal(AGE_ADMISSION)) %>% 
 ungroup() %>%
 pivot_wider(names_from = HC_Cluster, values_from = median_age)
median_values <- as.tibble(median_values)

median_value_overall <- cluster_results_table1 %>% 
  summarize(median_age = median_ordinal(AGE_ADMISSION))
median_value_overall <- as.tibble(median_value_overall)

hc_tbl1$AGE_ADMISSION <- as.numeric(hc_tbl1$AGE_ADMISSION)
Hmisc::label(hc_tbl1$AGE_ADMISSION) <- "Age Category on Admission (Median)"

# https://stackoverflow.com/questions/65665465/grouping-rows-in-gtsummary
# https://stackoverflow.com/questions/68278736/formatting-group-header-text-when-using-tbl-stack-from-gtsummary
hc_tbl1 <- tbl_summary(hc_tbl1, 
                   by = HC_Cluster,
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

hc_tbl1
# gt::gtsave(hc_tbl1, "HC_characteristics.html")

##############################################################################################################################
cluster_results_table1$ITU_Admission_YorN <- factor(cluster_results_table1$ITU_Admission_YorN,
                                                levels = c(1,0),
                                                labels = c("Yes",
                                                           "No"))
Hmisc::label(cluster_results_table1$ITU_Admission_YorN) <- "ITU Admission"

# 2 = Outcomes - RR, Weight, Fluid Balance, LoS, Number of Admissions, I/P death
hc_tbl2 <- cluster_results_table1[, c(77, 79, 82, 83, 84, 85, 86, 87, 88, 89)]

hc_tbl2 <- tbl_summary(hc_tbl2, 
                       by = HC_Cluster,
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

hc_tbl2
# gt::gtsave(as_gt(hc_tbl2), "HC_outcomes.html")

# 3 = Outcomes - Observations, Laboratory Tests
hc_tbl3 <- cluster_results_table1[, c(40:58, 89)]

hc_tbl3 <- tbl_summary(hc_tbl3, 
                       by = HC_Cluster,
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
  

hc_tbl3
# gt::gtsave(hc_tbl3, "HC_obsandbloods.html")

################################################ PAM #########################################################
cluster_results_table1$PAM_Cluster <- factor(cluster_results_table1$PAM_Cluster,
                                            levels = c(1,2,3),
                                            labels = c("Cluster A",
                                                       "Cluster B",
                                                       "Cluster C"))
Hmisc::label(cluster_results_table1$PAM_Cluster) <- "PAM Clusters"

# 1 = Demographics, PMH, medications
pam_tbl1 <- cluster_results_table1[, c(4:5, 7, 13:39, 90)]

# Compute median for AGE_ADMISSION
median_values_pam <- cluster_results_table1 %>%
  group_by(PAM_Cluster) %>%
  summarize(median_age = median_ordinal(AGE_ADMISSION)) %>% 
  ungroup() %>%
  pivot_wider(names_from = PAM_Cluster, values_from = median_age)
median_values_pam <- as.tibble(median_values_pam)

median_value_overall_pam <- cluster_results_table1 %>% 
  summarize(median_age = median_ordinal(AGE_ADMISSION))
median_value_overall_pam <- as.tibble(median_value_overall_pam)

pam_tbl1$AGE_ADMISSION <- as.numeric(pam_tbl1$AGE_ADMISSION)
Hmisc::label(pam_tbl1$AGE_ADMISSION) <- "Age Category on Admission (Median)"

# https://stackoverflow.com/questions/65665465/grouping-rows-in-gtsummary
# https://stackoverflow.com/questions/68278736/formatting-group-header-text-when-using-tbl-stack-from-gtsummary
pam_tbl1 <- tbl_summary(pam_tbl1, 
                       by = PAM_Cluster,
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
  modify_spanning_header(c(stat_1, stat_2, stat_3) ~ "**PAM Clusters**") %>%
  modify_table_body(~ .x %>%
                      dplyr::mutate(stat_1 = ifelse(variable == "AGE_ADMISSION", median_values_pam$`Cluster A`, stat_1),
                                    stat_2 = ifelse(variable == "AGE_ADMISSION", median_values_pam$`Cluster B`, stat_2),
                                    stat_3 = ifelse(variable == "AGE_ADMISSION", median_values_pam$`Cluster C`, stat_3),
                                    # stat_4 = ifelse(variable == "AGE_ADMISSION", median_values_pam$`Cluster 4`, stat_4),
                                    # stat_5 = ifelse(variable == "AGE_ADMISSION", median_values_pam$`Cluster 5`, stat_5),
                                    # stat_6 = ifelse(variable == "AGE_ADMISSION", median_values_pam$`Cluster 6`, stat_6),
                                    stat_0 = ifelse(variable == "AGE_ADMISSION", median_value_overall_pam, stat_0)
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

pam_tbl1
# gt::gtsave(pam_tbl1, "PAM_characteristics.html")

# 2 = Outcomes - RR, Weight, Fluid Balance, LoS, Number of Admissions, I/P death
pam_tbl2 <- cluster_results_table1[, c(77, 79, 82, 83, 84, 85, 86, 87, 88, 90)]

pam_tbl2 <- tbl_summary(pam_tbl2, 
                       by = PAM_Cluster,
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
  modify_spanning_header(c(stat_1, stat_2, stat_3) ~ "**PAM Clusters**")

pam_tbl2
# gt::gtsave(as_gt(pam_tbl2), "PAM_outcomes.html")

# 3 = Outcomes - Observations, Laboratory Tests
pam_tbl3 <- cluster_results_table1[, c(40:58, 90)]

pam_tbl3 <- tbl_summary(pam_tbl3, 
                       by = PAM_Cluster,
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
  modify_spanning_header(c(stat_1, stat_2, stat_3) ~ "**PAM Clusters**") %>% 
  modify_table_body(mutate,
                    groupname_col = case_when(variable %in% c("O2SAT", "RR", "SBP", "DBP", "HR", "Weight", "Height") ~ "Admission Observations",
                                              variable %in% c("WCC", "Hb", "Plts", "Na", "K", "Cr", "Ur", "CRP", "Alb", "Glucose", "Lactate", "BNP") ~ "Admission Blood Tests")
  ) %>% 
  as_gt() %>%
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_row_groups(groups = everything())
  )

pam_tbl3
# gt::gtsave(pam_tbl3, "PAM_obsandbloods.html")

################################################################################ Characteristics table for entire cohort ##################################################################
ML_final_data

ML_final_data$GENDER <- factor(ML_final_data$GENDER,
                                        levels = c(1,0),
                                        labels = c("Male",
                                                   "Female"))
Hmisc::label(ML_final_data$GENDER) <- "Gender"

# https://www.datadictionary.nhs.uk/data_elements/ethnic_category.html
ML_final_data$ETHNICITY <- factor(ML_final_data$ETHNICITY,
                                           levels = c(1,2,3,4,5,6,7,8),
                                           labels = c("Not Provided",
                                                      "White - British",
                                                      "White - Irish",
                                                      "White - Any Other Background",
                                                      "Asian or Asian British - Indian",
                                                      "Asian or Asian British - Any Other Background",
                                                      "Black or Black British - Caribbean",
                                                      "Not Stated"))
Hmisc::label(ML_final_data$ETHNICITY) <- "Ethnicity"

ML_final_data$AF <- factor(ML_final_data$AF,
                                    levels = c(1,0),
                                    labels = c("Present",
                                               "Absent"))
Hmisc::label(ML_final_data$AF) <- "Atrial Fibrillation"

ML_final_data$CAD <- factor(ML_final_data$CAD,
                                     levels = c(1,0),
                                     labels = c("Present",
                                                "Absent"))
Hmisc::label(ML_final_data$CAD) <- "Coronary Artery Disease"

ML_final_data$Hypertension <- factor(ML_final_data$Hypertension,
                                              levels = c(1,0),
                                              labels = c("Present",
                                                         "Absent"))
Hmisc::label(ML_final_data$Hypertension) <- "Hypertension"

ML_final_data$Heart_Failure <- factor(ML_final_data$Heart_Failure,
                                               levels = c(1,0),
                                               labels = c("Present",
                                                          "Absent"))
Hmisc::label(ML_final_data$Heart_Failure) <- "Heart Failure"

ML_final_data$Valvular_Heart_Disease <- factor(ML_final_data$Valvular_Heart_Disease,
                                                        levels = c(1,0),
                                                        labels = c("Present",
                                                                   "Absent"))
Hmisc::label(ML_final_data$Valvular_Heart_Disease) <- "Valvular Heart Disease"

ML_final_data$Pacemaker <- factor(ML_final_data$Pacemaker,
                                           levels = c(1,0),
                                           labels = c("Present",
                                                      "Absent"))
Hmisc::label(ML_final_data$Pacemaker) <- "Pacemaker"

ML_final_data$CVA <- factor(ML_final_data$CVA,
                                     levels = c(1,0),
                                     labels = c("Present",
                                                "Absent"))
Hmisc::label(ML_final_data$CVA) <- "Cerebrovascular Accident"

ML_final_data$PVD <- factor(ML_final_data$PVD,
                                     levels = c(1,0),
                                     labels = c("Present",
                                                "Absent"))
Hmisc::label(ML_final_data$PVD) <- "Peripheral Vascular Disease"

ML_final_data$Anaemia <- factor(ML_final_data$Anaemia,
                                         levels = c(1,0),
                                         labels = c("Present",
                                                    "Absent"))
Hmisc::label(ML_final_data$Anaemia) <- "Anaemia"

ML_final_data$Asthma <- factor(ML_final_data$Asthma,
                                        levels = c(1,0),
                                        labels = c("Present",
                                                   "Absent"))
Hmisc::label(ML_final_data$Asthma) <- "Asthma"

ML_final_data$COPD <- factor(ML_final_data$COPD,
                                      levels = c(1,0),
                                      labels = c("Present",
                                                 "Absent"))
Hmisc::label(ML_final_data$COPD) <- "Chronic Obstructive Pulmonary Disease"

ML_final_data$Diabetes <- factor(ML_final_data$Diabetes,
                                          levels = c(1,0),
                                          labels = c("Present",
                                                     "Absent"))
Hmisc::label(ML_final_data$Diabetes) <- "Diabetes"

ML_final_data$CKD <- factor(ML_final_data$CKD,
                                     levels = c(1,0),
                                     labels = c("Present",
                                                "Absent"))
Hmisc::label(ML_final_data$CKD) <- "Chronic Kidney Disease"

ML_final_data$Thyroid_Disease <- factor(ML_final_data$Thyroid_Disease,
                                                 levels = c(1,0),
                                                 labels = c("Present",
                                                            "Absent"))
Hmisc::label(ML_final_data$Thyroid_Disease) <- "Thyroid Dysfunction"

ML_final_data$Rheumatological_Disease <- factor(ML_final_data$Rheumatological_Disease,
                                                         levels = c(1,0),
                                                         labels = c("Present",
                                                                    "Absent"))
Hmisc::label(ML_final_data$Rheumatological_Disease) <- "Rheumatological Disease"

ML_final_data$VTE <- factor(ML_final_data$VTE,
                                     levels = c(1,0),
                                     labels = c("Present",
                                                "Absent"))
Hmisc::label(ML_final_data$VTE) <- "Venous Thromboembolism"

ML_final_data$Mental_Health <- factor(ML_final_data$Mental_Health,
                                               levels = c(1,0),
                                               labels = c("Present",
                                                          "Absent"))
Hmisc::label(ML_final_data$Mental_Health) <- "Mental Health Diagnosis"

ML_final_data$ACEi <- factor(ML_final_data$ACEi,
                                      levels = c(1,0),
                                      labels = c("Prescribed",
                                                 "Not Prescribed"))
Hmisc::label(ML_final_data$ACEi) <- "ACEi"

ML_final_data$ARB <- factor(ML_final_data$ARB,
                                     levels = c(1,0),
                                     labels = c("Prescribed",
                                                "Not Prescribed"))
Hmisc::label(ML_final_data$ARB) <- "ARB"

ML_final_data$MRA <- factor(ML_final_data$MRA,
                                     levels = c(1,0),
                                     labels = c("Prescribed",
                                                "Not Prescribed"))
Hmisc::label(ML_final_data$MRA) <- "MRA"

ML_final_data$SacubitrilValsartan <- factor(ML_final_data$SacubitrilValsartan,
                                                     levels = c(1,0),
                                                     labels = c("Prescribed",
                                                                "Not Prescribed"))
Hmisc::label(ML_final_data$SacubitrilValsartan) <- "Sacubitril/Valsartan"

ML_final_data$Thiazide <- factor(ML_final_data$Thiazide,
                                          levels = c(1,0),
                                          labels = c("Prescribed",
                                                     "Not Prescribed"))
Hmisc::label(ML_final_data$Thiazide) <- "Thiazide Diuretic"

ML_final_data$LoopDiuretic <- factor(ML_final_data$LoopDiuretic,
                                              levels = c(1,0),
                                              labels = c("Prescribed",
                                                         "Not Prescribed"))
Hmisc::label(ML_final_data$LoopDiuretic) <- "Loop Diuretic"

ML_final_data$Amiloride <- factor(ML_final_data$Amiloride,
                                           levels = c(1,0),
                                           labels = c("Prescribed",
                                                      "Not Prescribed"))
Hmisc::label(ML_final_data$Amiloride) <- "Amiloride"

ML_final_data$BBlocker <- factor(ML_final_data$BBlocker,
                                          levels = c(1,0),
                                          labels = c("Prescribed",
                                                     "Not Prescribed"))
Hmisc::label(ML_final_data$BBlocker) <- "Beta-Blocker"

ML_final_data$Statin <- factor(ML_final_data$Statin,
                                        levels = c(1,0),
                                        labels = c("Prescribed",
                                                   "Not Prescribed"))
Hmisc::label(ML_final_data$Statin) <- "Statin"

ML_final_data$Nitrates <- factor(ML_final_data$Nitrates,
                                          levels = c(1,0),
                                          labels = c("Prescribed",
                                                     "Not Prescribed"))
Hmisc::label(ML_final_data$Nitrates) <- "Nitrates"

ML_final_data$Died_During_Admission <- factor(ML_final_data$Died_During_Admission,
                                                       levels = c(1,0),
                                                       labels = c("Yes",
                                                                  "No"))
Hmisc::label(ML_final_data$Died_During_Admission) <- "Died During Admission"

ML_final_data$AGE_ADMISSION <- as.numeric(ML_final_data$AGE_ADMISSION)
ML_final_data$AGE_ADMISSION <- factor(ML_final_data$AGE_ADMISSION,
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
Hmisc::label(ML_final_data$AGE_ADMISSION) <- "Age Category on Admission"

# 1 = Demographics, PMH, medications
whole_dataset <- ML_final_data[, c(4:5, 7, 13:39)]

median_ordinal <- function(x) {
  levels(x)[median(as.numeric(x))]
}

# Compute median for AGE_ADMISSION
median_values_whole <- whole_dataset %>%
  group_by(GENDER) %>%
  summarize(median_age = median_ordinal(AGE_ADMISSION)) %>% 
  ungroup() %>%
  pivot_wider(names_from = GENDER, values_from = median_age)

median_value_overall_whole <- whole_dataset %>% 
  summarize(median_age = median_ordinal(AGE_ADMISSION))
median_value_overall_whole <- as.tibble(median_value_overall_whole)

whole_dataset$AGE_ADMISSION <- as.numeric(whole_dataset$AGE_ADMISSION)
Hmisc::label(whole_dataset$AGE_ADMISSION) <- "Age Category on Admission (Median)"

# https://stackoverflow.com/questions/65665465/grouping-rows-in-gtsummary
# https://stackoverflow.com/questions/68278736/formatting-group-header-text-when-using-tbl-stack-from-gtsummary
whole_dataset <- tbl_summary(whole_dataset,
                             by = GENDER,
                       percent = "column",
                       type = list(c(AF, CAD, Hypertension, Heart_Failure, Valvular_Heart_Disease, Pacemaker, CVA,
                                     PVD, Anaemia, Asthma, COPD, Diabetes, CKD, Thyroid_Disease, Rheumatological_Disease, VTE,
                                     Mental_Health, ACEi, ARB, MRA, SacubitrilValsartan, Thiazide, LoopDiuretic, Amiloride, BBlocker,
                                     Statin, Nitrates) ~ 'dichotomous',
                                   ETHNICITY ~ 'categorical',
                                   AGE_ADMISSION ~ 'continuous'),
                       value = list(AF = "Present",
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
                       statistic =  list(c(AF, CAD, Hypertension, Heart_Failure, Valvular_Heart_Disease, Pacemaker, CVA,
                                           PVD, Anaemia, Asthma, COPD, Diabetes, CKD, Thyroid_Disease, Rheumatological_Disease, VTE,
                                           Mental_Health, ACEi, ARB, MRA, SacubitrilValsartan, Thiazide, LoopDiuretic, Amiloride, BBlocker,
                                           Statin, Nitrates) ~ "{p}% [{n}]",
                                         AGE_ADMISSION ~ "{median}")) %>%
  add_p() %>% 
  add_overall() %>% 
  modify_table_body(~ .x %>%
                      dplyr::mutate(stat_1 = ifelse(variable == "AGE_ADMISSION", median_values_whole$`Male`, stat_1),
                                    stat_2 = ifelse(variable == "AGE_ADMISSION", median_values_whole$`Female`, stat_2),
                                    stat_0 = ifelse(variable == "AGE_ADMISSION", median_value_overall_whole, stat_0)
                      )) %>% 
  modify_table_body(mutate,
                    groupname_col = case_when(variable %in% c("AGE_ADMISSION", "ETHNICITY") ~ "Demographics",
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

whole_dataset
# gt::gtsave(whole_dataset, "Whole_Cohort_Characteristics.html")

## Outcomes

ML_final_data$ITU_Admission_YorN <- factor(ML_final_data$ITU_Admission_YorN,
                                                    levels = c(1,0),
                                                    labels = c("Yes",
                                                               "No"))
Hmisc::label(ML_final_data$ITU_Admission_YorN) <- "ITU Admission"

# 2 = Outcomes - RR, Weight, Fluid Balance, LoS, Number of Admissions, I/P death
whole_dataset_outcomes <- ML_final_data[, c(5, 77, 79, 82, 83, 84, 85, 86, 87, 88)]

whole_dataset_outcomes <- tbl_summary(whole_dataset_outcomes, 
                       by = GENDER,
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
  add_overall()

whole_dataset_outcomes
# gt::gtsave(as_gt(whole_dataset_outcomes), "Whole_Cohort_Outcomes.html")

# 3 = Outcomes - Observations, Laboratory Tests
whole_dataset_outcomes_obsandbloods <- ML_final_data[, c(40:58, 5)]

whole_dataset_outcomes_obsandbloods <- tbl_summary(whole_dataset_outcomes_obsandbloods, 
                        by = GENDER,
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
  modify_table_body(mutate,
                    groupname_col = case_when(variable %in% c("O2SAT", "RR", "SBP", "DBP", "HR", "Weight", "Height") ~ "Admission Observations",
                                              variable %in% c("WCC","Hb", "Plts", "Na", "K", "Cr", "Ur", "CRP", "Alb", "Glucose", "Lactate", "BNP") ~ "Admission Blood Tests")
  ) %>% 
  as_gt() %>%
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_row_groups(groups = everything())
  )

whole_dataset_outcomes_obsandbloods
# gt::gtsave(whole_dataset_outcomes_obsandbloods, "Whole_cohort_obsandbloods.html")
