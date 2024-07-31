### Table1 and Survival Analysis with Kaplan Meier

## Survival analysis for length-of-stay by cluster
# Create a status column - discharged = 1, died = 0
cluster_results_table1$status_column <- 1
# Computing survival model - hierarchical clustering
km_cluster_fit_los <- survfit(Surv(Length_of_Stay, status_column) ~ HC_Cluster, data=cluster_results_table1)
autoplot(km_cluster_fit_los)
cox_los_hc <- coxph(Surv(Length_of_Stay, status_column) ~ HC_Cluster, data=cluster_results_table1)
ggforest(cox_los_hc, fontsize = 1.1)

# Plot the baseline survival function for length-of-stay
simple_survival_los <- ggsurvplot(km_cluster_fit_los, data = cluster_results_table1, variable = "cluster",
                                      individual.curves = TRUE,
                                      xlab = "Days",
                                      ylab = "Cumulative Survival",
                                      pval = TRUE,
                                      title = "Length of Stay",
                                      font.x = c(22),
                                      font.y = c(22),
                                      font.legend = c(24),
                                      font.title = c(24),
                                      font.tickslab = c(16),
                                      risk.table = FALSE,
                                      linetype = "solid", # Change line type by groups,
                                      ggtheme = theme_bw(),
                                      legend = "top", # c(0.9,0.87)
                                      legend.title = "Hierarchical Clusters",
                                      legend.labs = c("Cluster 1", 
                                                      "Cluster 2", 
                                                      "Cluster 3"), 
                                                      #"Cluster 4", 
                                                      #"Cluster 5", 
                                                      #"Cluster 6"),
                                      palette = c("green", 
                                                  "blue", 
                                                  "orange")) #, 
                                                  # "red", 
                                                  # "pink", 
                                                  # "yellow"))
simple_survival_los

# Computing survival model - PAM clustering
km_cluster_fit_los_pam <- survfit(Surv(Length_of_Stay, status_column) ~ PAM_Cluster, data=cluster_results_table1)
autoplot(km_cluster_fit_los_pam)
cox_los_pam <- coxph(Surv(Length_of_Stay, status_column) ~ PAM_Cluster, data=cluster_results_table1)
ggforest(cox_los_pam)

# Plot the baseline survival function for length-of-stay
simple_survival_los_pam <- ggsurvplot(km_cluster_fit_los_pam, data = cluster_results_table1, variable = "cluster",
                                  individual.curves = TRUE,
                                  xlab = "Days",
                                  ylab = "Cumulative Survival",
                                  pval = TRUE,
                                  font.x = c(22),
                                  font.y = c(22),
                                  font.legend = c(24),
                                  font.tickslab = c(16),
                                  risk.table = FALSE,
                                  linetype = "solid", # Change line type by groups,
                                  ggtheme = theme_bw(),
                                  legend = "top", # c(0.9,0.87)
                                  legend.title = "Survival by Cluster - Length of Stay",
                                  legend.labs = c("Cluster 1", 
                                                  "Cluster 2", 
                                                  "Cluster 3"), 
                                                  #"Cluster 4", 
                                                  #"Cluster 5", 
                                                  #"Cluster 6"),
                                      palette = c("green", 
                                                  "blue", 
                                                  "orange")) #, 
                                                  # "red", 
                                                  # "pink", 
                                                  # "yellow"))
simple_survival_los_pam

# Plot the baseline survival function for mortality (overall - not just inpatient)
# Calculate time to death
mort_cluster_results <- cluster_results_table1
mort_cluster_results$Time_to_Death <- difftime(mort_cluster_results$DOD, mort_cluster_results$ADM_DATETIME, units = "days")
mort_cluster_results$Overall_Death_Status <- ifelse(is.na(mort_cluster_results$DOD) == TRUE, 0, 1)

# Fit survival model (ACM = all-cause mortality) - HC
km_cluster_fit_overall_acm <- survfit(Surv(Time_to_Death, Overall_Death_Status) ~ HC_Cluster, data=mort_cluster_results)
autoplot(km_cluster_fit_overall_acm)
cox_acm_hc <- coxph(Surv(Time_to_Death, Overall_Death_Status) ~ HC_Cluster, data=mort_cluster_results)
ggforest(cox_acm_hc)

simple_survival_overall_acm <- ggsurvplot(km_cluster_fit_overall_acm, data = mort_cluster_results, variable = "cluster",
                                  individual.curves = TRUE,
                                  xlab = "Days",
                                  ylab = "Cumulative Survival",
                                  pval = TRUE,
                                  title = "Time to Mortality (Inpatient or Outpatient)",
                                  font.x = c(22),
                                  font.y = c(22),
                                  font.title = c(24),
                                  font.legend = c(24),
                                  font.tickslab = c(16),
                                  risk.table = FALSE,
                                  linetype = "solid", # Change line type by groups,
                                  ggtheme = theme_bw(),
                                  legend = "top", # c(0.9,0.87)
                                  legend.title = "Hierarchical Clusters",
                                  legend.labs = c("Cluster 1", 
                                                  "Cluster 2", 
                                                  "Cluster 3"),
                                  palette = c("green", 
                                              "blue", 
                                              "orange"))
simple_survival_overall_acm

# Fit survival model (ACM = all-cause mortality) - PAM
km_cluster_fit_overall_acm_pam <- survfit(Surv(Time_to_Death, Overall_Death_Status) ~ PAM_Cluster, data=mort_cluster_results)
autoplot(km_cluster_fit_overall_acm_pam)
cox_acm_pam <- coxph(Surv(Time_to_Death, Overall_Death_Status) ~ PAM_Cluster, data=mort_cluster_results)
ggforest(cox_acm_pam)

simple_survival_overall_acm_pam <- ggsurvplot(km_cluster_fit_overall_acm_pam, data = mort_cluster_results, variable = "cluster",
                                          individual.curves = TRUE,
                                          xlab = "Days",
                                          ylab = "Cumulative Survival",
                                          pval = TRUE,
                                          title = "Time to Mortality (Inpatient or Outpatient)",
                                          font.x = c(22),
                                          font.y = c(22),
                                          font.legend = c(24),
                                          font.title = c(24),
                                          font.tickslab = c(16),
                                          risk.table = FALSE,
                                          linetype = "solid", # Change line type by groups,
                                          ggtheme = theme_bw(),
                                          legend = "top", # c(0.9,0.87)
                                          legend.title = "PAM Clusters",
                                          legend.labs = c("Cluster 1", 
                                                          "Cluster 2", 
                                                          "Cluster 3"),
                                          palette = c("green", 
                                                      "blue", 
                                                      "orange"))
simple_survival_overall_acm_pam

# Create panel for HC
survival_and_los <- ggarrange(simple_survival_los$plot, simple_survival_overall_acm$plot, ncol=2, nrow=1, labels="AUTO", common.legend = TRUE, legend="bottom")
