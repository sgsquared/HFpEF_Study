### Dataset split and training:-
### - Label 80% of the patients with clusters
### - Assess differences in outcomes amongst these clusters

complete_selected_data

# ML_final_data <- complete_selected_data
ML_final_data <- imputed_dataset
# Rather than removing columns - keep them for Table 1 later on

# Convert categorical to factor
ML_final_data <- ML_final_data %>% 
  mutate(across(AF:Nitrates, as.factor))
ML_final_data$AGE_ADMISSION <- ifelse(ML_final_data$AGE_ADMISSION == 1, "20 - 24",
                                      ifelse(ML_final_data$AGE_ADMISSION == 2, "25 - 29",
                                             ifelse(ML_final_data$AGE_ADMISSION == 3, "30 - 34",
                                                    ifelse(ML_final_data$AGE_ADMISSION == 4, "35 - 39",
                                                           ifelse(ML_final_data$AGE_ADMISSION == 5, "40 - 44",
                                                                  ifelse(ML_final_data$AGE_ADMISSION == 6, "45 - 49",
                                                                         ifelse(ML_final_data$AGE_ADMISSION == 7, "50 - 54",
                                                                                ifelse(ML_final_data$AGE_ADMISSION == 8, "55 - 59",
                                                                                       ifelse(ML_final_data$AGE_ADMISSION == 9, "60 - 64",
                                                                                              ifelse(ML_final_data$AGE_ADMISSION == 10, "65 - 69",
                                                                                                     ifelse(ML_final_data$AGE_ADMISSION == 11, "70 - 74",
                                                                                                            ifelse(ML_final_data$AGE_ADMISSION == 12, "75 - 79",
                                                                                                                   ifelse(ML_final_data$AGE_ADMISSION == 13, "80 - 84",
                                                                                                                          ifelse(ML_final_data$AGE_ADMISSION == 14, "85 - 89",
                                                                                                                                 ifelse(ML_final_data$AGE_ADMISSION == 15, "90 - 94",
                                                                                                                                        ifelse(ML_final_data$AGE_ADMISSION == 16, "95 - 99",
                                                                                                                                               ifelse(ML_final_data$AGE_ADMISSION == 17, "100 - 104", NA)))))))))))))))))
ML_final_data <- ML_final_data %>% 
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
ML_final_data$GENDER <- ifelse(ML_final_data$GENDER == 2, 1,
                               ifelse(ML_final_data$GENDER == 1, 0, NA))
ML_final_data <- ML_final_data %>% 
  mutate(GENDER = factor(GENDER))
ML_final_data <- ML_final_data %>% 
  mutate(ETHNICITY = factor(ETHNICITY))

# Process other outcomes
ML_final_data$NUMBER_OF_ADMISSIONS_IN_1YR <- as.numeric(ML_final_data$NUMBER_OF_ADMISSIONS_IN_1YR)
ML_final_data$ADMISSION_NUMBER <- as.numeric(ML_final_data$ADMISSION_NUMBER)
ML_final_data$ITU_Admission_YorN <- as.factor(ifelse(is.na(ML_final_data$DURATION_OF_ITU) == TRUE, 0, 1))
ML_final_data$Length_of_Stay <- difftime(ML_final_data$DISCH_DATETIME, ML_final_data$ADM_DATETIME, units = "days")
ML_final_data$RR_Diff_24HR <- ML_final_data$X24HR_RR - ML_final_data$ADM_RR
ML_final_data$RR_Diff_Disch <- ML_final_data$DISCH_RR - ML_final_data$ADM_RR
ML_final_data$Weight_Diff_24HR <- ML_final_data$X24HR_WEIGHT - ML_final_data$ADM_WEIGHT
ML_final_data$Weight_Diff_Disch <- ML_final_data$DISCH_WEIGHT - ML_final_data$ADM_WEIGHT
ML_final_data$Died_During_Admission <- as.factor(ifelse(ML_final_data$DISCH_DEST %in% "79", 1, 0))
ML_final_data$DISCH_DEST <- as.factor(ifelse(ML_final_data$DISCH_DEST %in% "19", "Usual Place of Residence",
                                             ifelse(ML_final_data$DISCH_DEST %in% "29", "Temporary Place of Residence (e.g. Hotel, Educational Establishment",
                                                    ifelse(ML_final_data$DISCH_DEST %in% "51", "NHS Other Hospital Provider for Adults",
                                                           ifelse(ML_final_data$DISCH_DEST %in% "54", "NHS Run Care Home",
                                                                  ifelse(ML_final_data$DISCH_DEST %in% "79", "Not Applicable - Patient Died",
                                                                         ifelse(ML_final_data$DISCH_DEST %in% "85", "Non-NHS Run Care Home",
                                                                                ifelse(ML_final_data$DISCH_DEST %in% "87", "Non-NHS Run Hospital",
                                                                                       ifelse(ML_final_data$DISCH_DEST %in% "88", "Non-NHS Run Hospice", ML_final_data$DISCH_DEST)))))))))
# https://archive.datadictionary.nhs.uk/DD%20Release%20October%202020/data_elements/discharge_destination_code__mother_post_delivery_hospital_provider_spell_.html

# Set seed
set.seed(123)

## 80% of the sample size
smp_size <- floor(0.80 * nrow(ML_final_data))

# Split the data
train_ind <- sample(seq_len(nrow(ML_final_data)), size = smp_size)
train <- ML_final_data[train_ind, ]
test <- ML_final_data[-train_ind, ]

# Train gower matrix
# Scale variables
gower_data <- train %>% dplyr::select(selected_features_based_on_contributions)
gower_data <- gower_data %>% 
  mutate(across(continuousvariable, scale))
gower_data <- gower_data %>% 
  mutate(across(continuousvariable, as.numeric))
gower_matrix <- daisy(gower_data, metric = c("gower"), type=list(symm=2))
gower_dist <- as.dist(gower_matrix)

# Determine optimal number of clusters for hierarchical clustering
# https://r-tastic.co.uk/post/optimal-number-of-clusters/
# https://rstudio-pubs-static.s3.amazonaws.com/154174_78c021bc71ab42f8add0b2966938a3b8.html
# Model-Based Clustering Using BIC
model_based_clustering_n <- Mclust(gower_matrix, G=1:6) # run across 10 iterations
best_mclust_n <- dim(model_based_clustering_n$BIC)[2]
plot(model_based_clustering_n$BIC)
cat("model-based optimal number of clusters:", best_mclust_n, "\n")
# Optimal number of cluster is 6

# Elbow Method
# Encode factors for statistical testing
gower_data <- gower_data %>%
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
gower_data$ETHNICITY <- as.numeric(gower_data$ETHNICITY)
gower_data$GENDER <- as.numeric(gower_data$GENDER)
gower_data$Nitrates <- as.numeric(gower_data$Nitrates)
gower_data$Statin <- as.numeric(gower_data$Statin)
gower_data$BBlocker <- as.numeric(gower_data$BBlocker)
gower_data$LoopDiuretic <- as.numeric(gower_data$LoopDiuretic)
gower_data$ACEi <- as.numeric(gower_data$ACEi)
gower_data$Hypertension <- as.numeric(gower_data$Hypertension)
gower_data$AF <- as.numeric(gower_data$AF)
gower_data <- gower_data %>%
  mutate(across(c(ETHNICITY, GENDER, Nitrates:AF), ~ ifelse(. == 2, 1, 0)))

# Plot Elbow Method
fviz_nbclust(gower_data, kmeans, method = "wss", k.max = 6) + theme_minimal() + ggtitle("the Elbow Method")
# Optimal number is 4

# Using NbClust - Gives a number of statistics
clusters_hierarchical <- NbClust(gower_data, diss = gower_dist, distance = NULL, min.nc = 3, max.nc = 6, method = "ward.D2", index = "all")
optimal_clustnumb_nbclust <- clusters_hierarchical$Best.nc
print(optimal_clustnumb_nbclust)
# Optimal number is 2 (followed by 5)

# Using the Gap Statistic
gap_stat <- clusGap(gower_data, FUN = kmeans, nstart = 25, K.max = 6, B = 100)
fviz_gap_stat(gap_stat) + theme_minimal() + ggtitle("fviz_gap_stat: Gap Statistic")
# Optimal number is 5

# hierarchical clustering
# selecting linkage term is up for debate
# https://cran.r-project.org/web/packages/hclust1d/vignettes/getting-started.html
# https://stats.stackexchange.com/questions/195446/choosing-the-right-linkage-method-for-hierarchical-clustering
hc.m = hclust(gower_matrix, method="median")
hc.s = hclust(gower_matrix, method="single")
hc.c = hclust(gower_matrix, method="complete") # this would be my number one choice if using hierarchical clustering
hc.w2 = hclust(gower_matrix, method="ward.D2")
plot(hc.m)
plot(hc.s)
plot(hc.c)
plot(hc.w2)
clusterCutc <- cutree(hc.c, 7)

# Attach hierarchical clusters
# 6 clusters
# clusterCutw <- cutree(hc.w2, 6)
# hc.w_results <- cbind(train, HC_Cluster = factor(unname(clusterCutw), labels = c(1,2,3,4,5,6)))
# 4 clusters
# clusterCutw <- cutree(hc.w2, 4)
# hc.w_results <- cbind(train, HC_Cluster = factor(unname(clusterCutw), labels = c(1,2,3,4)))
# 3 clusters
clusterCutw <- cutree(hc.w2, 3)
hc.w_results <- cbind(train, HC_Cluster = factor(unname(clusterCutw), labels = c(1,2,3)))

head(hc.w_results)

# PAM - 6 max cluster number in literature
pc = pamk(gower_matrix, krange=1:6, criterion="asw")
# best performing metric is 4 clusters
pc[2:3]
k <- 3

set.seed(123)
pam_fit <- pam(gower_matrix, diss = TRUE, k) 
cluster_results <- hc.w_results %>%
  mutate(PAM_Cluster = pam_fit$clustering) %>%
  group_by(PAM_Cluster) 
plot(pam_fit)

summary(as.factor(cluster_results$PAM_Cluster))

# Visualise PAM clusters
# https://dpmartin42.github.io/posts/r/cluster-mixed-types
# Calculate the number of samples
num_samples <- nrow(cluster_results)

# Set the perplexity to a value less than num_samples / 3
perplexity_value <- min(30, floor(num_samples / 3))

# Run t-SNE with the adjusted perplexity
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE, perplexity = perplexity_value)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(cluster_results$PAM_Cluster),
         name = cluster_results$PAM_Cluster)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster)) +
  geom_mark_ellipse(aes(fill = as.factor(cluster)), expand = unit(0.5,"mm"))

remove(clusters_hierarchical, combined_data, gap_stat, hc.c, hc.m, hc.s, hc.w2, loadings, ML_dataset, model_based_clustering_n, pam_fit, pc, pca_result, selected_data, tsne_data, tsne_obj)