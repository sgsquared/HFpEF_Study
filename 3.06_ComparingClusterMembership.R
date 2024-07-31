### Comparing cluster membership for different clustering methodologies

# First check which clusters relate most closely to each other
kappa_calculation <- cluster_results[, c(89,90)]
kappa_calculation <- as.data.frame(kappa_calculation)

# Function to calculate overlap between clusters
# https://stackoverflow.com/questions/47490156/determine-cluster-similarity-set-overlap
calculate_overlap <- function(kappa_calculation) {
  unique_clusters_method1 <- unique(kappa_calculation$HC_Cluster)
  unique_clusters_method2 <- unique(kappa_calculation$PAM_Cluster)
  
  overlap_matrix <- matrix(0, nrow = length(unique_clusters_method1), ncol = length(unique_clusters_method2))
  colnames(overlap_matrix) <- paste("Method2_Cluster", unique_clusters_method2, sep = "_")
  rownames(overlap_matrix) <- paste("Method1_Cluster", unique_clusters_method1, sep = "_")
  
  for (i in unique_clusters_method1) {
    for (j in unique_clusters_method2) {
      overlap_matrix[paste("Method1_Cluster", i, sep = "_"), paste("Method2_Cluster", j, sep = "_")] <- 
        sum(kappa_calculation$HC_Cluster == i & kappa_calculation$PAM_Cluster == j)
    }
  }
  
  return(overlap_matrix)
}

# Calculate the overlap
overlap_matrix <- calculate_overlap(kappa_calculation)

# Print the overlap matrix
print(overlap_matrix)
# 1=5, 2=2, 3=6, 4=1, 5=3, 6=4 - admissions
# 1=1, 2=3, 3=2, 4=4, 5=5, 6=6 - patients
heatmap(overlap_matrix)

# change cluster method 2 to match clusters better
# 3 clusters
kappa_calculation$new_PAM_clusters <- ifelse(kappa_calculation$PAM_Cluster == 1, 1,
                                            ifelse(kappa_calculation$PAM_Cluster == 3, 2,
                                                   ifelse(kappa_calculation$PAM_Cluster == 2, 3, NA)))
# 6 clusters
# kappa_calculation$new_PAM_clusters <- ifelse(kappa_calculation$PAM_Cluster == 1, 1,
#                                              ifelse(kappa_calculation$PAM_Cluster == 3, 2,
#                                                     ifelse(kappa_calculation$PAM_Cluster == 2, 3,  
#                                                            ifelse(kappa_calculation$PAM_Cluster == 4, 4,
#                                                                   ifelse(kappa_calculation$PAM_Cluster == 5, 5,
#                                                                          ifelse(kappa_calculation$PAM_Cluster == 6, 6, NA))))))
kappa_new_calculate <- kappa_calculation[, c(1,3)]
new_kappa_result <- cohen.kappa(as.data.frame(kappa_new_calculate))
print(new_kappa_result)

newARI1 <- adjustedRandIndex(kappa_new_calculate$HC_Cluster, kappa_new_calculate$new_PAM_clusters)
newARI1

newagreement <- sum(kappa_new_calculate$HC_Cluster == kappa_new_calculate$new_PAM_clusters) / nrow(kappa_new_calculate)
print(newagreement)

kappa_new_calculate <- as.data.frame(kappa_new_calculate)

# Function to calculate Kappa for each level
calculate_kappa_per_level <- function(kappa_new_calculate, level) {
  binary_data <- data.frame(
    method1_binary = ifelse(kappa_new_calculate$HC_Cluster == level, 1, 0),
    method2_binary = ifelse(kappa_new_calculate$new_PAM_clusters == level, 1, 0)
  )
  kappa_result <- kappa2(binary_data)
  return(kappa_result$value)
}

# Unique levels in the dataset
levels <- sort(unique(cluster_results$HC_Cluster))

# Calculate Kappa for each level
kappa_results <- sapply(levels, function(level) calculate_kappa_per_level(kappa_new_calculate, level))

# Print the results
kappa_results

# Change cluster method 2 to match clusters better in overall dataset - better for comparison
# 3 clusters
# cluster_results$PAM_Cluster <- ifelse(cluster_results$PAM_Cluster == 1, 1,
#                                              ifelse(kappa_calculation$PAM_Cluster == 3, 2,
#                                                     ifelse(kappa_calculation$PAM_Cluster == 2, 3, NA)))
# 6 clusters
# cluster_results$PAM_Cluster <- ifelse(cluster_results$PAM_Cluster == 1, 1,
#                                       ifelse(kappa_calculation$PAM_Cluster == 3, 2,
#                                              ifelse(kappa_calculation$PAM_Cluster == 2, 3,
#                                                     ifelse(kappa_calculation$PAM_Cluster == 4, 4,
#                                                            ifelse(kappa_calculation$PAM_Cluster == 5, 5,
#                                                                   ifelse(kappa_calculation$PAM_Cluster == 6, 6, NA))))))
