### Clustering on outcomes 
# Train 1D matrix on just LOS
los_clustering <- hclust1d(as.matrix(scale(train$Length_of_Stay)), distance = FALSE, squared = FALSE, method = "ward.D2")
plot(los_clustering)
clusterCutlos <- cutree(los_clustering, 4)
los_results <- cbind(train, Cluster = factor(unname(clusterCutlos), labels = c(1,2,3,4)))
head(los_results)
median_los_per_cluster <- los_results %>%
  group_by(Cluster) %>%
  summarize(median_los_category = mean(as.numeric(Length_of_Stay)))
summary(as.factor(los_results$Cluster))
print(median_los_per_cluster)

# Train matrix on multiple outcomes
outcomes <- c("FLUID_BALANCE", "NUMBER_OF_ADMISSIONS_IN_1YR", "TIME_TO_NEXT", "Length_of_Stay", "RR_Diff_24HR", "RR_Diff_Disch", "Weight_Diff_24HR", "Weight_Diff_Disch")
## Impute missing values ##
# Impute all NULLs
columns_impute <- train %>% 
  dplyr::select(outcomes)
columns_impute <- columns_impute %>% mutate(Length_of_Stay = as.numeric(Length_of_Stay))
columns_impute <- as.data.frame(columns_impute)

# Imputation
imputed_matrix <- impute.svd(as.matrix(columns_impute), maxit = 10000)

# Reinsert imputed values into dataframe
imputed_columns <- as.data.frame(imputed_matrix)
colnames(imputed_columns) <- colnames(columns_impute)

# Remove end columns (think I will need these back to be honest) - these offer some sort of statistical testing
imputed_columns <- imputed_columns[, -c(9,10)]
train[, outcomes] <- imputed_columns

## End of imputation ##

## Train matrix on outcomes
outcomes_df <- train[, outcomes]
outcomes_df <- outcomes_df %>% 
  mutate(across(outcomes, as.numeric))
outcomes_df <- outcomes_df %>% 
  mutate(across(outcomes, scale))
outcomes_df <- outcomes_df %>% mutate(Length_of_Stay = as.numeric(Length_of_Stay))
outcomes_df <- as.matrix(outcomes_df)
gower_matrix_outcomes <- daisy(outcomes_df, metric = c("gower"))
gower_dist_outcomes <- as.dist(gower_matrix_outcomes)

## Visualise outcomes matrix
hc.w2_outcomes = hclust(gower_matrix_outcomes, method="ward.D2")
plot(hc.w2_outcomes)

## Optimal number of clusters
# Using NbClust - Gives a number of statistics
clusters_hierarchical_outcomes <- NbClust(outcomes_df, diss = gower_dist_outcomes, distance = NULL, min.nc = 2, max.nc = 6, method = "ward.D2", index = "all")
optimal_clustnumb_nbclust_outcomes <- clusters_hierarchical_outcomes$Best.nc[1]
print(optimal_clustnumb_nbclust_outcomes)
clusterCutw_outcomed <- cutree(hc.w2_outcomes, 4)

## Attach hierarchical outcome clusters
hc.w_results_outcomes <- cbind(train, Cluster = factor(unname(clusterCutw_outcomed), labels = c(1,2,3,4)))
head(hc.w_results_outcomes)

### End of clustering by outcomes