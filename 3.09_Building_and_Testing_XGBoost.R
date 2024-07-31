### Train XGBoost Model
## https://www.analyticsvidhya.com/blog/2016/01/xgboost-algorithm-easy-steps/

set.seed(123)

### Using outcome clustering
cluster_results_table1

features <- gower_data

# Need to set labels 0 - 2 rather than 1 - 3
cluster_results_table1$HC_Cluster <- as.numeric(cluster_results_table1$HC_Cluster)
labels <- cluster_results_table1$HC_Cluster - 1

dtrain <- xgb.DMatrix(data = as.matrix(features), label = as.matrix(labels))

num_class <- length(unique(labels))

# Define the grid of hyperparameters
tune_grid <- expand.grid(
  nrounds = c(50, 100, 150),       
  max_depth = c(4, 6, 8, 10),     
  eta = c(0.01, 0.1, 0.3),        
  gamma = c(0, 1, 5),            
  colsample_bytree = c(0.5, 0.7, 1), 
  min_child_weight = c(1, 3, 5),   
  subsample = c(0.5, 0.7, 1)      
)

train_control <- trainControl(
  method = "cv",                
  number = 5,                    
  verboseIter = TRUE,           
  allowParallel = TRUE     
)

# Train the model using caret's train function with grid search
xgb_train <- train(
  x = features, 
  y = labels,
  trControl = train_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  metric = "logLoss",     
  maximize = FALSE             
)

print(xgb_train$bestTune)

best_params <- xgb_train$bestTune
final_model <- xgb.train(
  params = list(
    objective = "multi:softprob",
    eval_metric = "mlogloss",
    num_class = num_class,
    max_depth = best_params$max_depth,
    eta = best_params$eta,
    gamma = best_params$gamma,
    colsample_bytree = best_params$colsample_bytree,
    min_child_weight = best_params$min_child_weight,
    subsample = best_params$subsample
  ),
  data = dtrain,
  nrounds = best_params$nrounds
)

#################################################################################################
### Test Model on Test Data - Need to label test data with HC
# Train gower matrix
gower_data_test <- test %>% dplyr::select(selected_features_based_on_contributions)
gower_data_test <- gower_data_test %>% 
  mutate(across(continuousvariable, scale))
gower_data_test <- gower_data_test %>% 
  mutate(across(continuousvariable, as.numeric))
gower_matrix_test <- daisy(gower_data_test, metric = c("gower"), type=list(symm=2))
gower_dist_test <- as.dist(gower_matrix_test)

### Hierarchical clustering
hc.w2_test = hclust(gower_matrix_test, method="ward.D2")
plot(hc.w2_test)
clusterCutw_test <- cutree(hc.w2_test, 3)

# Attach hierarchical clusters
hc.w_results_test <- cbind(test, Cluster = factor(unname(clusterCutw_test), labels = c(1,2,3)))
head(hc.w_results_test)

#################################################################################################
# Can run table1_generation_test now

#################################################################################################
### Hierarchical clustering
test_labels <- hc.w_results_test$Cluster

# Need to remove factors from test data
test_features <- hc.w_results_test %>%
  ungroup() %>%
  select(selected_features_based_on_contributions)

test_features <- test_features %>%
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
test_features$ETHNICITY <- as.numeric(test_features$ETHNICITY)
test_features$GENDER <- as.numeric(test_features$GENDER)
test_features$Nitrates <- as.numeric(test_features$Nitrates)
test_features$Statin <- as.numeric(test_features$Statin)
test_features$BBlocker <- as.numeric(test_features$BBlocker)
test_features$ACEi <- as.numeric(test_features$ACEi)
test_features$Hypertension <- as.numeric(test_features$Hypertension)
test_features$AF <- as.numeric(test_features$AF)
test_features <- test_features %>%
  mutate(across(c(GENDER, Nitrates:AF), ~ ifelse(. == 2, 1, 0)))

dtest <- xgb.DMatrix(data = as.matrix(test_features), label = as.matrix(test_labels))

# Make predictions
predictions <- predict(final_model, dtest)
# Reshape predictions to have one row per observation and one column per class
predictions <- matrix(predictions, ncol = num_class, byrow = TRUE)

# Get the predicted class
predicted_labels <- max.col(predictions) - 1

# Convert predicted labels to 1-3 instead of 0-2
predicted_labels <- predicted_labels + 1

# Calculate accuracy
accuracy <- sum(predicted_labels == test_labels) / length(test_labels)
print(paste("Accuracy:", accuracy))

# Calculate F1 score
# Calculate confusion matrix
conf_matrix <- confusionMatrix(factor(predicted_labels), factor(test_labels))

# Print confusion matrix
print(conf_matrix)

# Calculate precision, recall, and F1 score for each class - https://towardsdatascience.com/performance-metrics-confusion-matrix-precision-recall-and-f1-score-a8fe076a2262
precision <- conf_matrix$byClass[, "Precision"]
recall <- conf_matrix$byClass[, "Recall"]
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print F1 score for each class
print(paste("F1 score for each class:", f1_score))

# Calculate macro F1 score
macro_f1 <- mean(f1_score)
print(paste("Macro F1 score:", macro_f1))

# Calculate micro F1 score
micro_f1 <- sum(2 * conf_matrix$table * precision * recall) / sum(conf_matrix$table * (precision + recall))
print(paste("Micro F1 score:", micro_f1))

# Convert test labels to one-hot encoding
test_labels_one_hot <- model.matrix(~ test_labels - 1)
colnames(test_labels_one_hot) <- levels(factor(test_labels))

# Calculate ROC and AUC for each class
roc_list <- lapply(1:num_class, function(i) {
  roc(test_labels_one_hot[, i], predictions[, i], levels = c(0, 1), direction = "<")
})

# Extract AUC for each class
auc_list <- sapply(roc_list, function(roc) roc$auc)

# Print AUC for each class
print(paste("AUC for each class:", auc_list))

# Calculate macro AUC (average AUC of all classes)
macro_auc <- mean(auc_list)
print(paste("Macro AUC:", macro_auc))

# Calculate micro AUC (aggregate contributions of all classes)
# This is typically done using the one-vs-all approach
micro_roc <- roc(as.vector(test_labels_one_hot), as.vector(predictions), levels = c(0, 1), direction = "<")
micro_auc <- micro_roc$auc
print(paste("Micro AUC:", micro_auc))
