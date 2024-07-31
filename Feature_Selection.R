### Variable selection using prinicpal feature analysis

# AS A MINIMUM
# Need to use a variable selection method to choose between:-
# - Height
# - Gender
# - SBP
# - DBP

# Generate a dataset for variable selection and subsequently analysis
ML_dataset <- imputed_dataset

# First normalise continuous variables
ML_dataset[40:58] <- as.data.frame(scale(ML_dataset[40:58]))

# Create variables with values assigned
predictorvariable <-  c("AF",
                        "CAD",
                        "Hypertension",
                        "Heart_Failure",
                        "Valvular_Heart_Disease",
                        "Pacemaker",
                        "CVA",
                        "PVD",
                        "Anaemia",
                        "Asthma",
                        "COPD",
                        "Diabetes",
                        "CKD",
                        "Thyroid_Disease",
                        "Rheumatological_Disease",
                        "VTE",
                        "Mental_Health",
                        "ACEi",
                        "ARB",
                        "MRA",
                        "SacubitrilValsartan",
                        "Thiazide",
                        "LoopDiuretic",
                        "Amiloride",
                        "BBlocker",
                        "Statin",
                        "Nitrates",
                        "O2SAT",
                        "RR",
                        "SBP",
                        "DBP",
                        "HR",
                        "Weight",
                        "Height",
                        "WCC",
                        "Hb",
                        "Plts",
                        "Na",
                        "K",
                        "Cr",
                        "Ur",
                        "CRP",
                        "Alb",
                        "Glucose",
                        "Lactate",
                        "BNP")

discretevariable <- c("AF",
                      "CAD",
                      "Hypertension",
                      "Heart_Failure",
                      "Valvular_Heart_Disease",
                      "Pacemaker",
                      "CVA",
                      "PVD",
                      "Anaemia",
                      "Asthma",
                      "COPD",
                      "Diabetes",
                      "CKD",
                      "Thyroid_Disease",
                      "Rheumatological_Disease",
                      "VTE",
                      "Mental_Health",
                      "ACEi",
                      "ARB",
                      "MRA",
                      "SacubitrilValsartan",
                      "Thiazide",
                      "LoopDiuretic",
                      "Amiloride",
                      "BBlocker",
                      "Statin",
                      "Nitrates")

continuousvariable <- c("O2SAT",
                        "RR",
                        "SBP",
                        "DBP",
                        "HR",
                        "Weight",
                        "Height",
                        "WCC",
                        "Hb",
                        "Plts",
                        "Na",
                        "K",
                        "Cr",
                        "Ur",
                        "CRP",
                        "Alb",
                        "Glucose",
                        "Lactate",
                        "BNP")

## Principal feature analysis for variable selection
# Combine scaled continuous data with encoded categorical data
combined_data <- cbind((ML_dataset %>% dplyr::select(all_of(continuousvariable))), (ML_dataset %>% dplyr::select(discretevariable)), (ML_dataset %>% dplyr::select(AGE_ADMISSION, GENDER, ETHNICITY)))

# Perform PCA on the combined data - continuous data is already scaled
pca_result <- prcomp(combined_data, center = TRUE, scale. = FALSE)

# Get the loadings (contributions of each feature to the principal components)
loadings <- pca_result$rotation

# Determine the number of components to use (explained variance threshold)
explained_variance <- summary(pca_result)$importance[2, ]
cumulative_explained_variance <- cumsum(explained_variance)

# Decide on the number of principal components to use (e.g., explaining 70% of the variance)
# https://www.statisticssolutions.com/free-resources/directory-of-statistical-analyses/factor-analysis/#:~:text=Kaiser%20Criterion%3A%20An%20eigenvalue%20greater,threshold%20of%200.7%20or%20higher.
num_components <- which(cumulative_explained_variance >= 0.70)[1]

# Calculate the contributions of each feature to the dplyr::selected principal components
feature_contributions <- apply(loadings[, 1:num_components]^2, 1, sum)
print(sort(feature_contributions, decreasing = TRUE))
###########################################################################
# Need to use a variable selection method to choose between:-
# - Height - explains more variance but gender forced due to aetiological importance
# - Gender
# - SBP
# - DBP - describes more variance
###########################################################################
# Remove height, heart_failure and SBP
###########################################################################

# Redefine included variables
continuousvariable <- c("O2SAT",
                        "RR",
                        # "SBP",
                        "DBP",
                        "HR",
                        "Weight",
                        # "Height",
                        "WCC",
                        "Hb",
                        "Plts",
                        "Na",
                        "K",
                        "Cr",
                        "Ur",
                        "CRP",
                        "Alb",
                        "Glucose",
                        "Lactate",
                        "BNP")

discretevariable <- c("AF",
                      "CAD",
                      "Hypertension",
                      # "Heart_Failure",
                      "Valvular_Heart_Disease",
                      "Pacemaker",
                      "CVA",
                      "PVD",
                      "Anaemia",
                      "Asthma",
                      "COPD",
                      "Diabetes",
                      "CKD",
                      "Thyroid_Disease",
                      "Rheumatological_Disease",
                      "VTE",
                      "Mental_Health",
                      "ACEi",
                      "ARB",
                      "MRA",
                      "SacubitrilValsartan",
                      "Thiazide",
                      "LoopDiuretic",
                      "Amiloride",
                      "BBlocker",
                      "Statin",
                      # "Hydralazine",
                      "Nitrates")

# Repeat PFA process
# Combine scaled continuous data with encoded categorical data
combined_data <- cbind((ML_dataset %>% dplyr::select(all_of(continuousvariable))), (ML_dataset %>% dplyr::select(discretevariable)), (ML_dataset %>% dplyr::select(AGE_ADMISSION, GENDER, ETHNICITY)))

# Perform PCA on the combined data - continuous data is already scaled
pca_result <- prcomp(combined_data, center = TRUE, scale. = FALSE)

# Get the loadings (contributions of each feature to the principal components)
loadings <- pca_result$rotation

# Determine the number of components to use (explained variance threshold)
explained_variance <- summary(pca_result)$importance[2, ]
cumulative_explained_variance <- cumsum(explained_variance)

# Decide on the number of principal components to use (e.g., explaining 70% of the variance)
# https://www.statisticssolutions.com/free-resources/directory-of-statistical-analyses/factor-analysis/#:~:text=Kaiser%20Criterion%3A%20An%20eigenvalue%20greater,threshold%20of%200.7%20or%20higher.
num_components <- which(cumulative_explained_variance >= 0.90)[1]

# Calculate the contributions of each feature to the dplyr::selected principal components
feature_contributions <- apply(loadings[, 1:num_components]^2, 1, sum)
print(sort(feature_contributions, decreasing = TRUE))

# Select features with the highest contributions
# Choose a threshold or select the top N features
threshold <- quantile(feature_contributions, 0.5)  # Top 50% most contributing features
selected_features <- names(sort(feature_contributions[feature_contributions >= threshold], decreasing = TRUE))
selected_features_based_on_contributions <- names(sort(which(feature_contributions >= 0.05), decreasing = TRUE)) # this is the key line
unselected_features_based_on_contributions <- names(sort(which(feature_contributions < 0.05), decreasing = TRUE))
# Selection of this threshold is a balance of:-
# - Dimensionality Reduction: Selecting fewer variables to simplify models and reduce overfitting.
# - Variance Retention: Ensuring selected variables collectively explain enough variance in the dataset.
total_explained_variance_by_all_features <- cumsum(sort(feature_contributions, decreasing = TRUE))
total_explained_variance_by_selected_features <- cumsum(sort(feature_contributions[feature_contributions >= threshold], decreasing = TRUE))
## See Data Plan for difference between cumulative variance threshold and quantile threshold

# Alternatively, select the top N features
top_n <- 20
selected_features_top_n <- names(sort(feature_contributions, decreasing = TRUE)[1:top_n])

# Print selected features
print(selected_features)
print(selected_features_based_on_contributions)
# 27 features selected - chosen to significantly describe 90% of the variance in the dataset
print(selected_features_top_n)

# Subset the original data to include only the selected features - imputed dataset used as continuous variables aren't normalised
selected_data <- imputed_dataset[, selected_features_based_on_contributions]
complete_selected_data <- imputed_dataset[, -which(names(imputed_dataset) %in% unselected_features_based_on_contributions)]
# Remove those we feature selected out
complete_selected_data <- complete_selected_data[, -which(names(complete_selected_data) %in% c("Heart_Failure", "Hydralazine", "Height", "SBP"))]
## In summary, I have selected principal components which explain 90% of the variance in the dataset - I have then selected features which explain up to a minimum of 5% (using p<0.05 logic) of these principal components
