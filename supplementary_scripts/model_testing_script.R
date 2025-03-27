## Script Name: model_testing_script.R
##
## Purpose of script: 
##    Evaluate and compare multiple GLMs for species distribution modeling.
##    Select the best model using AIC and K-Fold Cross-Validation (AUC).
##
## Author: Biology3579
##
## Date Created: 2025-02-26 
## ------------------------------------------------------------

# Load Required Libraries ----
library(terra)   # Handling raster and spatial data
library(dplyr)   # Data manipulation
library(caret)   # Cross-validation utilities
library(dismo)   # Model evaluation metrics

## Function to Fit Multiple GLMs ----

# This function fits multiple GLMs using a list of formulas and returns the models.
fit_glm_models <- function(data, formulas) {
  lapply(formulas, function(formula) glm(formula, data = data, family = binomial(link = "logit")))
}

## Fucntion to Select Best Model Based on AIC ----

# This function selects the model with the lowest AIC.
select_best_model_aic <- function(models) {
  
  # Compute AIC values for all models
  aic_values <- sapply(models, AIC)
  
  # Identify the best model (lowest AIC)
  best_model <- names(which.min(aic_values))
  
  # Return AIC values and best model name
  return(list(aic_values = aic_values, best_model = best_model))
}


## Function to Perform K-Fold Cross-Validation for GLMs ----

# This function performs K-fold cross-validation and selects the best model based on AUC.
cross_validate_glm <- function(data, formulas, k = 5, seed = 123) {
  
  set.seed(seed)  # Ensure reproducibility
  
  # Create K folds (balanced distribution of presence points)
  folds <- createFolds(data$presence, k = k, list = TRUE)
  
  # Store AUC results for each model
  auc_results <- matrix(NA, nrow = k, ncol = length(formulas))
  colnames(auc_results) <- names(formulas)
  
  # Loop through each fold
  for (i in seq_len(k)) {
    train_indices <- unlist(folds[-i])  # Training on k-1 folds
    test_indices <- folds[[i]]          # Testing on the remaining fold
    
    train_data <- data[train_indices, ]
    test_data <- data[test_indices, ]
    
    # Fit and evaluate each model
    for (j in seq_along(formulas)) {
      model <- glm(formulas[[j]], data = train_data, family = binomial(link = "logit"))
      
      # Predictions
      pred_test <- predict(model, test_data, type = "response")
      pred_background <- predict(model, data[data$presence == 0, ], type = "response")
      
      # Compute AUC and store results
      auc_results[i, j] <- evaluate(pred_test, pred_background)@auc
    }
  }
  
  # Compute mean AUC across all folds
  mean_auc <- colMeans(auc_results, na.rm = TRUE)
  
  # Identify the best model based on highest AUC
  best_model <- names(which.max(mean_auc))
  
  return(list(auc_values = mean_auc, best_model = best_model))
}

## Testing Date Palm Models ----

# Define GLM Formulas for Date Palm ---
palm_model_formulas <- list(
  full_model = presence ~ bio5 + bio6 + bio14 + bio16,
  model1 = presence ~ bio5 + bio6 + bio14,
  model2 = presence ~ bio5 + bio6 + bio16,
  model3 = presence ~ bio5 + bio14 + bio16,
  model4 = presence ~ bio6 + bio14 + bio16
)

# Fit GLMs ---
palm_glm_models <- fit_glm_models(palm_model_data, palm_model_formulas)

# Select Best Model Using AIC ---
palm_aic_results <- select_best_model_aic(palm_glm_models)

cat("\n--- Date Palm Model Selection ---\n")
cat("AIC Values:\n")
print(palm_aic_results$aic_values)
cat("Best Model (AIC):", palm_aic_results$best_model, "\n")

# Run K-Fold Cross-Validation ---
palm_cv_results <- cross_validate_glm(palm_model_data, palm_model_formulas)

cat("\nCross-Validation Results (Date Palm):\n")
cat("Mean AUC Values:\n")
print(palm_cv_results$auc_values)
cat("Best Model Based on AUC:", palm_cv_results$best_model, "\n")


## Testing Red Palm Weevil Models ----

# --- Define GLM Formulas for Red Palm Weevil ---
weevil_model_formulas <- list(
  full_model = presence ~ bio2 + bio6 + bio16 + bio17,
  model1 = presence ~ bio2 + bio6 + bio16,
  model2 = presence ~ bio2 + bio6 + bio17,
  model3 = presence ~ bio2 + bio16 + bio17,
  model4 = presence ~ bio6 + bio16 + bio17
)

# Fit GLMs ---
weevil_glm_models <- fit_glm_models(weevil_model_data, weevil_model_formulas)

# Select Best Model Using AIC ---
weevil_aic_results <- select_best_model_aic(weevil_glm_models)

cat("\n--- Red Palm Weevil Model Selection ---\n")
cat("AIC Values:\n")
print(weevil_aic_results$aic_values)
cat("Best Model (AIC):", weevil_aic_results$best_model, "\n")

# --- Run K-Fold Cross-Validation ---
weevil_cv_results <- cross_validate_glm(weevil_model_data, weevil_model_formulas)

cat("\nCross-Validation Results (Red Palm Weevil):\n")
cat("Mean AUC Values:\n")
print(weevil_cv_results$auc_values)
cat("Best Model Based on AUC:", weevil_cv_results$best_model, "\n")

