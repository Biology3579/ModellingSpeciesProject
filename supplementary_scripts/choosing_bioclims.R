## Script Name: choosing_bioclims.R
##
## Purpose of script: 
##    Identifying the most ecologically relevant and uncorrelated 
##    bioclimatic variables for modeling species distributions.
##
## Author: Biology3579
##
## Date Created: 2025-02-26 
## ------------------------------------------------------------

# Load required libraries
library(terra)   # For handling raster data
library(dplyr)   # For data manipulation


## Selecting Initial Bioclimatic Variables for Date Palm ----


# Initial selection based on ecological relevance
palm_selected_bioclims_initial <- c("bio5",  # Max Temp of Warmest Month (Heat Stress)
                                    "bio6",  # Min Temp of Coldest Month (Cold Stress)
                                    "bio10", # Mean Temp of Warmest Quarter
                                    "bio12", # Annual Precipitation
                                    "bio14", # Precipitation of Driest Month (Drought)
                                    "bio16") # Precipitation of Wettest Quarter

# Extract the selected bioclimatic layers
bioclim_data_palm_subset <- bioclim_data[[palm_selected_bioclims_initial]]

# Extract values for Date Palm presence-absence locations
palm_bioclim_data <- terra::extract(bioclim_data_palm_subset, palm_pa_data[, c("lon", "lat")], xy = FALSE)


## Check for Multicollinearity in Date Palm Variables ----
 

# Generate pairwise scatterplots to visualize correlations
pairs(palm_bioclim_data, use = "complete.obs",
      main = "Pairwise Scatterplots of Initial Bioclimatic Variables",
      pch = 20)

# Compute correlation matrix
cor_matrix <- cor(palm_bioclim_data, use = "complete.obs")

# Identify highly correlated pairs (> 0.7)
high_corr_pairs <- which(abs(cor_matrix) > 0.7 & abs(cor_matrix) < 1, arr.ind = TRUE)
print(high_corr_pairs)

# Decision: Remove "bio10" and "bio12" (high collinearity with "bio5" and "bio16") whislt also les....



## Selecting Initial Bioclimatic Variables for Red Palm Weevil ----


# Initial selection based on ecological relevance
weevil_selected_bioclims_initial <- c("bio1",  # Annual Mean Temperature
                                      "bio2",  # Mean Diurnal Range
                                      "bio6",  # Min Temp of Coldest Month
                                      "bio16", # Precipitation of Wettest Quarter
                                      "bio17") # Precipitation of Driest Quarter

# Extract the selected bioclimatic layers
bioclim_data_weevil_subset <- bioclim_data[[weevil_selected_bioclims_initial]]

# Extract values for Red Palm Weevil presence-absence locations
weevil_bioclim_data <- terra::extract(bioclim_data_weevil_subset, weevil_pa_data[, c("lon", "lat")], xy = FALSE)


## Check for Multicollinearity in Red Palm Weevil Variables ----


# Generate pairwise scatterplots to visualize correlations
pairs(weevil_bioclim_data, use = "complete.obs",
      main = "Pairwise Scatterplots of Initial Weevil Bioclimatic Variables",
      pch = 20)

# Compute correlation matrix
cor_matrix <- cor(weevil_bioclim_data, use = "complete.obs")
print(cor_matrix)

# Identify highly correlated pairs (> 0.7)
high_corr_pairs <- which(abs(cor_matrix) > 0.7 & abs(cor_matrix) < 1, arr.ind = TRUE)
print(high_corr_pairs)

# Decision: Remove "bio1" (high collinearity with "bio6") whislt also ...
