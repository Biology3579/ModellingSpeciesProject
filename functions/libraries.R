## Script name: libraries.R
##
## Purpose of script: 
##    A function to load all necessary libraries for modelling species distributions, 
##    to declutter the main analysis file. An outline of the purpose of each library is also provided.
##    A summary of key libraries is also included in each functions script, 
##    but here they are all combined for ease of management.
##
## Author: Biology3579
##
## Date Created: 2025-02-26 
##
## ---------------------------

# IMPORTANT: The packages should be installed – with the correct version history – 
# using the `renv::restore()` function in the main script. 
# Run that line *before* using this command to ensure all package versions are correct.

# Function to load required libraries for species distribution modelling ----
load_libraries <- function() {
  
  # Core data handling and wrangling ----
  library(here)          # For constructing relative file paths (used throughout for file loading/saving)
  library(tidyverse)     # Meta-package bundling core tidy tools (includes dplyr, tidyr, ggplot2, etc.)
  library(dplyr)         # Data manipulation: filter(), mutate(), summarise() – used in cleaning and combining datasets
  library(tidyr)         # Reshaping data: pivot_longer(), pivot_wider() – helpful for model input prep
  library(tibble)        # Modern reimagining of data.frames – used implicitly throughout tidyverse workflows
  library(magrittr)      # Pipes (%>% and %<>%) for clearer, chained code – used everywhere in data pipelines
  
  # Spatial and geographic data ----
  library(sf)            # For vector-based spatial data (points, polygons, shapefiles) – used in spatial transforms and plotting
  library(sp)            # Older spatial class system (still used by dismo and raster) – needed for backwards compatibility
  library(raster)        # Raster data handling – used in earlier workflows or legacy code
  library(terra)         # Modern raster handling (e.g., environmental layers, masking, cropping) – preferred over raster
  library(geodata)       # Easy access to global environmental datasets (e.g., WorldClim bioclim layers) – used for downloading predictors
  library(dismo)         # Species distribution modelling tools – used for pseudo-absence generation, evaluation metrics (AUC, etc.)
  library(rworldmap)     # Quick visualisation of world maps – less common if using rnaturalearth but handy for debugging
  library(rgbif)         # Interface to GBIF API – used in downloading occurrence records in `load_gbif_data()`
  library(rnaturalearth) # High-quality vector data for countries and borders – used in mapping functions
  library(rnaturalearthdata) # Required data backend for rnaturalearth (needed for `ne_countries()` calls)
  
  # Visualisation tools ----
  library(ggplot2)       # Core plotting library – used for all visualisation functions (species maps, predictions, overlap)
  library(viridis)       # Colourblind-friendly palettes – used in plotting GLM probability rasters
  
  # Machine learning and model evaluation ----
  library(caret)         # Framework for model training, cross-validation, and tuning – useful if building GLMs, RFs, etc.
  
  # Reproducibility and environment management ----
  library(renv)          # Tracks and restores project-specific package versions – used to ensure reproducibility across sessions
}
  