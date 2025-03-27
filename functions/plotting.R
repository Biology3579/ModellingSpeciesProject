## Script name: plotting.R
##
## Purpose of script: 
##    This script contains a collection of functions for plotting and visualising
##    species presence, pseudo-absence, and model predictions. It is designed to 
##    declutter the main analysis file by keeping all figure-related functions separate.
##
## Author: Biology3579
##
## Date Created: 2025-03-12 
##
## ---------------------------

# Load required libraries ----
library(ggplot2)          # Data visualisation
library(rnaturalearth)    # World map data
library(rnaturalearthdata)
library(terra)            # Raster handling
library(sf)               # Spatial vector data
library(dplyr)            # Data wrangling
library(viridis)          # Colour palettes (colour-blind friendly)

# Define custom colours ----
## Colour scheme for plotting species
## These are all taken from the turbo ... for ...
palm_colour    <- "#440154"   
weevil_colour  <- "#9E1B32"    
absence_colour <- "gray60"     
overlap_colour <- "#1F8A70"   


# Plotting species distribution ----
# This function plots presence and pseudo-absence points on a world map background.

plot_species_distribution <- function(distribution_data, species_colour, title) {
  
  data <- distribution_data[, c("lon", "lat", "presence")] # Subset data to required columns
  presence_data <- filter(data, presence == 1) # Filter presence points
  absence_data  <- filter(data, presence == 0) # Filter pseudo-absence points
  
  x_limits <- range(data$lon, na.rm = TRUE) # Set x-axis limits from data range
  y_limits <- range(data$lat, na.rm = TRUE) # Set y-axis limits from data range
  
  world_map <- ne_countries(scale = "medium", returnclass = "sf")  # Load medium-scale world map as an sf object
  
  ggplot() +
    geom_sf(data = world_map, fill = "lightyellow", color = "gray40", linewidth = 0.2) +  # Plot base map
    geom_point(data = presence_data, aes(x = lon, y = lat), # Plot presence points
               color = species_colour, fill = species_colour,
               shape = 21, size = 2, alpha = 0.7, stroke = 0.8) +
    geom_point(data = absence_data, aes(x = lon, y = lat),   # Plot pseudo-absence points
               color = absence_colour, fill = absence_colour,
               shape = 21, size = 2, alpha = 0.7, stroke = 0.8) +
    scale_x_continuous(limits = x_limits, expand = expansion(mult = 0.05)) +  # Set x-axis range with padding
    scale_y_continuous(limits = y_limits, expand = expansion(mult = 0.05)) +  # Set y-axis range with padding
    labs(
      title = title,  # Add plot title
      x = "Longitude", # Label x-axis
      y = "Latitude",  # Label y-axis
    ) +
    theme_minimal() +  # Apply clean minimal theme
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14), # Style title
      plot.caption = element_text(hjust = 1, size = 8, colour = "gray50"),# Style caption
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3), # Light gridlines
      panel.grid.minor = element_blank(),  # No minor gridlines
      plot.margin = margin(10, 10, 10, 10)   # Add padding around plot for aesthetics
    ) +
    coord_sf(crs = 4326)  # Use WGS84 coordinate reference system for map projection
}


# Plotting GLM probability (without points) ----
# This function plots the predicted probability raster of occurrence from a GLM model 

plot_glm_probability_raster <- function(
    final_glm_model,
    climate_data,  # Raster stack of bioclimatic variables
    buffered_region,  # Spatial boundary to crop raster
    title,  # Title for the plot
    probability_col = "viridis"  # Colour palette for probability gradient
) {
  
  predicted_distribution <- predict(climate_data, final_glm_model, type = "response")  # Predict probability from model
  cropped <- crop(predicted_distribution, buffered_region)  # Crop raster to buffered study region
  
  raster_df <- as.data.frame(cropped, xy = TRUE, na.rm = TRUE)  # Convert raster to dataframe with lon/lat
  colnames(raster_df) <- c("lon", "lat", "probability") # Rename columns
  
  ggplot() +
    geom_tile(data = raster_df, aes(x = lon, y = lat, fill = probability)) +  # Plot probability as raster tiles
    scale_fill_viridis(option = probability_col, name = "Occurrence Probability") + # Add viridis colour scale
    labs(title = title, x = "Longitude", y = "Latitude") + # Set labels
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14), # Style title
      axis.title = element_text(face = "bold"), # Bold axis titles
      axis.text = element_text(size = 10), # Axis text size
      legend.title = element_text(face = "bold")# Bold legend title
    )
}


# Plotting GLM probability (with points) ----
# Similar to above, but overlays presence points on top of predicted probability raster.

plot_probability_raster_and_presence <- function(presence_data, final_glm_model, buffered_region, species_colour, title, probability_col = "viridis") {
  
  predicted_distribution <- predict(bioclim_data, final_glm_model, type = "response") # Predict probability from bioclim data
  cropped <- crop(predicted_distribution, buffered_region)  # Crop to buffered study region
  
  raster_df <- as.data.frame(cropped, xy = TRUE, na.rm = TRUE) # Convert raster to dataframe
  colnames(raster_df) <- c("lon", "lat", "probability") # Rename columns
  
  ggplot() +
    geom_tile(data = raster_df, aes(x = lon, y = lat, fill = probability)) +  # Plot probability raster
    geom_point(data = presence_data, aes(x = lon, y = lat),   # Overlay presence points
               color = species_colour, fill = species_colour,
               shape = 21, size = 2, alpha = 0.5, stroke = 1) +
    scale_fill_viridis(option = probability_col, name = "Occurrence Probability") + # Viridis colour scale
    labs(title = title, x = "Longitude", y = "Latitude") + # Plot labels
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # Title styling
      axis.title = element_text(face = "bold"),    # Axis title styling
      axis.text = element_text(size = 10), # Axis text size
      legend.title = element_text(face = "bold")   # Legend title styling
    )
}


# Plotting species overlap ----
# This function plots palm, weevil, and overlapping points on a map.

plot_species_overlap_points <- function(palm_points, weevil_points, overlap_points,
                                        title = "Palm and Weevil Overlap") {
  
  palm_geo    <- st_transform(palm_points, crs = 4326)   # Convert palm points to WGS84
  weevil_geo  <- st_transform(weevil_points, crs = 4326)   # Convert weevil points to WGS84
  overlap_geo <- st_transform(overlap_points, crs = 4326)   # Convert overlap points to WGS84
  
  coords_palm   <- st_coordinates(palm_geo)   # Extract coordinates for plotting limits
  coords_weevil <- st_coordinates(weevil_geo)
  coords_all    <- rbind(coords_palm, coords_weevil) # Combine coordinates to set plot bounds
  
  x_limits <- range(coords_all[, 1], na.rm = TRUE) # Define x-axis limits
  y_limits <- range(coords_all[, 2], na.rm = TRUE) # Define y-axis limits
  
  world_map <- ne_countries(scale = "medium", returnclass = "sf")  # Load basemap
  
  ggplot() +
    geom_sf(data = world_map, fill = "lightyellow", colour = "grey60", linewidth = 0.2) +  # Plot world map
    geom_sf(data = palm_geo, shape = 21, fill = palm_colour, colour = palm_colour,   # Palm points
            size = 1.5, alpha = 0.7, stroke = 0.2) +
    geom_sf(data = weevil_geo, shape = 21, fill = weevil_colour, colour = weevil_colour, # Weevil points
            size = 1.5, alpha = 0.7, stroke = 0.2) +
    geom_sf(data = overlap_geo, shape = 21, fill = overlap_colour, colour = overlap_colour,# Overlap points
            size = 3, alpha = 0.9, stroke = 0.4) +
    scale_x_continuous(limits = x_limits, expand = expansion(mult = 0.05)) +  # X-axis limits with padding
    scale_y_continuous(limits = y_limits, expand = expansion(mult = 0.05)) + # Y-axis limits with padding
    labs(
      title = title,   # Plot title
      x = "Longitude",  # X-axis label
      y = "Latitude",   # Y-axis label
      caption = paste("Total weevils:", nrow(weevil_geo), "| Overlapping:", nrow(overlap_geo))  # Caption text
    ) +
    theme_minimal() +
    theme(
      plot.title   = element_text(hjust = 0.5, face = "bold", size = 14), # Title styling
      plot.caption = element_text(hjust = 1, size = 8, colour = "grey50"), # Caption styling
      panel.grid.major = element_line(colour = "grey90", linewidth = 0.3), # Light gridlines
      panel.grid.minor = element_blank()    # No minor gridlines
    )
  }
