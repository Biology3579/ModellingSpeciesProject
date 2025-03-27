## Script name: loading_and_processing_species_data.R
##
## Purpose of script: 
##    A collection of functions for loading GBIF species data and processing the
##    dataset for analysis, to declutter the main analysis file.
##
## Author: Biology3579
##
## Date Created: 2025-03-12
##

# Load required libraries ----
library(here)       # File paths
library(sf)         # Spatial vector data handling
library(terra)      # Raster data handling
library(dplyr)      # Data manipulation
library(sp)         # Spatial data (legacy support)
library(raster)     # Raster operations (legacy support)
library(dismo)      # Species distribution modelling
library(rgbif)      # GBIF data access

# Loading GBIF Species Data ----
# This function loads GBIF species data from local storage or downloads it if not already present in the specified directory.

load_gbif_data <- function(species_name) {
  file_path <- here("data", "raw", paste0(gsub(" ", "_", species_name), "_raw.rds"))  # Build file path using species name
  
  if (!file.exists(file_path)) {  # Check if the .rds file already exists locally
    species_data <- occ_search(scientificName = species_name,   # Query GBIF for the given species
                               hasCoordinate = TRUE,     # Only include records with coordinates
                               limit = 10000)[["data"]]   # Limit results to 10,000 and extract data
    saveRDS(species_data, file_path)  # Save the retrieved data as an .rds file
  } else {
    species_data <- readRDS(file_path)  # Load the existing .rds file from local storage
  }
  
  return(species_data)  # Return the species data as a dataframe
}

# Cleaning Species Occurrence Data ----
# This function cleans the raw GBIF data by selecting, renaming, filtering, and deduplicating occurrence records.
clean_species_data <- function(raw_data) {
  raw_data %>%
    dplyr::select(decimalLongitude, decimalLatitude, gbifID) %>%  # Select only longitude, latitude, and unique ID
    rename(lon = decimalLongitude, lat = decimalLatitude) %>%   # Rename columns to 'lon' and 'lat'
    filter(!is.na(lon) & !is.na(lat)) %>%   # Filter out rows with missing coordinates
    distinct(lon, lat, .keep_all = TRUE)    # Remove duplicates based on location
}


# Filtering Data to Study Region ----
# This function filters species coordinates to those within a bounding box (study region).

filter_coords <- function(clean_data) {
  clean_data %>%
    filter(
      lon >= xmin(study_region), lon <= xmax(study_region),  # Filter longitudes within bounds
      lat >= ymin(study_region), lat <= ymax(study_region)  # Filter latitudes within bounds
    )
}



# Removing Ocean Points ----
# This function removes coordinates that fall in the ocean using a shapefile of global ocean polygons.

get_mainland_points <- function(clean_data) {
  ocean_data_dir <- here("data", "raw", "ocean")    # Define directory to store ocean data
  if (!dir.exists(ocean_data_dir)) dir.create(ocean_data_dir)  # Create directory if it doesn't exist
  
  ocean_url <- "https://naturalearth.s3.amazonaws.com/110m_physical/ne_110m_ocean.zip"  # Ocean shapefile URL
  zip_file <- file.path(ocean_data_dir, basename(ocean_url))  # Path to downloaded zip file
  
  if (!file.exists(zip_file)) {
    download.file(ocean_url, zip_file, mode = "wb") # Download zip file if not already present
  }
  
  ocean_files <- unzip(zip_file, exdir = ocean_data_dir)      # Unzip shapefile contents
  oceans <- read_sf(grep("shp$", ocean_files, value = TRUE))    # Read the .shp file as an sf object
  
  coords_sf <- st_as_sf(clean_data, coords = c("lon", "lat"), crs = 4326)  # Convert input data to spatial format
  oceans <- st_make_valid(oceans)    # Ensure ocean geometry is valid
  oceans <- st_transform(oceans, crs = st_crs(coords_sf))   # Match CRS with input data
  
  sf_use_s2(FALSE)  # Disable spherical geometry engine for this step
  ocean_intersections <- sapply(
    st_intersects(coords_sf, oceans), # Find intersections between points and ocean
    function(x) if (length(x) == 0) NA_integer_ else x[1]  # Assign NA if no intersection
  )
  
  land_coords <- coords_sf[is.na(ocean_intersections), ]  # Keep only points not intersecting ocean
  
  coords_matrix <- st_coordinates(land_coords)   # Extract coordinate matrix
  land_coords_df <- land_coords %>%
    st_drop_geometry() %>%    # Remove spatial geometry
    mutate(lon = coords_matrix[, 1], lat = coords_matrix[, 2])  # Add back lon/lat as columns
  
  return(land_coords_df)   # Return cleaned dataframe
}


# Generating Pseudo-Absence Data ----
# This function generates random points within the study region to serve as pseudo-absences.
# These are filtered to exclude ocean points using the `get_mainland_points()` function.

# Generate random pseudo-absence points within the study region and exclude ocean points.

generate_pseudo_absence_data <- function(presence_data, num_points = 500) {
  set.seed(999)          # Set seed for reproducibility
  
  study_area_sf <- st_as_sf(as.polygons(study_area_mask))  # Convert study area raster mask to polygons
  
  pseudo_absence_points <- st_sample(study_area_sf, size = num_points, type = "random") %>%  # Generate random points
    st_as_sf() %>%
    st_transform(crs = 4326)  # Transform to geographic coordinates
  
  pseudo_absence_points_df <- pseudo_absence_points %>%
    st_coordinates() %>% # Extract coordinates
    as.data.frame() %>%
    rename(lon = X, lat = Y) %>%  # Rename coordinate columns
    mutate(presence = 0) # Label as pseudo-absences
  
  combined_data <- bind_rows(
    presence_data %>% mutate(presence = 1), # Add presence label to actual data
    pseudo_absence_points_df  # Combine with pseudo-absence data
  )
  
  combined_data_sf <- st_as_sf(combined_data, coords = c("lon", "lat"), crs = 4326)  # Convert to spatial format
  clean_combined_data <- get_mainland_points(combined_data_sf)  # Remove any points falling in ocean
  
  return(clean_combined_data) # Return cleaned dataset
}

# Extracting Bioclimatic Variables ----
# This function extracts selected bioclim variables from a global raster at species points.

selected_bioclims <- function(distribution_data, selected_bioclims) {
  extracted_bioclims <- terra::extract(
    bioclim_data[[selected_bioclims]], # Access specified bioclim layers
    distribution_data[, c("lon", "lat")],  # Use lon/lat for extraction
    xy = FALSE # Do not include coordinates in output
  )
  
  combined_data <- cbind(distribution_data, extracted_bioclims) # Combine with original dataset
  return(combined_data) # Return full dataframe
}

# Buffering Around Palm Points ----
# This function creates a buffer around palm presence points and returns both buffered area and weevil data.

process_species_data <- function(palm_presence_data, weevil_presence_data, buffer_distance = 10000, proj_crs = 3857) {
  palm_sf <- st_as_sf(palm_presence_data, coords = c("lon", "lat"), crs = 4326) # Convert palm data to sf
  weevil_sf <- st_as_sf(weevil_presence_data, coords = c("lon", "lat"), crs = 4326) # Convert weevil data to sf
  
  palm_sf_proj <- st_transform(palm_sf, crs = proj_crs) # Project palm data to metric CRS
  weevil_sf_proj <- st_transform(weevil_sf, crs = proj_crs) # Project weevil data to same CRS
  
  palm_buffer <- st_buffer(palm_sf_proj, dist = buffer_distance) # Create buffer around palm points
  
  palm_buffer <- st_transform(palm_buffer, crs = proj_crs) # Ensure buffer is in correct CRS
  weevil_sf_proj <- st_transform(weevil_sf_proj, crs = proj_crs) # Reconfirm weevil data CRS
  
  return(list(palm_buffer = palm_buffer, weevil_sf_proj = weevil_sf_proj)) # Return buffer and weevil data
}
