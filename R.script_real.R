# ============================================
#        Species Distribution Modelling
# ============================================

# A practical guide to modeling species distributions using R
# Originally by Tim Barraclough (tim.barraclough@biology.ox.ac.uk)
# Modified by Nilo M. Recalde (nilo.recalde@biology.ox.ac.uk)
# Based on tutorials from: https://rspatial.org/raster/sdm/

# This script demonstrates how to:
# 1. Download and process world map data
# 2. Obtain species locality data from GBIF (Global Biodiversity Information Facility)
# 3. Access climate data from WorldClim
# 4. Model species distributions based on climate variables
# 5. Predict future distributions under climate change scenarios

# The approach used here employs basic linear modelling to identify which climate variables 
# best predict current species distributions, project how distributions might shift with 
# future climate change, and visualize results using maps. Note that while this script uses 
# simple linear models, many other approaches exist for species distribution modeling (SDM).

# Load the libraries providing functions for spatial data handling and mapping.
# If you haven't installed these packages yet, see the setup script.
library(here)
library(dismo)
library(rworldmap)
library(sf)
library(geodata)
library(rgbif)
library(tidyverse) # works well for pipes! (rather than pipes)
library(terra)

# Now let's create a simple project structure if it doesn't exist:
# This script ensures that the required directories (data/raw, data/processed, and output) 
# exist in the project directory. If they don’t, it creates them:
#   create a folder called data 
sapply(c("data/raw", "data/processed", "output"), function(dir) { #creating different folders in the directory 
  dir_path <- here(dir)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
}) 

# ====================================
# Getting, Plotting and Cleaning data 
# ====================================

# a) WORLD MAP

#Essentially this creates a wolrd map that can then be used to plot the species distributions on 
wrld_simpl <- getMap(# getMap(resolution = "coarse") is a function from the {rworldmap} package that retrieves a spatial object representing a world map.
  resolution = "coarse") # resolution means a simplified version of the map with fewer details (faster to render).
plot(wrld_simpl) # The result is stored in wrld_simpl, which is a spatial polygons object. Plots this!!


# b) Download locality data for a species from GBIF and trim


# Access records from GBIF (Global Biodiversity Information Facility) for a species using the dismo package.
# I am looking at , the spruce species used for ... production.
# The function gbif() downloads data for a species, and the geo argument specifies that we want only records 
# with geographic coordinates.


#!! Ensure this aprt is reproducible - so that no matter what species you use, you could repeat it all 
  # Indeed make functions and code that are equal for species 1 and species 2 - all you have to do is change the initial code!

#Assessing data for Palm Date (Phoenix dactylifera)

Phoenix_gbif_file <- here("data", "raw", "Phoenix_gbif.rds") # creating a data file with the folder; (here ensures the path is relative to the project root).
if (!file.exists(Phoenix_gbif_file)) { # this checks if the file already exists at the specified path.
  Phoenix_gbif <- gbif("Phoenix", "dactylifera", geo = TRUE) # extracting the species from GBIF - from the dismo 
  saveRDS(Phoenix_gbif, Phoenix_gbif_file)
} else { #if the file does exist then just read it 
  Phoenix_gbif <- readRDS(Phoenix_gbif_file) #read it 
}

#Assessing data for the red palm weevil, (Rhynchophorus ferrugineus)

Rhynchophorus_gbif_file <- here("data", "raw", "Rhynchophorus_gbif.rds") # creating a data file with the folder; (here ensures the path is relative to the project root).
if (!file.exists(Rhynchophorus_gbif_file)) { # this checks if the file already exists at the specified path.
  Rhynchophorus_gbif <- gbif("Rhynchophorus", "ferrugineus", geo = TRUE) # extracting the species from GBIF - from the dismo 
  saveRDS(Rhynchophorus_gbif, Rhynchophorus_gbif_file)
} else { #if the file does exist then just read it 
  Rhynchophorus_gbif <- readRDS(Rhynchophorus_gbif_file) #read it 
}

#Clean the data 
#turn this into a pipe! maybe put all as tidyverse!!
species_1_coords <- cbind(Phoenix_gbif$lon, Phoenix_gbif$lat) %>%
  na.omit() %>%
  data.frame() %>%
  rename("lon" = X1, "lat" = X2)


# Plot on world country map, setting the xlim and ylim to span the range of longitudes and latitudes
plot(wrld_simpl, xlim = range(species_1_coords$lon), ylim = range(species_1_coords$lat), axes = TRUE, col = "light yellow")
# Add points for this species
points(species_1_coords, col = "red", cex = 0.75)

#Clean the data 
#turn this into a pipe! maybe put all as tidyverse!!
species_2_coords <- cbind(Rhynchophorus_gbif$lon, Rhynchophorus_gbif$lat) %>%
  na.omit() %>%
  data.frame() %>%
  rename("lon" = X1, "lat" = X2)


# Plot on world country map, setting the xlim and ylim to span the range of longitudes and latitudes
plot(wrld_simpl, xlim = range(species_2_coords$lon), ylim = range(species_2_coords$lat), axes = TRUE, col = "light yellow")
# Add points for this species
points(species_1_coords, col = "blue", cex = 0.75)


## RESTRICT MAP 

#Restrict the map for species 1 

trim.coords <- function(x, latmin, latmax, lonmin, lonmax) {
  x[x$lon >= lonmin & x$lon <= lonmax & x$lat >= latmin & x$lat <= latmax,] # include values of x where there is bigger and lwss than lon and lat 
}
species_1_coords_trim <- trim.coords(species_1_coords, latmin = -5, latmax = 45, lonmin = -20, lonmax = 60)

# Plot world map again now using the trimmed data:
plot(wrld_simpl, xlim = range(species_1_coords_trim$lon), ylim = range(species_1_coords_trim$lat), axes = TRUE, col = "light yellow")
# And add points for this species
points(species_1_coords_trim, col = "red", cex = 0.75)

#restrict map for species 2
trim.coords <- function(x, latmin, latmax, lonmin, lonmax) {
  x[x$lon >= lonmin & x$lon <= lonmax & x$lat >= latmin & x$lat <= latmax,] # include values of x where there is bigger and lwss than lon and lat 
}

# Then use the function to make a new table of coordinates, just within the ranges specified
species_2_coords_trim <- trim.coords(species_2_coords, latmin = -5, latmax = 45, lonmin = -20, lonmax = 60)

# Plot world map again now using the trimmed data:
plot(wrld_simpl, xlim = range(species_2_coords_trim$lon), ylim = range(species_2_coords_trim$lat), axes = TRUE, col = "light yellow")
# And add points for this species
points(species_2_coords_trim, col = "blue", cex = 0.75)



# Plot world map once
plot(wrld_simpl, 
     xlim = range(c(species_1_coords_trim$lon, species_2_coords_trim$lon)), 
     ylim = range(c(species_1_coords_trim$lat, species_2_coords_trim$lat)), 
     axes = TRUE, 
     col = "light yellow")

# Add points for both species with transparency and different shapes
points(species_1_coords_trim, col = adjustcolor("red", alpha.f = 0.6), pch = 16, cex = 1)  # Red, larger, semi-transparent
points(species_2_coords_trim, col = adjustcolor("blue", alpha.f = 0.6), pch = 16, cex = 1) # Blue, different shape, semi-transparent


# c) REMOVE OCEANIC AND ISLANDIC DATA
# # We only want to consider the continental land area, so let's to remove the ocean areas from the map.
# We can use the ocean shapefile from the Natural Earth dataset to do this.
# 

#!! Make sure that the clean data gets stored in the processed data file 
# Remember to refine this ocean section to ensure that its not arbitrary removal or - at least that this is explained

library(sf)
library(here)

# Download ocean data
ocean_data_dir <- here("data", "raw", "ocean")
if (!dir.exists(ocean_data_dir)) dir.create(ocean_data_dir)
URL <- "https://naturalearth.s3.amazonaws.com/110m_physical/ne_110m_ocean.zip"
zip_file <- file.path(ocean_data_dir, basename(URL))
if (!file.exists(zip_file)) {
  download.file(URL, zip_file)
}

# Unzip to ocean data directory and read shapefile
files <- unzip(zip_file, exdir = ocean_data_dir)
oceans <- read_sf(grep("shp$", files, value = TRUE))

# Load country borders (to identify mainland areas)
land_data_dir <- here("data", "raw", "land")
if (!dir.exists(land_data_dir)) dir.create(land_data_dir)
land_url <- "https://naturalearth.s3.amazonaws.com/110m_physical/ne_110m_land.zip"
land_zip_file <- file.path(land_data_dir, basename(land_url))
if (!file.exists(land_zip_file)) {
  download.file(land_url, land_zip_file)
}

# Unzip and read the land data
land_files <- unzip(land_zip_file, exdir = land_data_dir)
land <- read_sf(grep("shp$", land_files, value = TRUE))

# Convert coordinates to a spatial features (sf) object for GIS operations
species_1_coords_sf <- st_as_sf(species_1_coords_trim, coords = c("lon", "lat"))
# Set the coordinate reference system (CRS) to match the land and ocean data
st_crs(species_1_coords_sf) <- st_crs(land)
sf_use_s2(FALSE)  # Disable spherical geometry

# Ensure the ocean polygons are valid
oceans <- st_make_valid(oceans)

# 1. Find points that intersect with the ocean
ocean_intersections <- sapply(st_intersects(species_1_coords_sf, oceans), function(x) if (length(x) == 0) NA_integer_ else x[1])

# 1. Identify mainland by using landmass size (buffer mainland area to exclude small islands)
# Filter out small islands by keeping only large landmasses based on area threshold (e.g., 10,000 km²)
land_areas <- st_area(land)
land_areas_numeric <- as.numeric(land_areas)  # Extract numeric values from the area
large_land <- land[land_areas_numeric > 10000000, ]  # Keep landmasses > 10,000 km² (can adjust based on species' habitat)

# Create a buffer around mainland (landmasses)
mainland_buffer <- st_buffer(large_land, dist = 100000)  # Buffer by 100 km (can adjust)

# 3. Find points that are within the mainland buffer
mainland_intersections <- sapply(st_intersects(species_1_coords_sf, mainland_buffer), function(x) if (length(x) == 0) NA_integer_ else x[1])

# 4. Remove points that are either in the ocean or outside mainland buffer (i.e., on small islands)
valid_points <- !(!is.na(ocean_intersections) | is.na(mainland_intersections))

# Filter the species coordinates
species_1_coords_clean <- species_1_coords_sf[valid_points, ]
species_1_coords_clean <- data.frame(st_coordinates(species_1_coords_clean))
colnames(species_1_coords_clean) <- c("lon", "lat")

# Now plot again to check
plot(wrld_simpl, 
     xlim = range(species_1_coords_clean$lon), 
     ylim = range(species_1_coords_clean$lat), 
     col = "lightyellow", axes = TRUE)
points(species_1_coords_clean$lon, species_1_coords_clean$lat, col = "red", cex = 0.75)



# Download ocean data
ocean_data_dir <- here("data", "raw", "ocean")
if (!dir.exists(ocean_data_dir)) dir.create(ocean_data_dir)
URL <- "https://naturalearth.s3.amazonaws.com/110m_physical/ne_110m_ocean.zip"
zip_file <- file.path(ocean_data_dir, basename(URL))
if (!file.exists(zip_file)) {
  download.file(URL, zip_file)
}

# Unzip to ocean data directory and read shapefile
files <- unzip(zip_file, exdir = ocean_data_dir)
oceans <- read_sf(grep("shp$", files, value = TRUE))

# Load country borders (to identify mainland areas)
land_data_dir <- here("data", "raw", "land")
if (!dir.exists(land_data_dir)) dir.create(land_data_dir)
land_url <- "https://naturalearth.s3.amazonaws.com/110m_physical/ne_110m_land.zip"
land_zip_file <- file.path(land_data_dir, basename(land_url))
if (!file.exists(land_zip_file)) {
  download.file(land_url, land_zip_file)
}

# Unzip and read the land data
land_files <- unzip(land_zip_file, exdir = land_data_dir)
land <- read_sf(grep("shp$", land_files, value = TRUE))

# Convert coordinates to a spatial features (sf) object for GIS operations
species_2_coords_sf <- st_as_sf(species_2_coords_trim, coords = c("lon", "lat"))
# Set the coordinate reference system (CRS) to match the land and ocean data
st_crs(species_2_coords_sf) <- st_crs(land)
sf_use_s2(FALSE)  # Disable spherical geometry

# Ensure the ocean polygons are valid
oceans <- st_make_valid(oceans)

# 1. Find points that intersect with the ocean
ocean_intersections <- sapply(st_intersects(species_2_coords_sf, oceans), function(x) if (length(x) == 0) NA_integer_ else x[1])

# 1. Identify mainland by using landmass size (buffer mainland area to exclude small islands)
# Filter out small islands by keeping only large landmasses based on area threshold (e.g., 10,000 km²)
land_areas <- st_area(land)
land_areas_numeric <- as.numeric(land_areas)  # Extract numeric values from the area
large_land <- land[land_areas_numeric > 10000000, ]  # Keep landmasses > 10,000 km² (can adjust based on species' habitat)

# Create a buffer around mainland (landmasses)
mainland_buffer <- st_buffer(large_land, dist = 100000)  # Buffer by 100 km (can adjust)

# 3. Find points that are within the mainland buffer
mainland_intersections <- sapply(st_intersects(species_2_coords_sf, mainland_buffer), function(x) if (length(x) == 0) NA_integer_ else x[1])

# 4. Remove points that are either in the ocean or outside mainland buffer (i.e., on small islands)
valid_points <- !(!is.na(ocean_intersections) | is.na(mainland_intersections))

# Filter the species coordinates
species_2_coords_clean <- species_2_coords_sf[valid_points, ]
species_2_coords_clean <- data.frame(st_coordinates(species_2_coords_clean))
colnames(species_2_coords_clean) <- c("lon", "lat")

# Now plot again to check
plot(wrld_simpl, 
     xlim = range(species_2_coords_clean$lon), 
     ylim = range(species_2_coords_clean$lat), 
     col = "lightyellow", axes = TRUE)
points(species_2_coords_clean$lon, species_2_coords_clean$lat, col = "blue", cex = 0.75)



# Plot the world map
plot(world_map, 
     xlim = range(c(species_1_coords_clean$lon, species_2_coords_clean$lon)), 
     ylim = range(c(species_1_coords_clean$lat, species_2_coords_clean$lat)), 
     col = "lightyellow", axes = TRUE)

# Add points for species 1 (red circles) and species 2 (blue triangles)
points(species_1_coords_clean$lon, species_1_coords_clean$lat, col = "red", cex = 0.75, pch = 16)  # Red circles for species 1
points(species_2_coords_clean$lon, species_2_coords_clean$lat, col = "blue", cex = 0.75, pch = 16) # Blue triangles for species 2


## ==================================
# 1) Species 1 Distribution Modeling
# ===================================
# Run linear models to predict the present-day distribution of species 1 using climate variables and use them to present a map of its current distribution. 
# Which set of climatic variables best explain the current distribution of the species?
# 
# 

# The bio.data object contains 19 bioclimatic variables from WorldClim
# Each variable represents different aspects of temperature and precipitation
# See full descriptions at: https://www.worldclim.org/data/bioclim.html
# 
# Examples:
# BIO1 = Annual Mean Temperature
# BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
# BIO3 = Isothermality (BIO2/BIO7) (×100)
# ...
#
# The data is stored as a RasterStack, where each layer is one bioclimatic variable, 
# each pixel represents a 10 arc-minute (~18.5km) grid cell, and the pixel values 
# contain the climate data

# the resolution is 18.5 km quite big - may not be very in tune with species populations (coarse reolution)
# there might be a better database for a specific region with greater resolution

# Define file path for cached bioclimatic data (to avoid redundant downloads)
bio_data_path <- here("data", "raw", "wc10.tif")

# Check if bioclimatic data already exists; if not, download it
if (!file.exists(bio_data_path)) {
  bio.data <- worldclim_global(var = "bio", res = 10, path = here("data", "raw"))
  names(bio.data) <- paste0("bio", 1:19)
} else {
  bio.data <- rast(bio_data_path)  # Load existing raster
}

# Plot first two climate variables for visualization
#plot(bio.data, 1, main = "Annual Mean Temperature")
#plot(bio.data, 2, main = "Mean Diurnal Range")

# Ensure species_1_coords_trim is in the correct format (convert to SpatVector)
species_1_sf <- st_as_sf(species_1_coords_trim, coords = c("lon", "lat"), crs = 4326)  
species_1_spat <- vect(species_1_sf)  # Convert to SpatVector for terra

# Extract bioclimatic data for species locations
bio.values <- terra::extract(bio.data, species_1_spat)

# Combine extracted values with coordinates
species_1_data <- cbind(species_1_coords_trim, bio.values)

# Remove rows with any missing data
species_1_data <- na.omit(species_1_data)

# Check if row count is correct
if (nrow(species_1_data) > 0) {
  rownames(species_1_data) <- NULL  # Remove row names to avoid length mismatch errors
}


# Ensure species_1_data is a dataframe (remove spatial class if necessary)
species_1_data_df <- as.data.frame(species_1_data)

# Now plot the first two climate variables
plot(species_1_data_df[, 3], species_1_data_df[, 4], 
     xlab = names(species_1_data_df)[3], 
     ylab = names(species_1_data_df)[4], 
     main = "Climate Variable Correlation")



# You can extract bioclimatic data for the focal localities in Africa where your species is found
# filtering data for just the focal region
bio.values <- terra::extract(bio.data, species_1_coords_trim)[, -1]
rownames(bio.values) <- rownames(species_1_coords_trim)

# And plot them to see the range of environmental values the species lives in
plot(bio.values[, 1], bio.values[, 2])
# just visuakising correltaion but could be good to plot a linear model!

# Or look at how different variables correlate; here focusing on the first five:
pairs(bio.values[, 1:5])
# evryhting agaisnt evrtyhing - just to identify correlations!!
# if two explanatory variables are corralted they might itnroduce confoudnign variables 

# Append to lat lon, remove rows with missing data, and save to file for future use
species_1_data <- cbind(species_1_coords_trim, bio.values)
write.csv(species_1_data, file = here("data", "processed", "species_1_data.csv"), row.names = FALSE)

# Extract mean values, min and max, as a sanity check for the data
#     jsut to check for big mistakes 
rbind(
  mean = colMeans(species_1_data),
  min = apply(species_1_data, 2, min),
  max = apply(species_1_data, 2, max)
)

# =========================================================================
# 4) Model the species current distribution based on bioclimatic variables
# =========================================================================

# We will build a model to predict species presence (1) or absence (0) using bioclimatic variables (X). 
# However, we don’t have true absence data—locations where we are certain the species is not present. 
#   we only have presence data (not absence)
# Without absence data, the model cannot distinguish between areas where the species might be absent 
# and areas we simply haven’t surveyed. 
# To address this, we generate "pseudo-absence" data by sampling random background points from the region, 
# assuming these locations represent areas where the species is unlikely to occur. 
# Pseudo-absence is essential because it provides a contrast to presence data, 
# allowing the model to learn patterns associated with species occurrence.
#   We need data for absence and presence if we are to poredict how the species distribution chanegs over time 

# Question 1: What are the limitations of this approach?
#   Assuming that the random point extracted in the making of pseudo-absence really represents absence
#   Whilst it is likely that this assumption is correct (no sample data = no presence) it might not be the case at all!
#   So for example socio-political factors could prevent sampling 
#   Additionally, if it is growing in very rural and isoalted areas then it migth be less sampled 
# look at where the species is endemic! perhaps it is jsut sampled where there is a plantation!!
# 


# 4.1) Generate random background points for comparison

# Define study extent based on species occurrence data (with some buffer around it)
e <- extent(
  min(species_1_coords_trim$lon) - 5,
  max(species_1_coords_trim$lon) + 5,
  min(species_1_coords_trim$lat) - 5,
  max(species_1_coords_trim$lat) + 5
)

# Create a mask from the world map for the study region
mask <- rasterize(wrld_simpl, raster(e, res=0.5))

# Generate 500 random background points within the study region
bg <- randomPoints(mask, 500, ext=e)
colnames(bg) <- c("lon", "lat")

# Visualize the results
plot(crop(wrld_simpl, e), col="grey90", legend=FALSE, border = NA, 
     main="Species occurrences and background points")
points(bg, col="black", pch=20, cex=0.7)
points(species.coords, col="red", pch=20, cex=0.7)

# Question 2: Why is it better not to have too large a region for your background?
# The larger they are the larger the uncertainty and larger the error?
# The more likely there will actually be presence of the sepcies 
# 
# Question 3: Do you think C. arabica is really absent from all the
# localities selected as background points? Will it matter?
#   Np, it is unlikel to be abesnt forom all of those areas, because wuite liarhe areas and may o them 
#   Sampling efforts 
#   
#   This will matter if then we are predicting future scenarios; it needs to be considered 
#   The corealtions and assumptiosn we make might not be accurate and this our futrue preictions would eb worng 
# 

# Now we are settled on our area of extent, we can crop the bio.data to just keep values for this region
# It isn't essential but speeds up some later steps
bio.data <- crop(bio.data, e)


# 4.2 Next step: combine the presence data and the background data in one data frame

train <- rbind(species_1_coords_trim, bg)
# Create a vector of 1s and 0s to indicate presence/absence
pb_train <- c(rep(1, nrow(species.coords)), rep(0, nrow(bg)))
# Extract the bioclimatic data for the presence and background points
envtrain <- extract(bio.data, train)
envtrain <- data.frame(cbind(pa = pb_train, envtrain))
# And for each set separately
testpres <- data.frame(extract(bio.data, species.coords))
testbackg <- data.frame(extract(bio.data, bg))


# 4.3 Now we are ready to fit a logistic regression to predict presence/absence.
# We use a general linear model assuming binomial errors, which is appropriate
# for modelling a binary Y variable of 0s and 1s.
# More on this in Year 2 lecture Analysis of associations Part III: Non-Linear Regression Bonsall, Michael, and in Year 4!

# To start with I am including the first 5 bioclim variables as predictors, but you could include 1,2, or more.
gm1 <- glm(pa ~ bio1 + bio2 + bio3 + bio4 + bio5,
           family = binomial #binomial distribution for binary data ()
           (link = "logit"), data = envtrain
)

#correlation?? if you put them both in then you might not ...; reducing the power 

# Look at a summary of the results
summary(gm1)
# intercept = basiline probability 
# how much it deviates for, the estiamte
# relationshipo betwene the variabeks  

# Question 4: Which variables contribute significantly to explaining presence/absence?
# Here it says that all 5 variables seem to contribute signficnaty
# However this is likely to vayr under a different absnece data set!!
# This ussggetss that there is a lot of uncertainty 
# 
# Look at effect size!!! seemingly bio1 and bio2 and bio5!!
# 
# Finding the balacne!!
# 

# 4.4 Predict species distribution from the model and plot it
# Based on the model, we can predict the probability of occurrence of the species
# across the whole of the area being considered
pg <- predict(bio.data, gm1, ext = e, type = "response")
pg <- crop(pg, e)

# pg is a raster layer, like for our bioclim variables, but now representing the
# probability of occurrence from our linear model, for or area of extent e.

# Plot this
plot(pg, main = "GLM probability of occurrence")
# Add country boundaries
plot(wrld_simpl, add = TRUE, border = "dark grey")
#good for geo-political analyses
# Add our observed locality data
points(species.coords, col="red", pch=20, cex=1.5)

# Question 5: How well do you think the model has predicted the distribution?
#   it predcits it quite well - indeed species fgeenrally found in cliamtic hotspots 
#     howevrr it is worth thinking that perhaps artificlly planted and maiantiend
#     aslo worth notign that not all hotspots may have been smapled 
#     aslo worth notigh that cliamte data isnt everyhtign 

# If you want to show a single map of distribution instead, can convert the
# probabilities by selecting a threshold that gives the best match between predicted
# and observed localities.

# First, we evaluate how well the model predicts presence/absence at each point
ge <- evaluate(testpres, testbackg, gm1)
print(ge)

# The output gives several metrics such as:
# Area Under the Curve: AUC, and correlation coefficient between observed and predicted.
# Higher values of both metrics = better match between model predictions and observed
# presence/absences.

# Then we use this evaluation to pick a threshold probability for defining presence/absence
# using the model that gives the most accurate match to observed presence/absence
tr <- threshold(ge, "prevalence")
plot(pg > tr, main = "presence/absence")
plot(wrld_simpl, add = TRUE, border = "dark grey")
points(species.coords, col="red", pch=20, cex=1.5)

# 4.5 You can construct and compare different models

# This model uses next 5 climatic variables instead
gm2 <- glm(pa ~ bio6 + bio7 + bio8 + bio9 + bio10, family = binomial(link = "logit"), data = envtrain)
summary(gm2)

# You can compare two models, even with different predictor variables, using the Akaike Information Criterion
AIC(gm1, gm2)

# AIC helps compare model fit vs complexity (lower = more parsimonious)
# While differences >2 units suggest meaningful differences between models,
# model selection should be guided by theory and research questions,
# not just by AIC or p-values. Consider multiple evaluation metrics and
# use cross-validation when possible, and give enough thought to what your aims are: do you want to 
# predict well, or understand the underlying biology?

# You can try fitting different models with different predictor variables, but
# remember that the more variables you test, the more likely you are to overfit the observed data, which can 
# lead to poor predictions on new data.

# You can also compare metrics of how well the model predicts the data
evaluate(testpres, testbackg, gm1)
evaluate(testpres, testbackg, gm2)

# Question 6: Which bioclimatic variables do you expect to be most important for the species?
# Does a model including these variables predict distribution better than alternatives?











# Define x and y limits based on both species' coordinates
xlims <- range(c(palm_presence_data$lon, weevil_presence_data$lon), na.rm = TRUE)
ylims <- range(c(palm_presence_data$lat, weevil_presence_data$lat), na.rm = TRUE)

# Plot world map
plot(wrld_simpl, 
     xlim = xlims, 
     ylim = ylims, 
     axes = TRUE, 
     col = "light yellow", 
     border = "gray50")

# Add Palm Presence Points (Red)
points(palm_presence_data$lon, palm_presence_data$lat, 
       col = adjustcolor("red", alpha.f = 0.5), 
       pch = 16, cex = 1)

#Add Weevil Presence Points (Blue)
points(weevil_presence_data$lon, weevil_presence_data$lat, 
       col = adjustcolor("blue", alpha.f = 0.5), 
       pch = 16, cex = 1)

# --- Add Legend ---
legend("topright", legend = c("Palm Presence", "Weevil Presence"), 
       col = c("red", "blue"), 
       pch = 16,
       pt.cex = 1, 
       bg = "white", 
       title = "Species Presence")

