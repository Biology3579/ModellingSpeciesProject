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

# Now let's create a simple project structure if it doesn't exist:
# This script ensures that the required directories (data/raw, data/processed, and output) 
# exist in the project directory. If they don’t, it creates them:
#   create a folder called data 
sapply(c("data/raw", "data/processed", "output"), function(dir) { #creating different folders in the directory 
  dir_path <- here(dir)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
}) 

# ==============================
# 1) Load world map and plot it
# ==============================

#Essentially this creates a wolrd map that can then be used to plot the species distributions on 
wrld_simpl <- getMap(# getMap(resolution = "coarse") is a function from the {rworldmap} package that retrieves a spatial object representing a world map.
  resolution = "coarse") # resolution means a simplified version of the map with fewer details (faster to render).
plot(wrld_simpl) # The result is stored in wrld_simpl, which is a spatial polygons object. Plots this!!


# ===========================================================
# 2) Download locality data for a species from GBIF and trim
# ===========================================================

# Access records from GBIF (Global Biodiversity Information Facility) for a species using the dismo package.
# I am looking at , the spruce species used for ... production.
# The function gbif() downloads data for a species, and the geo argument specifies that we want only records 
# with geographic coordinates.


#Assessing data for Palm Date (Phoenix dactylifera)

Phoenix_gbif_file <- here("data", "raw", "Phoenix_gbif.rds") # creating a data file with the folder; (here ensures the path is relative to the project root).
if (!file.exists(Phoenix_gbif_file)) { # this checks if the file already exists at the specified path.
  Phoenix_gbif <- gbif("Phoenix", "dactylifera", geo = TRUE) # extracting the species from GBIF - from the dismo 
  saveRDS(Phoenix_gbif, Phoenix_gbif_file)
} else { #if the file does exist then just read it 
  Phoenix_gbif <- readRDS(Phoenix_gbif_file) #read it 
}

#Assessing data for ed palm weevil, (Rhynchophorus ferrugineus)

Rhynchophorus_gbif_file <- here("data", "raw", "Rhynchophorus_gbif.rds") # creating a data file with the folder; (here ensures the path is relative to the project root).
if (!file.exists(Rhynchophorus_gbif_file)) { # this checks if the file already exists at the specified path.
  Rhynchophorus_gbif <- gbif("Rhynchophorus", "ferrugineus", geo = TRUE) # extracting the species from GBIF - from the dismo 
  saveRDS(Rhynchophorus_gbif, Rhynchophorus_gbif_file)
} else { #if the file does exist then just read it 
  Rhynchophorus_gbif <- readRDS(Ips_gbif_file) #read it 
}



















species.gbif_file_Picea <- here("data", "raw", "species.gbif.Picea.rds")

if (!file.exists(species.gbif_file_Picea)) {
  species.gbif_Picea <- gbif("Picea", "abies", geo = TRUE, nrecs = 20000)
  saveRDS(species.gbif_Picea, species.gbif_file_Picea)
} else {
  species.gbif_Picea <- readRDS(species.gbif_file_Picea)
}

species.gbif_Picea_df <- species.gbif_Picea$data  # Extract only the data frame
print(head(species.gbif_Picea_df))




# Define the file path
species.gbif_file_Picea <- here("data", "raw", "species.gbif.Picea.rds")

# Check if the file already exists
if (!file.exists(species.gbif_file_Picea)) { 
  # Extract species occurrence data from GBIF (limit records to 1000)
  species.gbif_Picea <- occ_search(
    scientificName = "Picea abies", 
    hasCoordinate = TRUE,   # Only records with coordinates
    limit = 10000            # Limit the records to 1000
  )
  
  # Save the results as an RDS file
  saveRDS(species.gbif_Picea, species.gbif_file_Picea)
}else { 
  # If the file already exists, load it
  species.gbif_Picea <- readRDS(species.gbif_file_Picea)
}


# Extract only the dataframe from the list
species.gbif_Picea <- species.gbif_Picea[["data"]]

# Save it as an RDS file
saveRDS(species.gbif_Picea , here("data", "raw", " species.gbif_Picea .rds"))

# Print the first few rows to confirm
print(head(species.gbif_Picea))

library(here)

library(dplyr)

# Define the file path using `here`
species.gbif_file_Picea <- here("data", "raw", "species.gbif.Picea.rds")

# Check if the file already exists, then extract and process data
if (!file.exists(species.gbif_file_Picea)) { 
  occ_search(
    scientificName = "Picea abies",
    hasCoordinate = TRUE,  # Only records with coordinates
    limit = 1000           # Maximum number of records
  ) %>%
    .[["data"]] %>%  # Extract only the data (occurrence data, without metadata)
    saveRDS(., here("data", "raw", "species_dataframe.rds")) %>%  # Save as an RDS file
    write.csv(., here("data", "raw", "species_dataframe.csv"), row.names = FALSE)  # Save as a CSV file
} else { 
  # If the file exists, load it
  species_dataframe <- readRDS(species.gbif_file_Picea)
}

# Print the first few rows to confirm
print(head(species_dataframe))



## Things that might be sueful if you are downlaoding the data
#file.copy(from = normalizePath("~/.Renviron"), to = here(".Renviron"), overwrite = TRUE)

#install.packages("usethis")
#usethis::edit_r_environ()

#GBIF_USER="biology3579"
#GBIF_PWD="Encarabincunquinada1248!"
#GBIF_EMAIL="candela.ferrerdiez@queens.oax.ac.uk"
#readRenviron(here(".Renviron"))  # Load from correct directory
#Sys.getenv("GBIF_USER")          # Check if GBIF_USER is set
#Sys.getenv("GBIF_PWD")           # Check if GBIF_PWD is set
#Sys.getenv("GBIF_EMAIL")         # Check if GBIF_EMAIL is set
#file.exists(here(".Renviron"))


#library(rgbif)

#key <- occ_download(
 # pred("scientificName", "Picea abies"),
 # format = "SIMPLE_CSV" )

#occ_download_wait(key) # Wait for download
#species.gbif_Picea <- occ_download_get(key) %>% occ_download_import()

#normalizePath("~/.Renviron")

#Assessing data for Ips typographus - spruce bark beetle

Ips_gbif_file <- here("data", "raw", "Ips_gbif.rds") # creating a data file with the folder; (here ensures the path is relative to the project root).
if (!file.exists(Ips_gbif_file)) { # this checks if the file already exists at the specified path.
  Ips_gbif <- gbif("Ips", "typographus", geo = TRUE) # extracting the species from GBIF - from the dismo 
  saveRDS(Ips_gbif, Ips_gbif_file)
} else { #if the file does exist then just read it 
  Ips_gbif <- readRDS(Ips_gbif_file) #read it 
}

# for different species use different files 

# Pull out the lat and lon columns. There is a lot more info in the `species.gbif` dataframe (>200 variables!) 
# if you want it, but we don't need it here. 
# Let's also delete rows with missing spatial coordinates.
species.coords <- cbind(species.gbif$lon, species.gbif$lat) # combining lat and long into matrix
species.coords <- na.omit(species.coords) # deleting rows with missing data (NA)
species.coords <- data.frame(species.coords) # creating this into a data frame
colnames(species.coords) <- c("lon", "lat")  # column names 

#turn this into a pipe! maybe put all as tidyverse!!
library(tidyverse)
species_2_coords <- cbind(Ips_gbif$lon, Ips_gbif$lat) %>%
  na.omit() %>%
  data.frame() %>%
  rename("lon" = X1, "lat" = X2)


#^ turn into a pipe!!! pipes can be used to avoid overwriting 
# perhaps use tidy verse?? Rethink of how to do this!! 

# Plot on world country map, setting the xlim and ylim to span the range of longitudes and latitudes
plot(wrld_simpl, xlim = range(species_1_coords$lon), ylim = range(species_1_coords$lat), axes = TRUE, col = "light yellow")
# Add points for this species
points(species_1_coords, col = "red", cex = 0.75)




#Let's see what the Species 2 Distribution is looking like to ensure that we can make informed choices of where to explore the data. 
#

#Assessing data for Ips typographus - spruce bark beetle

Ips_gbif_file <- here("data", "raw", "Ips_gbif.rds") # creating a data file with the folder; (here ensures the path is relative to the project root).
if (!file.exists(Ips_gbif_file)) { # this checks if the file already exists at the specified path.
  Ips_gbif <- gbif("Ips", "typographus", geo = TRUE) # extracting the species from GBIF - from the dismo 
  saveRDS(Ips_gbif, Ips_gbif_file)
} else { #if the file does exist then just read it 
  Ips_gbif <- readRDS(Ips_gbif_file) #read it 
}

#turn this into a pipe! maybe put all as tidyverse!!
library(tidyverse)
species_2_coords <- cbind(Ips_gbif$lon, Ips_gbif$lat) %>%
  na.omit() %>%
  data.frame() %>%
  rename("lon" = X1, "lat" = X2)


#^ turn into a pipe!!! pipes can be used to avoid overwriting 
# perhaps use tidy verse?? Rethink of how to do this!! 

# Plot on world country map, setting the xlim and ylim to span the range of longitudes and latitudes
plot(wrld_simpl, xlim = range(species_2_coords$lon), ylim = range(species_2_coords$lat), axes = TRUE, col = "light yellow")
# Add points for this species
points(species_2_coords, col = "red", cex = 0.75)



Crocus_gbif_file <- here("data", "raw", "Crocus_bif.rds") # creating a data file with the folder; (here ensures the path is relative to the project root).
if (!file.exists(Crocus_gbif_file)) { # this checks if the file already exists at the specified path.
  wasp_gbif <- gbif("Phoenix", "dactylifera", geo = TRUE) # extracting the species from GBIF - from the dismo 
  saveRDS(Crocus_gbif, Crocus_gbif_file)
} else { #if the file does exist then just read it 
  Crocus_gbif <- readRDS(Crocus_gbif_file) #read it
}

#Assessing data for Ips typographus - spruce bark beetle

Olea_gbif_file <- here("data", "raw", "Olea_bif.rds") # creating a data file with the folder; (here ensures the path is relative to the project root).
if (!file.exists(Olea_gbif_file)) { # this checks if the file already exists at the specified path.
  Olea_gbif <- gbif("Olea", "europaea", geo = TRUE) # extracting the species from GBIF - from the dismo 
  saveRDS(Olea_gbif, Olea_gbif_file)
} else { #if the file does exist then just read it 
  Olea_gbif <- readRDS(Olea_gbif_file) #read it 
}

#turn this into a pipe! maybe put all as tidyverse!!
library(tidyverse)
species_3_coords <- cbind(Olea_gbif$lon, Olea_gbif$lat) %>%
  na.omit() %>%
  data.frame() %>%
  rename("lon" = X1, "lat" = X2)


#^ turn into a pipe!!! pipes can be used to avoid overwriting 
# perhaps use tidy verse?? Rethink of how to do this!! 

# Plot on world country map, setting the xlim and ylim to span the range of longitudes and latitudes
plot(wrld_simpl, xlim = range(species_3_coords$lon), ylim = range(species_3_coords$lat), axes = TRUE, col = "light yellow")
# Add points for this species
points(species_3_coords, col = "red", cex = 0.75)









# This includes the whole world: let's trim down to the data for Africa, to make it easier to work with.
# A bit of digging online suggests latitudes +40 to -40 and longitudes -20 to +55.

# First, here is a function to pull out just the data within a lat and lon range. Note how we have documented it: 
# this is good practice for your own functions, and will help you remember what they do later on.

#' Trim coordinates based on bounding box
#' 
#' This function filters a spatial data frame to keep only points within specified
#' latitude and longitude boundaries.
#' this exmaples in  each of the parameters that feature in the function
#'  (using @param)
#'
#' @param x A data frame containing latitude and longitude columns named 'lat' and 'lon'
#' @param latmin Minimum latitude (southern boundary)
#' @param latmax Maximum latitude (northern boundary)
#' @param lonmin Minimum longitude (western boundary)
#' @param lonmax Maximum longitude (eastern boundary)
#'
#' @return (this is specifying what the function produces) A filtered data frame containing only points within the specified boundaries
#' 
#' @examples
#' df <- data.frame(lat = c(30, 35, 40), lon = c(-120, -115, -110))
#' trim.coords(df, 32, 38, -118, -112)

trim.coords <- function(x, latmin, latmax, lonmin, lonmax) {
  x[x$lon >= lonmin & x$lon <= lonmax & x$lat >= latmin & x$lat <= latmax,] # include values of x where there is bigger and lwss than lon and lat 
}

# Then use the function to make a new table of coordinates, just within the ranges specified
species_1_coords_trim <- trim.coords(species_1_coords, latmin = 35, latmax = 71, lonmin = -25, lonmax = 45)

# Plot world map again now using the trimmed data:
plot(wrld_simpl, xlim = range(species_1_coords_trim$lon), ylim = range(species_1_coords_trim$lat), axes = TRUE, col = "light yellow")
# And add points for this species
points(species_1_coords_trim, col = "red", cex = 0.75)


trim.coords <- function(x, latmin, latmax, lonmin, lonmax) {
  x[x$lon >= lonmin & x$lon <= lonmax & x$lat >= latmin & x$lat <= latmax,] # include values of x where there is bigger and lwss than lon and lat 
}

# Then use the function to make a new table of coordinates, just within the ranges specified
species_2_coords_trim <- trim.coords(species_2_coords, latmin = 35, latmax = 71, lonmin = -25, lonmax = 45)

# Plot world map again now using the trimmed data:
plot(wrld_simpl, xlim = range(species_2_coords_trim$lon), ylim = range(species_2_coords_trim$lat), axes = TRUE, col = "light yellow")
# And add points for this species
points(species_2_coords_trim, col = "blue", cex = 0.75)


# Define function to trim coordinates
trim.coords <- function(x, latmin, latmax, lonmin, lonmax) {
  x[x$lon >= lonmin & x$lon <= lonmax & x$lat >= latmin & x$lat <= latmax,]  
}

# Trim both species' coordinates
species_1_coords_trim <- trim.coords(species_1_coords, latmin = 35, latmax = 71, lonmin = -25, lonmax = 45)
species_2_coords_trim <- trim.coords(species_2_coords, latmin = 35, latmax = 71, lonmin = -25, lonmax = 45)

# Plot world map once
plot(wrld_simpl, 
     xlim = range(c(species_1_coords_trim$lon, species_2_coords_trim$lon)), 
     ylim = range(c(species_1_coords_trim$lat, species_2_coords_trim$lat)), 
     axes = TRUE, 
     col = "light yellow")

# Add points for both species
points(species_1_coords_trim, col = "red", cex = 0.75)
points(species_2_coords_trim, col = "blue", cex = 0.75)




# If it looks OK, let's use the trimmed version as our new coordinates
species_1_coords <- species.coords.trim #(BUT THIS REWRITES THE CODE!! BAD PRACTICE!)




# We only want to consider the continental land area, so let's to remove the ocean areas from the map.
# We can use the ocean shapefile from the Natural Earth dataset to do this.

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

# Convert coordinates to a spatial features (sf) object for GIS operations
species.coords <- st_as_sf(species.coords, coords = c("lon", "lat"))
# Set the coordinate reference system (CRS) to match the oceans data
st_crs(species.coords) <- st_crs(oceans)
sf_use_s2(FALSE)  # Disable spherical geometry

# Find where out points intersect with the ocean
tmp <- sapply(st_intersects(species.coords, oceans), function(z) if (length(z) == 0) NA_integer_ else z[1])

# Remove points that intersect with the ocean and convert back to table of coordinates
# Here it is an arbitrary - maybe fine tune by inspecting at specific data points that intersect with ocean
if (sum(!is.na(tmp)) > 0) {
  species.coords <- data.frame(st_coordinates(species.coords[is.na(tmp), ]))
} else {
  species.coords <- data.frame(st_coordinates(species.coords))
}
colnames(species.coords) <- c("lon", "lat")

# Now plot again to check
plot(wrld_simpl, xlim = range(species.coords$lon), ylim = range(species.coords$lat), axes = TRUE, col = "light yellow")
points(species.coords, col = "red", cex = 0.75)



# ======================================================================
# 3) Extract climatic values for the localities occupied by the species
# ======================================================================

# Download bioclimatic data from the worldclim database and convert to Raster format
bio.data <- worldclim_global(var = "bio", res = 10, path = here("data", "raw"))
names(bio.data) <- paste0("bio", 1:19)

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
# 

# Let's visualise the first two variables:
plot(bio.data, 1, main="Annual Mean Temperature")
plot(bio.data, 2, main="Mean Diurnal Range")

# You can extract bioclimatic data for the focal localities in Africa where your species is found
# filtering data for just the focal region
bio.values <- extract(bio.data, species.coords)[, -1]
rownames(bio.values) <- rownames(species.coords)

# And plot them to see the range of environmental values the species lives in
plot(bio.values[, 1], bio.values[, 2])
# just visuakising correltaion but could be good to plot a linear model!

# Or look at how different variables correlate; here focusing on the first five:
pairs(bio.values[, 1:5])
# evryhting agaisnt evrtyhing - just to identify correlations!!
# if two explanatory variables are corralted they might itnroduce confoudnign variables 

# Append to lat long, remove rows with missing data, and save to file for future use
species.data <- cbind(species.coords, bio.values)
write.csv(species.data, file = here("data", "processed", "species_data.csv"), row.names = FALSE)

# Extract mean values, min and max, as a sanity check for the data
#     jsut to check for big mistakes 
rbind(
  mean = colMeans(species.data),
  min = apply(species.data, 2, min),
  max = apply(species.data, 2, max)
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
  min(species.coords$lon) - 5,
  max(species.coords$lon) + 5,
  min(species.coords$lat) - 5,
  max(species.coords$lat) + 5
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

train <- rbind(species.coords, bg)
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

#correlation?? if you put them both in then you might not 
# reducing the power 

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


# =========================================================================
# 5) Predict future distributions under climate change scenarios
# =========================================================================


# Step 1: Download future climate projections
# We'll use CMIP5 (Coupled Model Intercomparison Project Phase 5) data, specifically for the time 
# period: 2061-2080, using one selected climate model (from many available), and with one set of parameters.
# Note that while CMIP5 contains many models and scenarios, we're using just one
# combination to demonstrate the methods

future.bio.data <- cmip6_world(
  model = "CanESM5",
  var = "bio", 
  ssp = "245",
  res = 10,
  time = "2061-2080",
  path = here("data", "raw")
)
names(future.bio.data) <- names(bio.data)

# Crop future climate data to region of interest for efficiency
future.bio.data <- crop(future.bio.data, e)

# Fit model with present data
gm1 <- glm(pa ~ bio1 + bio2 + bio3 + bio4 + bio5,
           family = binomial(link = "logit"), data = envtrain
)

# Calculate predictions for present and future
pg <- predict(bio.data, gm1, ext = e, type = "response")
pg <- crop(pg, e)
pg.future <- predict(future.bio.data, gm1, ext = e, type = "response")
pg.future <- crop(pg.future, e)

# Plot results side by side
par(mfrow = c(1, 2))

# Present distribution
plot(pg, main = "A) GLM present")
plot(wrld_simpl, add = TRUE, border = "dark grey")
points(species.coords, col = "black", pch = 4, cex = 0.5)

# Future distribution
plot(pg.future, main = "B) GLM, 2060-2081")
plot(wrld_simpl, add = TRUE, border = "dark grey")
points(species.coords, col = "black", pch = 4, cex = 0.5)

# Question 7: How is the distribution of the species expected to change in the future?

# Extract counts for range changes
# First, get predictions for present and future at all localities
predict.localities.now <- extract(pg >= tr, species.coords)[, -1]
predict.localities.future <- extract(pg.future >= tr, species.coords)[, -1]

# We can extract some numbers about how the range will change, for example:
present_range <- sum(predict.localities.now)
future_range <- sum(predict.localities.future, na.rm = TRUE)
range_expansion <- sum((predict.localities.now == 0) & (predict.localities.future == 1), na.rm = TRUE)
range_contraction <- sum((predict.localities.now == 1) & (predict.localities.future == 0), na.rm = TRUE)

# Print results in a clear format
print("Range change metrics:")
print(paste("Current suitable localities:", present_range))
print(paste("Future suitable localities:", future_range))
print(paste("Number of new suitable localities (expansion):", range_expansion))
print(paste("Number of lost suitable localities (contraction):", range_contraction))


# Question 8: Use these numbers to calculate the percentage contraction or expansion in the range.

# A final plot that can be useful to understand causes of changes is one showing how climate will change

# Work out the change in bioclim variables from now to the future
change.bio.data <- future.bio.data - bio.data

# plot present, future and change in climate for bioclim 1 variable, mean annual temperature.
par(mfrow = c(1, 3))
plot(bio.data[[1]], ext = e, main = "Present Day")
plot(future.bio.data[[1]], ext = e, main = "2061-2080")
plot(change.bio.data[[1]], ext = e, main = "Projected Change") 
