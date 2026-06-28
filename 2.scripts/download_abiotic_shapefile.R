#
# Download abiotic data and world shapefile
#
# Rafael F. Barduzzi
#
# 04/2026
#

# Setup =======================================================================

## Libraries ------------------------------------------------------------------

### data manipulation
library(tidyverse)
### abiotic data
library(geodata)
library(terra)
### shapefile
library(rnaturalearth)
library(sf)

# to install geodata, run:
# install.packages("remotes")
# remotes::install_github("rspatial/geodata")

## Parameters -----------------------------------------------------------------

### download abiotic data?
DOWNLOAD <- F

### calculate average for 5-, 15-, and 30-cm sampling depths of edaphic data?
AVERAGE <- F

### path do store abiotic data and world shapefile
data_path <- file.path(getwd(), "data")

### variables to download using soildgrids
soil_vars <- c("bdod", "cfvo", "clay", "nitrogen", "ocd", "phh2o", "sand", 
               "silt", "soc")

### depths to download using soilgrids
depths <- c(5, 15, 30)

# Download data ===============================================================

if (DOWNLOAD == TRUE) {
  
  options(geodata_default_path = data_path)
  
  # download global elevation data for 30s resolution 
  # from WordClim 2.1
  elevation_global(res = 0.5)
  
  # download global climatic data for 30s resolution
  # from WordClim 2.1
  worldclim_global(res = 0.5, var = "bio")
  
  # download global edaphic data for 5-, 15-, and 30-cm sampling depths 
  # from SoilGrids 2.1
  lapply(depths, function(d) {
    soil_world(
      var = soil_vars,
      depth = d,
      stat = "mean",
      name = "",
      path = data_path
    )
  })
  
  stop("Abiotic data was downloaded or is already stored")
  
}

## Average edaphic data across dephts -----------------------------------------

if (AVERAGE == TRUE) {
  
  soilgrids_edaphic <- lapply(list.files(
    path = "C:/abiotic_data/soil_world", 
    pattern = "\\.tif$", 
    full.names = TRUE,
    recursive = TRUE), 
    rast)
  
  ### compute average raster for each variable across all depths
  avg_rasters <- list()
  
  for (var in soil_vars) {
    # select rasters for this variable (will match variable in layer name)
    rasters <- soilgrids_edaphic[sapply(soilgrids_edaphic, function(x) startsWith(names(x), var))]
    # stack them
    stack <- rast(rasters)
    # calculate mean across depths
    avg <- mean(stack, na.rm = TRUE)
    names(avg) <- var  # assign the original variable name
    avg_rasters[[var]] <- avg
    # plot
    plot(avg_rasters[[var]], main = paste("Average of", var))
  }
  
  for (var in names(avg_rasters)) {
    file_path <- file.path("C:/abiotic_data/soil_data_avg", 
                           paste0(var, "_5-30cm_avg_30s.tif"))
    writeRaster(avg_rasters[[var]], file_path, overwrite = TRUE)
    
  }
  
}

# Download World Shapefile ====================================================

# Get world countries as an sf object
world <- ne_countries(scale = "small", returnclass = "sf")

# Write as an ESRI Shapefile
out_dir <- file.path(data_path, "ne_countries_small")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

sf::st_write(world,
             dsn = out_dir,
             layer = "ne_countries_small",
             driver = "ESRI Shapefile",
             delete_layer = TRUE)


