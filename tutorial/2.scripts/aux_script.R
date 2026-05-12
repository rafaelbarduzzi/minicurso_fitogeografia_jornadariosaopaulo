library(terra)
library(sf)

# Elevation
elevation <- rast("data/elevation_mercator/elevation.tif")

# BioClim variables
library(naturalsort)
bioclim_list <- 
  naturalsort(list.files("data/bioclim_mercator",
                         pattern =".tif$", full.names = TRUE))
bioclim_rasters <- rast(bioclim_list)

# Read soil
soil_list <- list.files("data/soil_world/depth_mean",
                        pattern = ".tif$", full.names = TRUE)
soil_rasters <- rast(soil_list)

crs(soil_rasters) == crs(bioclim_rasters)
crs(bioclim_rasters) == crs(elevation)
crs("EPSG:3395") == crs(elevation)

soil_rasters <- project(soil_rasters, "EPSG:3395")

# Save 
for(i in 1:length(names(soil_rasters))){
  writeRaster(soil_rasters[[i]], paste0("data/soil_world_mercator/",
                                        names(soil_rasters)[i],
                                           ".tif"))
}
