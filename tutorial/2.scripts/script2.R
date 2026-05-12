library(terra)
library(sf)
world <- vect("data/World_Countries/World_Countries.shp")

str(world)
world
head(as.data.frame(world))

library(tidyverse)
SAm <- world %>% 
  subset(subset = .$CONTINENT == "South America")

SAm

plot(SAm)

# Convert 'SpatVector' to 'sf'
SAm_sf <- st_as_sf(SAm) %>%
  st_make_valid(.)

# Make hexagonal grid cells spanning the extension of South America and add an 
# identification number for each grid cell
hex_grid_sf <- st_make_grid(SAm_sf, cellsize = 100000, square = F) %>%
  st_sf(.) %>%
  mutate(id = 1:nrow(.))
plot(st_geometry(hex_grid_sf))

# Intersect grid cells with South America geometries and get a binary vector
# (0 = does not intersect; 1 = intersects)
is_intersecting <- hex_grid_sf %>%
  st_intersects(., SAm_sf) %>%
  sapply(., function(x) if(length(x) == 0) 0 else 1)

# Subset grid cells, keeping only the ones that overlap with the South American 
# geometries
hex_grid_SAm <- hex_grid_sf[which(is_intersecting == 1), ] 
plot(st_geometry(hex_grid_SAm))




# Load occurrence data
occurrence <- read.csv("data/occurrence_clean.csv")

str(occurrence)

# Convert data frame into sf points
occurrence_points <- st_as_sf(x = occurrence, 
                              coords = c("longitude", "latitude"),
                              crs = crs(hex_grid_SAm))

# Make community matrix
library(data.table)
comm_matrix <- dcast(as.data.table(occurrence), 
                     grid_id ~ full_name_clean_sp_lvl,
                     fun.aggregate = length)
comm_matrix <- as.data.frame(comm_matrix)
rownames(comm_matrix) <- comm_matrix$grid_id
comm_matrix <- comm_matrix[ , -1]
comm_matrix[comm_matrix > 1] <- 1

