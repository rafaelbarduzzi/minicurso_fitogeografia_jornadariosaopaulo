rm(list = ls())

###

library(terra)
library(sf)
library(tidyverse)
library(data.table)
library(betapart)
library(phangorn)
library(dendextend) 
library(viridis)
library(paletteer)
library(ggdendro)
library(gridExtra)

###

world <- vect("data/World_Countries/World_Countries.shp")

###

world

###

SAm <- world %>% 
  subset(subset = .$CONTINENT == "South America") %>%
  project("EPSG:3395")

### 

plot(SAm)

###

# Convert 'SpatVector' to 'sf', a spatial object class that is more flexible 
# and convenient to work with in R
## Occasionally, spatial geometries can become invalid due to self-intersections 
## or overlapping lines in polygon boundaries. Here, we also fix this.
SAm_sf <- st_as_sf(SAm) %>%
  st_make_valid(.)

# Create hexagonal grid cells spanning the spatial extent of South America and 
# add an ID for each grid cell
hex_grid_sf <- st_make_grid(SAm_sf, cellsize = 100000, square = F) %>%
  st_sf(.) %>%
  mutate(grid_id = 1:nrow(.))

# Identify grid cells that actually overlap with South America, creating a 
# binary vector (0 = no intersection; 1 = intersects)
is_intersecting <- hex_grid_sf %>%
  st_intersects(., SAm_sf) %>%
  sapply(., function(x) if(length(x) == 0) 0 else 1)

# Keep only grid cells that intersect with South America
hex_grid_SAm <- hex_grid_sf[which(is_intersecting == 1), ] 

###

plot(st_geometry(hex_grid_SAm))

###

occurrence <- read.csv("data/occurrence_clean.csv")

###

str(occurrence)

###

# Convert occurrence data frame to a 'sf' object using the WGS84 CRS, which is 
# the standard for GPS devices, and then project the object using the grid 
# cells' CRS 
occurrence_points <- st_as_sf(x = occurrence, 
                              coords = c("longitude", "latitude"),
                              crs = "EPSG:4326") %>% 
  st_transform(., crs(hex_grid_SAm))

###

# Intersect occurrences with grid cells to identify which records are within 
# each grid cell
inter_occurrence <- st_intersects(occurrence_points, hex_grid_SAm)

# Assign each occurrence record to a grid cell based on the intersection result
occurrence_points$grid_id <- NA
for(j in 1:nrow(occurrence_points)){
  if(length(inter_occurrence[[j]]) == 1){
    occurrence_points$grid_id[j] <- hex_grid_SAm$grid_id[inter_occurrence[[j]]]
  }
}

# Filter the occurrence data to keep only records that fall within the study area
occurrence$grid_id <- occurrence_points$grid_id
occurrence <- occurrence %>%
  filter(!is.na(grid_id))

### 

# Create a community matrix with species presence/absence in each grid cell
comm_matrix <- dcast(as.data.table(occurrence), 
                     grid_id ~ full_name_clean_sp_lvl,
                     fun.aggregate = length)
comm_matrix <- as.data.frame(comm_matrix)
rownames(comm_matrix) <- comm_matrix$grid_id
comm_matrix <- comm_matrix[ , -1]           # Remove the grid_id column
comm_matrix[comm_matrix > 1] <- 1           # Convert values >1 to 1 for 
# presence/absence data

###

# Sum number of species per grid cell
sp_richness <- rowSums(comm_matrix)

# Store species richness values in each grid cell
hex_grid_SAm <- hex_grid_SAm %>% 
  mutate(sp_richness = sp_richness[as.character(grid_id)])

# Plot species richness
sr <- ggplot() +
  geom_sf(data = SAm_sf,
          aes(), color = "black", fill = "transparent",
          lwd = 0.3) +
  geom_sf(data = hex_grid_SAm, 
          aes(fill = sp_richness),
          color = "transparent",
          lwd = 0) +
  scale_fill_continuous(type = "viridis",
                        na.value = NA,
                        alpha = 0.9,
                        name = "Species richness") +
  theme_bw() +
  theme(legend.position = "right")
sr

###

# Remove (1) grid cells with a number of taxa below the provided threshold, and 
# (2) unique taxa (i.e. occurs in less than two cells)
threshold <- 2

### Loop over steps 1 and 2
repeat {
  # Identify cells and taxa to remove
  cells_to_rm <- which(rowSums(comm_matrix) < threshold)
  taxa_to_rm <- which(colSums(comm_matrix) < threshold)
  
  # Break loop if no more rows or columns meet the removal criteria
  if (length(cells_to_rm) == 0 && length(taxa_to_rm) == 0) {
    break
  }
  
  # Remove identified cells and taxa
  if (length(cells_to_rm) > 0) {
    comm_matrix <- comm_matrix[-cells_to_rm, , drop = FALSE]
  }
  if (length(taxa_to_rm) > 0) {
    comm_matrix <- comm_matrix[, -taxa_to_rm, drop = FALSE]
  }
}

beta_multi <- beta.multi(x = comm_matrix,
                         index.family = "sorensen")
beta_multi

###

beta_pair <- beta.pair(x = comm_matrix, 
                       index.family = "sorensen")

###

# Run a clustering analysis using the Ward algorithm for the turnover component
set.seed(1)
simp_cluster <- upgma(beta_pair$beta.sim, method = "ward.D")
simp_hc <- as.hclust(simp_cluster)

# Make a dendrogram
simp_dend <- as.dendrogram(simp_hc) 

# Split the dendrogram into clusters and assign a different color to each one of 
# them
## Here, we arbitrarily split the dendrogram into 5 clusters. Ultimately, the 
## number of clusters will come to the research question, but it is usually better
## to report results with a varying number of clusters. 
ncluster <- 5
colors <- as.character(paletteer_d("ggthemes::colorblind"))[1:ncluster]
simp_dend <- color_branches(simp_dend, k = ncluster,
                            col = colors) %>% 
  set("branches_lwd", c(0.25)) 

# Make dendrogram plot
dend <- as.ggdend(simp_dend)
dend <- ggplot(dend, labels = FALSE) +
  scale_x_reverse(expand = c(0.2, 0)) + # Reverse the x-axis to make tips face 
  # left
  coord_flip()                          # Rotate plot to horizontal orientation

# Get the cluster ID and color for each grid cell
grid_id_cluster <- data.frame("grid_id" = labels(simp_dend), 
                              "colour" = get_leaves_branches_col(
                                simp_dend
                              ), 
                              "cluster_membership" = NA)
colors_2 <- colors
names(colors_2) <- as.character(1:length(colors_2))
for(i in 1:nrow(grid_id_cluster)){
  grid_id_cluster$cluster_membership[i] <- 
    names(colors_2)[unname(colors_2) == grid_id_cluster$colour[i]]
}

# Encode cluster IDs as factors
grid_id_cluster$cluster_membership <- 
  factor(grid_id_cluster$cluster_membership, 
         levels = unique(grid_id_cluster$cluster_membership))

# Merge spatial data and cluster IDs and colours
simp_sf <- merge(hex_grid_SAm, grid_id_cluster, by.x = "grid_id",
                 all.x = TRUE, all.y = TRUE)

# Make map plot
map <- ggplot() +
  geom_sf(data = SAm_sf,
          aes(), color = "black", fill = "transparent",
          lwd = 0.3)  +
  geom_sf(data = simp_sf,
          aes(fill = cluster_membership),
          color = "transparent",
          lwd = 0) +
  scale_fill_manual(values = adjustcolor(colors, alpha = 0.9),
                    na.value = NA)+
  theme_bw() +
  theme(legend.position = "none") 

# Plot map and dendrogram side by side
grid.arrange(map, dend, ncol = 2)

###

# Elevation
elevation <- rast("data/elev_merc/elev.tif")

# BioClim variables
library(naturalsort)
bioclim_list <- 
  naturalsort(list.files("data/bioclim_merc",
                         pattern =".tif$", full.names = TRUE))
bioclim_rasters <- rast(bioclim_list)

# Save 
# writeRaster(elevation, "data/elev_merc/elev.tif")
# for(i in 1:length(names(bioclim_rasters))){
#   writeRaster(bioclim_rasters[[i]], paste0("data/bioclim_merc/bio_",
#                                            i,
#                                            ".tif"))
# }

###

# Get centroid coordinates of each grid cell
grid_coords <- crds(centroids(vect(hex_grid_SAm)))

# Prepare site-by-predictor matrix
site_predictor <- data.frame("grid_id" = hex_grid_SAm$grid_id, 
                             "lon" = grid_coords[ , 1],
                             "lat" = grid_coords[ , 2])
site_predictor <- cbind(site_predictor,
                        z_stats_elev,
                        z_stats_precip)

# Assign abiotic values for each grid cell
grid_cells <- cbind(hex_grid_SAm, site_predictor)



# # Plot elevation
# ggplot() +
#   geom_sf(data = SAm_sf,
#           color = "black",
#           lwd = 0.3) +
#   geom_sf(data = grid_cells,
#           aes(fill = wc2.1_30s_elev),
#           color = "transparent",
#           lwd = 0) +
#   scale_fill_gradient(low = adjustcolor(viridis(1), alpha.f = 0.7), 
#                       high = adjustcolor(viridis(16), alpha.f = 0.7)) +
#   ggtitle("Elevation") +
#   labs(fill = NULL,
#        colour = NULL) +
#   theme_bw()
# 
# # Plot precipitation
# ggplot() +
#   geom_sf(data = SAm_sf,
#           color = "black",
#           lwd = 0.3) +
#   geom_sf(data = grid_cells,
#           aes(fill = wc2.1_30s_bio_12),
#           color = "transparent",
#           lwd = 0) +
#   scale_fill_gradient(low = adjustcolor(viridis(1), alpha.f = 0.7), 
#                       high = adjustcolor(viridis(16), alpha.f = 0.7)) +
#   ggtitle("Annual Mean Precipitation") +
#   labs(fill = NULL,
#        colour = NULL) +
#   theme_bw()
