# Step 1: Load the Files
folder_path <- "Figure_2/"  # Adjust as needed

# Load administrative boundaries shapefile
admin_boundaries <- st_read(paste0(folder_path, "eth_admbnda_adm3_csa_bofedb_2021.shp"))

# Load population dataset
eth_admpop <- read.csv(paste0(folder_path, "eth_admpop_adm3_2022_v2.csv"))  # Replace with the correct filename

# Load Ethiopia electricity transmission network shapefile
transmission_network <- st_read(paste0(folder_path, "Ethiopia Electricity Transmission Network.shp"))

# Load Ethiopia roads shapefile
roads <- st_read(paste0(folder_path, "Ethiopia_Roads.shp"))

# Load Sudan grid shapefile
sudan_grid <- st_read(paste0(folder_path, "sud20112_grid.shp"))

# Load Sudan power plants shapefile
sudan_powerplants <- st_read(paste0(folder_path, "SDN_PowerPlants.shp"))

# Load Ethiopia power plants shapefile
ethiopia_powerplants <- st_read(paste0(folder_path, "ETH_PowerPlants.shp"))

# Load South Sudan roads shapefile
south_sudan_roads <- st_read(paste0(folder_path, "Final_Southern_Sudan_Roads_Version01.shp"))

# Load Eritrea power plants shapefile
eritrea_powerplants <- st_read(paste0(folder_path, "ERI_PowerPlants.shp"))

# Load Eritrea roads shapefile
eritrea_roads <- st_read(paste0(folder_path, "Final_Eritrea_Roads_Version01.shp"))

# Load North Sudan roads shapefile
north_sudan_roads <- st_read(paste0(folder_path, "North Sudan_Roads.shp"))

# Load Kenya roads shapefile
kenya_roads <- st_read(paste0(folder_path, "Kenya_roads_version2.shp"))

# Step 2: Assign CRS to Missing Layers if Necessary

# Assign CRS to all layers explicitly if missing
if (is.na(st_crs(eritrea_powerplants))) {
  st_crs(eritrea_powerplants) <- 4326  # Assign WGS 84
}

if (is.na(st_crs(ethiopia_powerplants))) {
  st_crs(ethiopia_powerplants) <- 4326  # Assign WGS 84
}

if (is.na(st_crs(sudan_powerplants))) {
  st_crs(sudan_powerplants) <- 4326  # Assign WGS 84
}

if (is.na(st_crs(kenya_roads))) {
  st_crs(kenya_roads) <- 4326  # Assign WGS 84
}

# Ensure all layers have a valid CRS before transformation
admin_boundaries <- st_transform(admin_boundaries, crs = 4326)
transmission_network <- st_transform(transmission_network, crs = 4326)
roads <- st_transform(roads, crs = 4326)
sudan_grid <- st_transform(sudan_grid, crs = 4326)
south_sudan_roads <- st_transform(south_sudan_roads, crs = 4326)
eritrea_powerplants <- st_transform(eritrea_powerplants, crs = 4326)
eritrea_roads <- st_transform(eritrea_roads, crs = 4326)
north_sudan_roads <- st_transform(north_sudan_roads, crs = 4326)
kenya_roads <- st_transform(kenya_roads, crs = 4326)

# Define a single bounding box for the map
bounding_box <- st_bbox(c(xmin = 32, ymin = 1, xmax = 47, ymax = 16), crs = 4326)

# Crop all layers to the single bounding box
admin_boundaries <- st_crop(admin_boundaries, bounding_box)
transmission_network <- st_crop(transmission_network, bounding_box)
roads <- st_crop(roads, bounding_box)
sudan_grid <- st_crop(sudan_grid, bounding_box)
south_sudan_roads <- st_crop(south_sudan_roads, bounding_box)
eritrea_roads <- st_crop(eritrea_roads, bounding_box)
north_sudan_roads <- st_crop(north_sudan_roads, bounding_box)
kenya_roads <- st_crop(kenya_roads, bounding_box)
ethiopia_powerplants <- st_crop(ethiopia_powerplants, bounding_box)
sudan_powerplants <- st_crop(sudan_powerplants, bounding_box)
eritrea_powerplants <- st_crop(eritrea_powerplants, bounding_box)

admin_boundaries <- admin_boundaries %>%
  left_join(eth_admpop, by = c("ADM3_PCODE" = "admin3Pcode"))

# Plot the map
ggplot() +
  annotation_map_tile(type = "osm", zoom = 7) +
  
  # Plot administrative boundaries with population
  geom_sf(data = admin_boundaries, aes(fill = T_TL), color = "black", size = 0.3, alpha = 0.7) +
  
  # Plot transmission networks (Ethiopia and Sudan)
  geom_sf(data = transmission_network, aes(color = "Transmission Lines"), size = 1.2, alpha = 0.9) +
  geom_sf(data = sudan_grid, aes(color = "Transmission Lines"), size = 1.2, alpha = 0.9) +
  
  # Plot roads (Ethiopia, South Sudan, Eritrea, North Sudan, Kenya)
  geom_sf(data = roads, aes(color = "Roads"), size = 0.6) +
  geom_sf(data = south_sudan_roads, aes(color = "Roads"), size = 0.6) +
  geom_sf(data = eritrea_roads, aes(color = "Roads"), size = 0.6) +
  geom_sf(data = north_sudan_roads, aes(color = "Roads"), size = 0.6) +
  geom_sf(data = kenya_roads, aes(color = "Roads"), size = 0.6) +
  
  # Plot power plants (Ethiopia, Sudan, Eritrea)
  geom_sf(data = ethiopia_powerplants, aes(color = "Power Plants"), shape = 24, size = 3, alpha = 0.9, fill = "green") +
  geom_sf(data = sudan_powerplants, aes(color = "Power Plants"), shape = 24, size = 3, alpha = 0.9, fill = "green") +
  geom_sf(data = eritrea_powerplants, aes(color = "Power Plants"), shape = 24, size = 3, alpha = 0.9, fill = "green") +
  
  # Adjust population color scale
  scale_fill_gradientn(
    colors = c("lightblue", "blue", "darkblue", "navy"),
    na.value = "grey90",
    name = "Population",
    breaks = pretty(range(admin_boundaries$T_TL, na.rm = TRUE), n = 5),
    labels = scales::comma
  ) +
  
  # Add manual legends for the layers
  scale_color_manual(
    name = "Infrastructure",
    values = c(
      "Power Plants" = "green",       # Green triangle for power plants
      "Roads" = "black",              # Black for roads
      "Transmission Lines" = "red"    # Red for transmission lines
    ),
    limits = c("Power Plants", "Roads", "Transmission Lines"),  # Correct the legend order
    guide = guide_legend(
      override.aes = list(
        linetype = c("blank", "solid", "solid"),  # Blank for power plants, solid for others
        shape = c(24, NA, NA),                   # Triangle for power plants
        size = c(4, 1.0, 1.5),                   # Adjust size for visuals
        fill = c("green", NA, NA)                # Fill color for triangle
      )
    )
  ) +
  
  theme_minimal() +
  labs(
    title = "Ethiopia and surrounding infastructure Infrastructure",
    subtitle = "Population, Transmission Network, Roads, and Power Plants",
    caption = "Data Sources: World Bank Data"
  )