rasterOptions(maxmemory = 1e+08)  # Increase memory allocation
# Unzip gradient data (GeoTIFF) 
gradient_zip <- "Figure_1/SA_NLC_2022_GEO.tif.zip"  # Gradient zip file
unzip_dir_gradient <- tempdir()
unzip(gradient_zip, exdir = unzip_dir_gradient)

# Locate the .tif file
tif_files <- list.files(unzip_dir_gradient, pattern = "\\.tif$", full.names = TRUE)
gradient <- rast(tif_files[1])  # Load the first .tif file

# Load river shapefile
rivers <- st_read("Figure_1/wriall500.shp")

st_crs(rivers) <- 4326  # Assign WGS 84 if CRS is missing

# Load district boundaries
districts <- st_read("Figure_1/zaf_admbnda_adm2_sadb_ocha_20201109.shp")

# Check and assign CRS for districts
if (is.na(st_crs(districts))) {
  st_crs(districts) <- 4326  # Assign WGS 84 if CRS is missing
}

# Transform CRS of rivers and districts to match the gradient raster
rivers <- st_transform(rivers, st_crs(gradient))
districts <- st_transform(districts, st_crs(gradient))

# Rasterize river network
rivers_raster <- rasterize(vect(rivers), gradient, field = 1)  # Field = 1 for presence

# Mask gradient data to include only river pixels
river_gradient <- mask(gradient, rivers_raster)

# Calculate the average river gradient per district
district_avg_gradient <- exact_extract(river_gradient, districts, fun = "mean")

# Add the average gradient to district data
districts$avg_gradient <- district_avg_gradient

# Unzip and read dam data from KML
unzip_dir_dams <- tempdir()
unzip("Figure_1/Registered_Dams_Oct2024.kmz", exdir = unzip_dir_dams)

# Locate the KML file
kml_files <- list.files(unzip_dir_dams, pattern = "\\.kml$", full.names = TRUE)
kml_path <- kml_files[1]

# Read the KML file
spatial_data <- st_read(kml_path)

# Filter dam data for irrigation purpose
dams_data <- read_excel("Figure_1/List_of_Registered_Dams_Oct2024.xlsx", sheet = "Sheet1")
filtered_dams <- dams_data %>%
  filter(grepl("Irrigation", Purpose, ignore.case = TRUE)) %>%
  mutate(`Registration date` = as.Date(`Registration date`, format = "%Y-%m-%d")) %>%
  filter(`Registration date` < as.Date("2002-12-31"))

filtered_dams$`Name of dam` <- as.character(filtered_dams$`Name of dam`)
spatial_data$Name <- as.character(spatial_data$Name)
matched_dams <- spatial_data %>%
  filter(Name %in% filtered_dams$`Name of dam`)

# Ensure CRS alignment for dams
if (st_crs(matched_dams) != st_crs(districts)) {
  matched_dams <- st_transform(matched_dams, st_crs(districts))
}

# Plot the map
ggplot() +
  # Districts with average gradient shaded
  geom_sf(data = districts, aes(fill = avg_gradient), color = "black", size = 0.2) +
  # Dam locations as black dots
  geom_sf(data = matched_dams, color = "black", size = 1, alpha = 0.7) +
  # Custom color scale
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  theme_minimal() +
  labs(
    title = "Average District River Gradient and Dam Locations",
    subtitle = "Irrigation Dams (1980-2002) and Average River Gradient",
    fill = "Avg. River Gradient",
    caption = "Data Sources: DWS and South Africa Shapefiles"
  )