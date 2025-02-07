# Set the path to the Vietnam shapefile
shapefile_path <- "Figure_4/vn.shp"

# Read the shapefile
shapefile_data <- st_read(shapefile_path)

# Set the path to the road infrastructure shapefile
road_path <- "Figure_4/vnm_rdsl_2015_OSM.shp"

# Read the road shapefile
road_data <- st_read(road_path)

str(road_data)

# View unique road types
unique_road_types <- unique(road_data$type)
print(unique_road_types)

# Define the classification for road types
type_summary <- c(
  "footway" = "Other roads", "residential" = "Minor roads", 
  "primary" = "Major roads", "secondary" = "Major roads",
  "pedestrian" = "Other roads", "tertiary" = "Minor roads", 
  "trunk" = "Dual carriageway", "unclassified" = "Other roads",
  "service" = "Other roads", "motorway_link" = "Freeway", 
  "motorway" = "Freeway", "primary_link" = "Major roads",
  "track" = "Other roads", "tertiary_link" = "Minor roads", 
  "road" = "Other roads", "trunk_link" = "Dual carriageway",
  "secondary_link" = "Major roads", "living_street" = "Other roads",
  "steps" = "Other roads", "path" = "Other roads", 
  "construction" = "Other roads", "cycleway" = "Other roads", 
  "proposed" = "Other roads", "crossing" = "Other roads",
  "services" = "Other roads", "rest_area" = "Other roads", 
  "yes" = "Other roads"
)

# Apply classification to the road data
road_data$road_type <- type_summary[road_data$type]

# Remove NA values (in case of missing or undefined road types)
road_data <- road_data[!is.na(road_data$road_type), ]

ggplot() +
  # Plot the Vietnam boundary
  geom_sf(data = shapefile_data, fill = "gray90", color = "black") +
  # Plot the road data with color and size mapped by road type
  geom_sf(data = road_data, aes(color = road_type, size = road_type)) +
  # Define color scheme for road types
  scale_color_manual(
    values = c(
      "Freeway" = "blue",
      "Dual carriageway" = "darkgrey",
      "Major roads" = "red",
      "Minor roads" = "orange",
      "Other roads" = "yellow"
    ),
    name = "Road types"
  ) +
  # Define size scheme for road types
  scale_size_manual(
    values = c(
      "Freeway" = 2, # Thicker lines for Freeway
      "Dual carriageway" = 2, # Thicker lines for Dual carriageway
      "Major roads" = 1.5,
      "Minor roads" = 1.5,
      "Other roads" = 0.2
    ),
    guide = "none" # Remove size legend (optional)
  ) +
  theme_minimal() +
  ggtitle("Road Map of Vietnam (2015)") +
  theme(
    legend.position = "right", # Position the legend on the right
    legend.direction = "vertical", # Arrange the legend vertically
    legend.title = element_text(size = 14, face = "bold"), # Bold legend title
    legend.text = element_text(size = 12), # Larger legend text
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Centered bold title
    legend.key = element_rect(fill = "white", color = NA), # White background for legend keys
    legend.key.size = unit(1.5, "lines") # Increase legend key size
  ) +
  guides(color = guide_legend(
    title.position = "top", # Place legend title at the top
    title.hjust = 0.5 # Center align the legend title
  ))