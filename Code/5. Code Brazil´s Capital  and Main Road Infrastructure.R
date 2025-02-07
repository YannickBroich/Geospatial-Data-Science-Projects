# Read Brazil shapefile
brazil <- read_country(year = 2020)

# Define state capitals with coordinates
capitals <- data.frame(
  city = c("Rio Branco", "Maceió", "Macapá", "Manaus", "Salvador",
           "Fortaleza", "Brasília", "Vitória", "Goiânia", "São Luís",
           "Cuiabá", "Campo Grande", "Belo Horizonte", "Belém",
           "João Pessoa", "Curitiba", "Recife", "Teresina",
           "Rio de Janeiro", "Natal", "Porto Alegre", "Porto Velho",
           "Boa Vista", "Florianópolis", "São Paulo", "Aracaju", "Palmas"),
  lon = c(-67.8243, -35.7089, -51.0664, -60.0217, -38.5014,
          -38.5433, -47.9297, -40.3128, -49.2646, -44.3028,
          -56.0974, -54.6464, -43.9378, -48.4898, -34.845,
          -49.2731, -34.877, -42.8019, -43.1729, -35.2094,
          -51.2287, -63.9004, -60.6733, -48.548, -46.6333, -37.0731, -48.3336),
  lat = c(-9.97499, -9.64985, 0.03889, -3.11903, -12.9714,
          -3.71722, -15.7797, -20.3155, -16.6864, -2.52972,
          -15.601, -20.4428, -19.9208, -1.45502, -7.11949,
          -25.4278, -8.04756, -5.08917, -22.9068, -5.795,
          -30.0277, -8.76116, 2.81972, -27.5954, -23.5505, -10.9472, -10.1844)
)

# Convert capitals to an sf object
capitals_sf <- st_as_sf(capitals, coords = c("lon", "lat"), crs = 4326)

# Define city connections
city_connections <- data.frame(
  city1 = c("Boa Vista", "Manaus", "Porto Velho", "Rio Branco", "Cuiabá", "Campo Grande", "Goiânia",
            "Porto Alegre", "Florianópolis", "Curitiba", "São Paulo", "Rio de Janeiro", "Vitória",
            "Belo Horizonte", "Macapá", "Belém", "São Luís", "Palmas", "João Pessoa", "Natal",
            "Fortaleza", "Teresina", "João Pessoa", "Recife", "Maceió", "Aracaju", "Salvador"),
  city2 = c("Manaus", "Porto Velho", "Brasília", "Cuiabá", "Brasília", "Goiânia", "Brasília",
            "Florianópolis", "Curitiba", "São Paulo", "Brasília", "Belo Horizonte", "Belo Horizonte",
            "Brasília", "Belém", "São Luís", "Palmas", "Brasília", "Natal", "Fortaleza",
            "Teresina", "Brasília", "Recife", "Maceió", "Aracaju", "Salvador", "Brasília")
)

# Generate line geometries for the city connections
lines <- lapply(1:nrow(city_connections), function(i) {
  city1_coords <- st_coordinates(capitals_sf[capitals_sf$city == city_connections$city1[i], ])
  city2_coords <- st_coordinates(capitals_sf[capitals_sf$city == city_connections$city2[i], ])
  st_linestring(rbind(city1_coords, city2_coords))
})

# Create an sf object for the city connection lines
city_lines_sf <- st_sfc(lines, crs = st_crs(capitals_sf))

# Load highway data (assumes highway data is already loaded and prepared as `highways`)
highways <- st_transform(read_sf("Figure_5/SNV_202001A.shp"), crs = 4326)

# Identify radial and non-radial highways
highways$is_radial <- substr(highways$vl_br, 1, 1) == "0"

# Adjusted ggplot code to match the desired output
ggplot() +
  # Brazil shapefile
  geom_sf(data = brazil, fill = "white", color = "black") +
  # Radial highways
  geom_sf(data = highways %>% filter(is_radial), aes(linetype = "Radial Highway"), color = "black", size = 0.7) +
  # Non-radial highways
  geom_sf(data = highways %>% filter(!is_radial), aes(linetype = "Non-Radial Highway"), color = "grey", size = 0.7) +
  # Minimum Spanning Tree (City connection lines)
  geom_sf(data = city_lines_sf, aes(linetype = "Minimum Spanning Tree"), color = "red", size = 1) +
  # State capitals
  geom_sf(data = capitals_sf, color = "black", size = 2) +
  # Labels for cities
  geom_text(data = capitals, aes(x = lon, y = lat, label = city), hjust = 0.5, vjust = -1, size = 3) +
  # Coordinate limits
  coord_sf(xlim = c(-74, -34), ylim = c(-34, 6), expand = FALSE) +
  # Theme
  theme_minimal() +
  # Title and legend
  labs(
    title = "Map of Straight-Line Instrument and Radial Highways",
    linetype = "Type of Line"
  ) +
  # Custom legend for line types
  scale_linetype_manual(
    values = c(
      "Radial Highway" = "solid",
      "Non-Radial Highway" = "dotted",
      "Minimum Spanning Tree" = "dashed"
    ),
    guide = guide_legend(
      override.aes = list(color = c("red", "grey", "black"))
    )
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )