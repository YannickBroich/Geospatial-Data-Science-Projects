# Load the meso-region shapefile
mun <- read_meso_region(year = 2017)

# Load population data from the Excel file
pop_data <- read_excel("Figure_3/ipeadata.xlsx")

# Standardize text: ensure consistency for matching
mun <- mun %>%
  mutate(code_meso = as.numeric(code_meso)) # Ensure numeric for matching

pop_data <- pop_data %>%
  mutate(Codigo = as.numeric(Codigo)) # Ensure numeric for matching

# Join using code_meso and Codigo
matched_data <- mun %>%
  left_join(pop_data, by = c("code_meso" = "Codigo"))

# Replace NA values in the population data with 0
matched_data <- matched_data %>%
  mutate(
    `1950` = ifelse(is.na(`1950`), 0, as.numeric(`1950`)),
    `1980` = ifelse(is.na(`1980`), 0, as.numeric(`1980`)),
    `2010` = ifelse(is.na(`2010`), 0, as.numeric(`2010`))
  )

# Calculate total population for each year
total_pop <- matched_data %>%
  summarise(
    total_1950 = sum(`1950`),
    total_1980 = sum(`1980`),
    total_2010 = sum(`2010`)
  )

# Calculate population shares for each meso-region
matched_data <- matched_data %>%
  mutate(
    share_1950 = `1950` / total_pop$total_1950,
    share_1980 = `1980` / total_pop$total_1980,
    share_2010 = `2010` / total_pop$total_2010
  )

# Check for unmatched regions based on `code_meso` only
unmatched_regions <- mun %>%
  filter(!code_meso %in% matched_data$code_meso) %>%
  dplyr::select(code_meso, name_meso)


if (nrow(unmatched_regions) > 0) {
  print("The following meso-regions in the shapefile do not exist in the population data:")
  print(unmatched_regions)
} else {
  print("All regions matched successfully.")
}

# Prepare data for visualization
matched_data_long <- matched_data %>%
  dplyr::select(name_meso, geom, share_1950, share_1980, share_2010) %>% # Use `name_meso` instead of `name_state`
  pivot_longer(
    cols = starts_with("share"),
    names_to = "Year",
    values_to = "Pop_Share"
  ) %>%
  mutate(Year = recode(Year, `share_1950` = "1950", `share_1980` = "1980", `share_2010` = "2010"))

# Plot the faceted map
ggplot(data = matched_data_long) +
  geom_sf(aes(fill = Pop_Share), color = "black") +
  scale_fill_viridis_c(name = "Population Share", option = "C", trans = "sqrt") +
  facet_wrap(~Year, ncol = 3) +
  theme_minimal() +
  labs(
    title = "Population Share in Brazil (1950, 1980, 2010)",
    fill = "Population Share"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(2, "cm")
  )