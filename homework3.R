library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(sf)

vehicles <- ggplot2::mpg
gems <- ggplot2::diamonds


# Problem 1A

p_A <- ggplot(vehicles, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~drv, nrow = 1) +
  labs(
    x = "Engine displacement (litres)",
    y = "Highway fuel economy (mpg)",
    color = "drv"
  ) +
  theme_bw()

print(p_A)

ggsave("HW3_outputs/Problem1A.png", p_A,
       width = 10, height = 4, dpi = 300)

# There are three fitted lines because facet_wrap(~drv)
# divides the dataset into three groups based on drive type.
# Each panel contains only its own drive type, so
# geom_smooth() fits a separate regression line in each panel.



# Problem 1B: Count plot

p_B_count <- ggplot(vehicles,
                    aes(x = drv, fill = factor(cyl))) +
  geom_bar() +
  labs(
    x = "drv",
    y = "count",
    fill = "factor(cyl)"
  ) +
  theme_bw()

print(p_B_count)

ggsave("HW3_outputs/Problem1B_Count.png",
       p_B_count, width = 6, height = 5, dpi = 300)

# Problem 1B: Proportion plot

p_B_prop <- ggplot(vehicles,
                   aes(x = drv, fill = factor(cyl))) +
  geom_bar(position = "fill") +
  labs(
    x = "drv",
    y = "Proportion within drive type",
    fill = "factor(cyl)"
  ) +
  theme_bw()

print(p_B_prop)

ggsave("HW3_outputs/Problem1B_Proportion.png",
       p_B_prop, width = 6, height = 5, dpi = 300)

# Equal total bar heights in the proportion plot do not
# mean equal sample sizes because position = "fill"
# standardizes each bar to a height of 1 (100%).
# The count plot shows the actual number of vehicles
# in each drive type.


# Problem 1C

gems_small <- gems %>%
  filter(carat <= 2)

# Records retained and excluded
cat("Retained:", nrow(gems_small), "\n")
cat("Excluded:", nrow(gems) - nrow(gems_small), "\n")

# Fine bins
p_fine <- ggplot(gems_small, aes(x = carat)) +
  geom_histogram(
    binwidth = 0.01,
    boundary = 0,
    closed = "right",
    fill = "steelblue",
    color = "white"
  ) +
  coord_cartesian(xlim = c(0, 2)) +
  labs(
    x = "Carat",
    y = "Count",
    title = "Diamond Carat Histogram (Bin Width = 0.01)"
  ) +
  theme_bw()

# Coarse bins
p_coarse <- ggplot(gems_small, aes(x = carat)) +
  geom_histogram(
    binwidth = 0.5,
    boundary = 0,
    closed = "right",
    fill = "steelblue",
    color = "white"
  ) +
  coord_cartesian(xlim = c(0, 2)) +
  labs(
    x = "Carat",
    y = "Count",
    title = "Diamond Carat Histogram (Bin Width = 0.5)"
  ) +
  theme_bw()

print(p_fine)
print(p_coarse)

ggsave("HW3_outputs/Problem1C_Fine.png",
       p_fine, width = 8, height = 5, dpi = 300)

ggsave("HW3_outputs/Problem1C_Coarse.png",
       p_coarse, width = 8, height = 5, dpi = 300)

# Count exact values from the original dataset

sum(gems$carat == 0.99)
sum(gems$carat == 1.00)

# Narrower bins reveal spikes at particular carat
# values, especially popular rounded measurements.
# Wider bins combine many different carat values,
# making these individual spikes harder to see.


# PROBLEM 2: CHICAGO PUBLIC LIBRARIES


# SETUP
# The required packages were loaded in the
# assignment's original setup.

dir.create("HW3_outputs", showWarnings = FALSE)
dir.create("HW3_outputs/raw", showWarnings = FALSE)

# STEP 1: USE THE REAL DATASETS

# library_data is the CSV we imported earlier.
# areas_raw contains the 77 Chicago community areas.

# If starting a fresh RStudio session,
# uncomment the following two lines:
#
# library_data <- read_csv(file.choose())
# areas_raw <- st_read(file.choose(), quiet = TRUE)

stopifnot(exists("library_data"))
stopifnot(exists("areas_raw"))

cat("Original library records:",
    nrow(library_data), "\n")

cat("Original community areas:",
    nrow(areas_raw), "\n")

stopifnot(nrow(library_data) >= 20)
stopifnot(nrow(areas_raw) >= 5)

# Document the original data sources.

data_sources <- data.frame(
  dataset = c(
    "Chicago Public Libraries",
    "Chicago Community Areas"
  ),
  publisher = c(
    "City of Chicago",
    "City of Chicago"
  ),
  source_url = c(
    "https://data.cityofchicago.org/d/x8fc-8rcq",
    "https://data.cityofchicago.org/d/igwz-8jzy"
  ),
  access_date = as.character(Sys.Date()),
  notes = c(
    "Library locations; downloaded CSV",
    "Community area boundary polygons"
  )
)

write_csv(
  data_sources,
  "HW3_outputs/data_sources.csv"
)

# Preserve a CSV copy of the imported library data.
# Do not modify the original downloaded file.

write_csv(
  library_data,
  "HW3_outputs/raw/chicago_libraries.csv"
)

# STEP 2: CLEAN LIBRARY LOCATIONS


raw_count <- nrow(library_data)

# The LOCATION field contains coordinates
# in the format (latitude, longitude).

# Extract both numbers from LOCATION.
# This uses base R, so no extra package is needed.

location_text <- as.character(library_data$LOCATION)

coord_pattern <- paste0(
  "^\\s*\\(\\s*",
  "(-?[0-9]+(?:\\.[0-9]+)?)",
  "\\s*,\\s*",
  "(-?[0-9]+(?:\\.[0-9]+)?)",
  "\\s*\\)\\s*$"
)

matched <- regexec(
  coord_pattern,
  location_text,
  perl = TRUE
)

parts <- regmatches(location_text, matched)

latitude <- vapply(
  parts,
  function(x) {
    if (length(x) == 3) as.numeric(x[2])
    else NA_real_
  },
  numeric(1)
)

longitude <- vapply(
  parts,
  function(x) {
    if (length(x) == 3) as.numeric(x[3])
    else NA_real_
  },
  numeric(1)
)

library_clean <- library_data

library_clean$latitude <- latitude
library_clean$longitude <- longitude

# Keep valid geographic coordinates.

valid_coords <- is.finite(latitude) &
  is.finite(longitude) &
  latitude >= -90 &
  latitude <= 90 &
  longitude >= -180 &
  longitude <= 180

bad_coordinate_count <- sum(!valid_coords)

library_clean <- library_clean[valid_coords, ]

# Create unique identifiers and facility names.

library_clean$site_id <- paste0(
  "LIB_",
  seq_len(nrow(library_clean))
)

library_clean$facility_name <- as.character(
  library_clean$BRANCH
)

missing_names <- is.na(library_clean$facility_name) |
  trimws(library_clean$facility_name) == ""

library_clean$facility_name[missing_names] <- paste0(
  "Unnamed library ",
  which(missing_names)
)

# Convert the CSV into geographic point data.

libraries <- st_as_sf(
  library_clean,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

cat("Usable coordinate records:",
    nrow(libraries), "\n")

cat("Invalid coordinates:",
    bad_coordinate_count, "\n")

# STEP 3: PREPARE STUDY BOUNDARIES


# Repair invalid boundary geometries.

areas <- st_make_valid(areas_raw)

# Transform into the same geographic CRS.

areas <- st_transform(areas, 4326)

# Remove empty boundary geometries.

areas <- areas[!st_is_empty(areas), ]

# Confirm the geometry type.

print(table(st_geometry_type(areas)))

# Combine the community areas into one
# Chicago study boundary.

study_boundary <- st_union(areas)

# Check which libraries fall within Chicago.

inside <- lengths(
  st_intersects(libraries, study_boundary)
) > 0

outside_count <- sum(!inside)

# Keep all eligible libraries in the study area.

libraries <- libraries[inside, ]

stopifnot(nrow(libraries) >= 20)

cat("Libraries inside Chicago:",
    nrow(libraries), "\n")

cat("Libraries outside Chicago:",
    outside_count, "\n")

# STEP 4: RECORD DUPLICATES AND REMOVALS


# Record libraries sharing the same coordinates.
# Do not remove them.

duplicate_coordinates <- sum(
  duplicated(
    st_as_text(st_geometry(libraries))
  )
)

removed_count <- raw_count - nrow(libraries)

cleaning_summary <- data.frame(
  measure = c(
    "Original records",
    "Retained records",
    "Removed records",
    "Invalid coordinates",
    "Outside study area",
    "Duplicate coordinates retained"
  ),
  count = c(
    raw_count,
    nrow(libraries),
    removed_count,
    bad_coordinate_count,
    outside_count,
    duplicate_coordinates
  )
)

print(cleaning_summary)

write_csv(
  cleaning_summary,
  "HW3_outputs/cleaning_summary.csv"
)

# Document which records were removed.

removal_audit <- library_data

removal_audit$reason <- ifelse(
  !valid_coords,
  "Invalid or missing coordinates",
  "Valid coordinates"
)

valid_rows <- which(valid_coords)

removal_audit$reason[valid_rows[!inside]] <-
  "Outside Chicago study boundary"

removal_audit <- removal_audit[
  removal_audit$reason != "Valid coordinates",
]

write_csv(
  removal_audit,
  "HW3_outputs/removal_audit.csv"
)

# STEP 5: CREATE PROJECTED SPATIAL DATA

# EPSG:26916
# NAD83 / UTM Zone 16N
# Units: meters

sites_m <- st_transform(libraries, 26916)

areas_m <- st_transform(areas, 26916)

cat("Projected CRS: EPSG 26916\n")
cat("Coordinate units: meters\n")

# Obtain projected X and Y coordinates.

site_xy <- st_coordinates(sites_m)

cleaned_sites <- data.frame(
  site_id = sites_m$site_id,
  facility_name = sites_m$facility_name,
  x_m = site_xy[,1],
  y_m = site_xy[,2]
)

write_csv(
  cleaned_sites,
  "HW3_outputs/cleaned_sites.csv"
)

# Save projected library points.

st_write(
  sites_m,
  "HW3_outputs/sites_m.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

# STEP 6: MAP LIBRARY LOCATIONS (p_sites)


map_caption <- paste(
  "Data: City of Chicago Public Libraries",
  "and Community Areas;",
  "accessed", Sys.Date()
)

p_sites <- ggplot() +
  geom_sf(
    data = areas_m,
    fill = "gray95",
    color = "gray65",
    linewidth = 0.2
  ) +
  geom_sf(
    data = sites_m,
    aes(color = "Public Library"),
    size = 1.6
  ) +
  scale_color_manual(
    values = c(
      "Public Library" = "blue"
    ),
    name = "Facility"
  ) +
  labs(
    title = "Chicago Public Library Locations",
    caption = map_caption
  ) +
  theme_bw()

print(p_sites)

ggsave(
  "HW3_outputs/p_sites.png",
  p_sites,
  width = 9,
  height = 8,
  dpi = 300
)

# STEP 7: CREATE 500 METER BUFFERS


# Make a circle with a 500 m radius
# around every library.

buffer_500 <- st_buffer(
  sites_m,
  dist = 500
)

# Combine overlapping buffers.
# Shared coverage is counted only once.

union_500 <- st_union(buffer_500)

buffer_500_sf <- st_sf(
  radius_m = 500,
  geometry = union_500
)

# STEP 8: CREATE 1000 METER BUFFERS


buffer_1000 <- st_buffer(
  sites_m,
  dist = 1000
)

union_1000 <- st_union(buffer_1000)

buffer_1000_sf <- st_sf(
  radius_m = 1000,
  geometry = union_1000
)

# STEP 9: CALCULATE COMBINED BUFFER AREAS


# st_area() gives square meters.
# Divide by 1,000,000 for square kilometers.

area_500 <- as.numeric(
  st_area(union_500)
) / 1000000

area_1000 <- as.numeric(
  st_area(union_1000)
) / 1000000

buffer_summary <- data.frame(
  radius_m = c(500, 1000),
  union_area_km2 = c(
    area_500,
    area_1000
  )
)

print(buffer_summary)

# Confirm 1000 m coverage is at least
# as large as 500 m coverage.

stopifnot(area_1000 >= area_500)

write_csv(
  buffer_summary,
  "HW3_outputs/buffer_summary.csv"
)

# Save both combined buffer zones.

st_write(
  buffer_500_sf,
  "HW3_outputs/buffer_500m.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

st_write(
  buffer_1000_sf,
  "HW3_outputs/buffer_1000m.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

# STEP 10: MAP 500 METER BUFFER (p_buffers)


p_buffers <- ggplot() +
  geom_sf(
    data = areas_m,
    fill = "gray95",
    color = "gray70",
    linewidth = 0.2
  ) +
  geom_sf(
    data = buffer_500_sf,
    aes(fill = "Within 500 meters"),
    alpha = 0.5,
    color = NA
  ) +
  geom_sf(
    data = sites_m,
    aes(color = "Public Library"),
    size = 1
  ) +
  scale_fill_manual(
    values = c(
      "Within 500 meters" = "skyblue"
    ),
    name = "Buffer coverage"
  ) +
  scale_color_manual(
    values = c(
      "Public Library" = "darkblue"
    ),
    name = "Facility"
  ) +
  labs(
    title = "Chicago Public Libraries: 500 m Coverage",
    caption = map_caption
  ) +
  theme_bw()

print(p_buffers)

ggsave(
  "HW3_outputs/p_buffers.png",
  p_buffers,
  width = 9,
  height = 8,
  dpi = 300
)

# STEP 11: INTERPRETATION


# The first map shows the locations of public
# libraries throughout Chicago's community areas.
#
# The second map shows the combined geographic
# area within 500 meters of at least one library.
#
# I used EPSG:26916 (NAD83 / UTM Zone 16N),
# a projected coordinate system measured in meters.
#
# Both 500 m and 1000 m buffers were calculated
# using the same retained library locations.
#
# I used st_union() to avoid counting overlapping
# buffer areas more than once.
#
# The combined buffer areas include portions outside
# Chicago's community-area boundaries.
#
# The 1000-meter buffer has an area greater than
# or equal to the 500-meter buffer area.
#
# The buffers represent straight-line geographic
# distances rather than actual travel distances.


# PROBLEM 3: CHICAGO LIBRARY COUNTS AND DENSITY


# STEP 1: PREPARE COMMUNITY AREA BOUNDARIES


# Reuse the projected data from Problem 2

stopifnot(exists("sites_m"))
stopifnot(exists("areas_m"))

# Make sure both datasets use the same CRS

stopifnot(st_crs(sites_m) == st_crs(areas_m))

# Inspect boundary columns

print(names(areas_m))

# Identify community area name and ID fields

name_col <- names(areas_m)[
  tolower(names(areas_m)) == "community"
][1]

id_candidates <- c(
  "area_numbe",
  "area_num_1",
  "area_num",
  "area_number"
)

id_col <- names(areas_m)[
  tolower(names(areas_m)) %in% id_candidates
][1]

if (is.na(name_col) || is.na(id_col)) {
  stop("Check the community area column names.")
}

# Create the district dataset

districts <- areas_m

districts$district_id <- as.character(
  districts[[id_col]]
)

districts$district_name <- as.character(
  districts[[name_col]]
)

# Validate district geometries

districts <- st_make_valid(districts)

stopifnot(
  !anyDuplicated(districts$district_id),
  all(!st_is_empty(districts)),
  all(st_is_valid(districts))
)

# Calculate area in square kilometers

districts$area_km2 <- as.numeric(
  st_area(districts)
) / 1000000

stopifnot(all(districts$area_km2 > 0))

cat("Number of community areas:",
    nrow(districts), "\n")

cat("Projected CRS:",
    st_crs(districts)$epsg, "\n")

# STEP 2: MATCH LIBRARIES TO COMMUNITY AREAS


# Identify which areas intersect each library

matches <- st_intersects(
  sites_m,
  districts
)

# Count the number of matching areas per site

n_matches <- lengths(matches)

# Assign a district only when exactly one area matches

matched_id <- vapply(
  matches,
  function(x) {
    if (length(x) == 1) {
      districts$district_id[x]
    } else {
      NA_character_
    }
  },
  character(1)
)

# Create site matching audit

site_match_audit <- data.frame(
  site_id = sites_m$site_id,
  facility_name = sites_m$facility_name,
  n_area_matches = n_matches,
  matched_district_id = matched_id
)

# Save audit table

write_csv(
  site_match_audit,
  "HW3_outputs/site_match_audit.csv"
)

# Report matching results

cat("Sites matching zero areas:",
    sum(n_matches == 0), "\n")

cat("Sites matching exactly one area:",
    sum(n_matches == 1), "\n")

cat("Sites matching multiple areas:",
    sum(n_matches > 1), "\n")

# Identify any sites without exactly one match

print(
  site_match_audit[
    site_match_audit$n_area_matches != 1,
  ]
)

# STEP 3: CALCULATE LIBRARY COUNTS AND DENSITY


# Count sites assigned to exactly one area

assigned_sites <- site_match_audit %>%
  filter(n_area_matches == 1) %>%
  count(
    matched_district_id,
    name = "n_sites"
  )

# Join counts to every community area
# Keep districts containing zero libraries

district_counts <- districts %>%
  left_join(
    assigned_sites,
    by = c(
      "district_id" = "matched_district_id"
    )
  ) %>%
  mutate(
    n_sites = replace_na(n_sites, 0L),
    sites_per_km2 = n_sites / area_km2
  )

# Validate that each assigned site is counted once

stopifnot(
  sum(district_counts$n_sites) ==
    sum(n_matches == 1)
)

# Create summary table

district_table <- district_counts %>%
  st_drop_geometry() %>%
  select(
    district_id,
    district_name,
    n_sites,
    area_km2,
    sites_per_km2
  ) %>%
  arrange(desc(n_sites))

# Display results

print(district_table)

# Save the required table

write_csv(
  district_table,
  "HW3_outputs/district_counts.csv"
)

# STEP 4: CREATE LIBRARY COUNT MAP


# Count sites that were not uniquely assigned

unassigned <- sum(n_matches != 1)

map_caption_3 <- paste0(
  "Sources: City of Chicago Public Libraries ",
  "and Community Areas; accessed ",
  Sys.Date(),
  "; sites not uniquely assigned: ",
  unassigned
)

p_count <- ggplot(district_counts) +
  geom_sf(
    aes(fill = n_sites),
    color = "white",
    linewidth = 0.2
  ) +
  scale_fill_gradient(
    low = "lightblue",
    high = "darkblue",
    name = "Library count",
    na.value = "gray70"
  ) +
  labs(
    title = "Public Library Counts by Chicago Community Area",
    caption = map_caption_3
  ) +
  theme_bw()

# Display map

print(p_count)

# Save map

ggsave(
  "HW3_outputs/p_count.png",
  p_count,
  width = 9,
  height = 8,
  dpi = 300
)

# STEP 5: CREATE LIBRARY DENSITY MAP


# Density = number of libraries / area in km2

p_density <- ggplot(district_counts) +
  geom_sf(
    aes(fill = sites_per_km2),
    color = "white",
    linewidth = 0.2
  ) +
  scale_fill_gradient(
    low = "lightyellow",
    high = "darkred",
    name = "Libraries / km2",
    na.value = "gray70"
  ) +
  labs(
    title = "Public Library Density by Chicago Community Area",
    caption = map_caption_3
  ) +
  theme_bw()

# Display map

print(p_density)

# Save map

ggsave(
  "HW3_outputs/p_density.png",
  p_density,
  width = 9,
  height = 8,
  dpi = 300
)

# STEP 6: MAP LIBRARIES OVER BOUNDARIES


p_sites_areas <- ggplot() +
  geom_sf(
    data = districts,
    fill = "gray95",
    color = "gray60",
    linewidth = 0.3
  ) +
  geom_sf(
    data = sites_m,
    aes(color = "Library"),
    size = 1.4
  ) +
  scale_color_manual(
    values = c(
      "Library" = "blue"
    ),
    name = "Facility"
  ) +
  labs(
    title = "Chicago Libraries and Community Area Boundaries",
    caption = map_caption_3
  ) +
  theme_bw()

# Display map

print(p_sites_areas)

# Save map

ggsave(
  "HW3_outputs/p_sites_areas.png",
  p_sites_areas,
  width = 9,
  height = 8,
  dpi = 300
)

# STEP 7: SAVE SPATIAL RESULTS


# Save cleaned community area boundaries

st_write(
  districts,
  "HW3_outputs/districts.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

# Save counts and density as spatial data

st_write(
  district_counts,
  "HW3_outputs/district_counts.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

# STEP 8: COMPARE COUNTS AND DENSITY


# Five areas with the most libraries

cat("\nTop 5 areas by library count:\n")

top_count <- district_table %>%
  arrange(desc(n_sites)) %>%
  head(5)

print(top_count)

# Five areas with the highest density

cat("\nTop 5 areas by library density:\n")

top_density <- district_table %>%
  arrange(desc(sites_per_km2)) %>%
  head(5)

print(top_density)


# STEP 9: INTERPRETATION


# The count map displays the absolute number of
# public libraries in each Chicago community area.
#
# The density map accounts for the geographic size
# of each area by dividing the library count by
# its area in square kilometers.
#
# A community area with more libraries may not
# have the highest library density because larger
# areas can have more facilities spread over
# a greater geographic area.
#
# All community areas were retained, including
# those with zero libraries.
#
# Only sites matching exactly one community area
# were included in the count and density tables.
#
# Sites with zero or multiple matches were recorded
# separately in site_match_audit.csv.
#
# The library locations and community boundaries
# come from separate City of Chicago datasets.
# Their publication or update dates may differ.
#
# Library density measures facilities per square
# kilometer, not accessibility by population
# or actual walking distance.





