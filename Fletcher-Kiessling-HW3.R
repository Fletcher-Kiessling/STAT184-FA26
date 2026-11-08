install.packages(c("ggplot2", "dplyr", "tidyr", "readr", "sf"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(sf)
vehicles <- ggplot2::mpg
gems <- ggplot2::diamonds

# Problem 1 Part A
ggplot(vehicles, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth(aes(color = drv), method = "lm", se = FALSE) +
  facet_wrap(~drv) +
  labs(
    x = "Engine displacement (litres)",
    y = "Highway fuel economy (mpg)",
    color = "drv"
  ) +
  theme_bw()

ggsave("part1A.png", width = 8, height = 6)

# Each drive type has its own graph, so each gets its own line.

# Problem 1 Part B
# Step 1
ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar() +
  labs(x = "drv", y = "count", fill = "factor(cyl)") +
  theme_bw()

ggsave("part1B_counts.png", width = 8, height = 6)

# Step 2
ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar(position = "fill") +
  labs(x = "drv", y = "Proportion within drive type",
       fill = "factor(cyl)") +
  theme_bw()

ggsave("part1B_proportions.png", width = 8, height = 6)

# The bars are the same height because they show percentages, not the actual number of cars in each group.

# Problem 1 Part C
# Step 1
small_gems <- gems %>%
  filter(carat <= 2)

# Step 2
nrow(small_gems)
nrow(gems) - nrow(small_gems)

# Step 3
p_fine <- ggplot(small_gems, aes(x = carat)) +
  geom_histogram(binwidth = 0.01, boundary = 0, closed = "right") +
  labs(title = "Bin width = 0.01", x = "Carat", y = "Count") +
  coord_cartesian(xlim = c(0, 2)) +
  theme_bw()
p_fine

ggsave("part1C_fine.png", p_fine, width = 8, height = 6)

# Step 4
p_coarse <- ggplot(small_gems, aes(x = carat)) +
  geom_histogram(binwidth = 0.5, boundary = 0, closed = "right") +
  labs(title = "Bin width = 0.5", x = "Carat", y = "Count") +
  coord_cartesian(xlim = c(0, 2)) +
  theme_bw()
p_coarse

ggsave("part1C_coarse.png", p_coarse, width = 8, height = 6)

# Step 5
sum(gems$carat == 0.99)
sum(gems$carat == 1.00)

# Smaller bins make it easier to see which exact carat sizes are more common.



# Problem 2
# Step 1
list.files()

# Step 2
libraries <- read_csv(list.files(pattern = "\\.csv$", full.names = TRUE)[1])
boundaries <- st_read(list.files(pattern = "\\.geojson$", full.names = TRUE)[1])
names(libraries)
nrow(libraries)

# Step 3
head(libraries$LOCATION)

# Step 4
libraries <- libraries %>%
  separate(LOCATION, into = c("lat", "lon"), sep = ",",
           remove = FALSE) %>%
  mutate(
    lat = as.numeric(gsub("\\(", "", lat)),
    lon = as.numeric(gsub("\\)", "", lon))
  )

head(libraries[, c("BRANCH", "lat", "lon")])
sum(is.na(libraries$lat) | is.na(libraries$lon))

# Step 5
sites_m <- libraries %>%
  mutate(site_id = row_number(),
         facility_name = BRANCH) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(26916)

nrow(sites_m)
st_crs(sites_m)$units_gdal

# Step 6
p_sites <- ggplot() +
  geom_sf(data = boundaries, fill = "white", color = "gray") +
  geom_sf(data = sites_m, color = "blue", size = 2) +
  labs(
    title = "Chicago Public Libraries",
    caption = "Source: Chicago Data Portal, downloaded Nov 8, 2026"
  ) +
  theme_bw()

p_sites

ggsave("p_sites.png", p_sites, width = 8, height = 6)

# Step 7
buffer_500 <- st_union(st_buffer(sites_m, 500))
buffer_1000 <- st_union(st_buffer(sites_m, 1000))

p_buffers <- ggplot() +
  geom_sf(data = boundaries, fill = "white", color = "gray") +
  geom_sf(data = buffer_500, aes(fill = "500m buffer"), alpha = 0.5) +
  geom_sf(data = sites_m, aes(color = "Libraries"), size = 1) +
  scale_fill_manual(values = c("500m buffer" = "lightblue")) +
  scale_color_manual(values = c("Libraries" = "blue")) +
  labs(
    title = "Chicago Library 500m Buffers",
    fill = "Area",
    color = "Sites",
    caption = "Source: Chicago Data Portal, downloaded Nov 8, 2026"
  ) +
  theme_bw()

p_buffers

ggsave("p_buffers.png", p_buffers, width = 8, height = 6)

# Step 8
buffer_summary <- data.frame(
  radius_m = c(500, 1000),
  union_area_km2 = c(
    as.numeric(st_area(buffer_500)) / 1000000,
    as.numeric(st_area(buffer_1000)) / 1000000
  )
)

buffer_summary

buffer_summary$union_area_km2[2] >= buffer_summary$union_area_km2[1]

write_csv(buffer_summary, "buffer_summary.csv")

# The 1000m buffers cover more area than the 500m buffers. Overlapping areas are only counted once.

# Step 9
nrow(libraries)
sum(is.na(libraries$lat) | is.na(libraries$lon))
sum(duplicated(libraries$BRANCH))
sum(duplicated(libraries[, c("lat", "lon")]))

# Step 10
# 82 libraries before and after cleaning.
# No missing coordinates or duplicates.
# No libraries were removed.
# CRS: EPSG 26916, units: meters.

write_csv(st_drop_geometry(sites_m), "cleaned_libraries.csv")

st_write(sites_m, "sites_m.gpkg", delete_dsn = TRUE)
st_write(buffer_500, "buffer_500.gpkg", delete_dsn = TRUE)
st_write(buffer_1000, "buffer_1000.gpkg", delete_dsn = TRUE)

# Step 11
data_sources <- data.frame(
  dataset = c("Chicago Libraries", "Chicago Community Areas"),
  source = c(
    "https://data.cityofchicago.org/d/x8fc-8rcq",
    "https://data.cityofchicago.org/d/cauq-8yn6"
  ),
  date_downloaded = c("2026-11-08", "2026-11-08")
)

write_csv(data_sources, "data_sources.csv")



# Problem 3
# Step 1
districts_m <- boundaries %>%
  st_make_valid() %>%
  st_transform(26916)
names(districts_m)

nrow(districts_m)
sum(st_is_empty(districts_m))
sum(!st_is_valid(districts_m))

# Step 2
names(districts_m)
head(st_drop_geometry(districts_m))

# Step 3
districts_m <- districts_m %>%
  mutate(
    district_id = area_numbe,
    district_name = community
  )
matches <- st_intersects(sites_m, districts_m)
site_match_audit <- data.frame(
  site_id = sites_m$site_id,
  n_matches = lengths(matches)
)
table(site_match_audit$n_matches)

write_csv(site_match_audit, "site_match_audit.csv")

# Step 4
districts_m$n_sites <- sapply(
  seq_len(nrow(districts_m)),
  function(i) sum(sapply(matches, function(x) i %in% x))
)
districts_m$area_km2 <- as.numeric(st_area(districts_m)) / 1000000
districts_m$sites_per_km2 <- districts_m$n_sites / districts_m$area_km2

district_counts <- districts_m %>%
  st_drop_geometry() %>%
  select(district_id, district_name, n_sites,
         area_km2, sites_per_km2)
View(district_counts)

sum(district_counts$n_sites)
all(district_counts$area_km2 > 0)

# Step 5
p_count <- ggplot(districts_m) +
  geom_sf(aes(fill = n_sites)) +
  scale_fill_gradient(
    low = "white", high = "blue",
    limits = c(0, max(districts_m$n_sites))
  ) +
  labs(
    title = "Libraries per Chicago Community Area",
    fill = "Number of Libraries",
    caption = "Source: Chicago Data Portal, Nov 8, 2026"
  ) +
  theme_bw()
p_count

ggsave("p_count.png", p_count, width = 8, height = 6)

# Step 6
p_density <- ggplot(districts_m) +
  geom_sf(aes(fill = sites_per_km2)) +
  scale_fill_gradient(
    low = "white", high = "red",
    limits = c(0, max(districts_m$sites_per_km2))
  ) +
  labs(
    title = "Library Density in Chicago",
    fill = "Libraries per km²",
    caption = "Source: Chicago Data Portal, Nov 8, 2026"
  ) +
  theme_bw()
p_density

ggsave("p_density.png", p_density, width = 8, height = 6)

# Step 7
table(site_match_audit$n_matches)
sum(district_counts$n_sites)
sum(district_counts$n_sites == 0)

# All 82 libraries matched one community area.
# Some areas have more libraries than others.
# Library density also depends on the size of each area.

st_write(districts_m, "districts_m.gpkg", delete_dsn = TRUE)
write_csv(district_counts, "district_counts.csv")

list.files()














