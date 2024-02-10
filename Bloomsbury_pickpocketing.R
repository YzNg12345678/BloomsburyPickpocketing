library(dplyr)
library(readr)

data <- list.files(path = "/Users/recovery/Documents/Mapping/metropdata/", full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows()

# changing the names
colnames(data)[10] <- "type_of_crime"
colnames(data)[2] <- "month"
colnames(data)[5] <- "longitude"
colnames(data)[6] <- "latitude"

# checking types of variables in dataset
sapply(data, class)

data$type_of_crime <- as.factor(data$type_of_crime)
data$month <- as.factor(data$month)


## handling NA values -- getting rid of points with 0 spatial information
data$is_na = ifelse(is.na(data$longitude), TRUE, FALSE)
index = data$is_na == TRUE
data[index, "longitude"] <- 0
data[index, "latitude"] <- 0


## Importing boundary
library(sf)

boundary <- read_sf("https://mapit.mysociety.org/area/8592.geojson")

# Set the CRS
st_crs(boundary) <- st_crs("+init=epsg:4326")  
# Assuming the original CRS is WGS 84 (EPSG:4326)

# Transform the boundary to the desired CRS (if needed)
boundary <- st_transform(boundary, "+init=epsg:27700")  
# Assuming the desired CRS is British National Grid (EPSG:27700)

## transforming the geographical feature
## Filter the bloomsbury data
blm_data <- data |>
  st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326") |>
  st_transform("EPSG:27700") |>
  st_filter(boundary, .predicate = st_intersects)


## pickpocketing is from theft-from-person
pickpocket <- blm_data |>
  filter(type_of_crime == "Theft from the person") |>
  select(month, type_of_crime, geometry)


## making date variable (by creating pseudo date)
pickpocket$month <- as.Date(paste(pickpocket$month, "01", sep = "-"))


## merging maps
library(lubridate)
library(ggplot2)
library(tidyr)
library(sfhotspot)
library(ggspatial)
library(ggrepel)
library(gganimate)
library(viridis)
library(transformr)

kde_pickpocket <- pickpocket |>
  mutate(
    date = stringr::str_glue("{month}")
  ) |>
  group_by(date) |>
  group_modify(
      ~ hotspot_kde(data = pickpocket, grid = hotspot_grid(.x, cell_size = 50),
      bandwidth_adjust = 0.5)
      ) |>
  ungroup() |>
  st_as_sf() |>
  st_intersection(boundary)

map <- ggplot() + 
  annotation_map_tile(type = "cartolight", zoomin = 0) + 
  geom_sf(aes(fill = kde),
          data = kde_pickpocket,
          alpha = 0.3,
          colour = NA) +
  geom_sf(data = boundary, colour = "grey33", fill = NA) +
  transition_states(states = date) +
  scale_fill_distiller(
    palette = "RdPu",
    breaks = range(pull(kde_pickpocket, kde)),
    labels = c("lower","higher"),
    direction = 1
  ) + labs(
    title = "Pickpocketing in Bloombury, 2022.12-2023.12",
    subtitle = "Density of pickpockting: {closest_state}",
    caption = "Source of Data: https://data.police.uk/data/",
    fill = "density of \npickpocketing"
  ) +
  theme_void()
  
## Save the map

anim_save(
  filename = "Pickpocketing in Bloombury.gif",
  animation = animate(
    plot = map,
    height = 800,
    width = 800,
    units = "px"
  )
)

  




