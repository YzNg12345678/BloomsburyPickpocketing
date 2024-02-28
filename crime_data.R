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
colnames(data)[11] <- "category"


# checking types of variables in dataset
sapply(data, class)

data$type_of_crime <- as.factor(data$type_of_crime)
data$month <- as.factor(data$month)

data <- data |>
  select('Location', 'type_of_crime', 'category', 'latitude', 'longitude')

write.csv(data, "data.csv")
