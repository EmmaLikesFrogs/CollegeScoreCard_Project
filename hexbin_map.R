# library
install.packages("geojsonio")
install.packages("RColorBrewer")
install.packages("rgdal")
install.packages("rgeos")
install.packages("broom")
install.packages('mapproj')
library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(dplyr)
library(ggplot2)
library(rgeos)
library(mapproj)
library(plyr)
library(forcats)

# OBJECTIVE: The goal was to create a hexbin map of the total number of schools in each state.

# Download the Hexagones boundaries at geojson format here: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.

# Load this file. (Note: I stored in a folder called DATA)
spdf <- geojson_read("/Users/yuhanburgess/Documents/GitHub/DataMungingProject2/us_states_hexgrid.geojson",  what = "sp")
library(broom)

# Bit of reformatting
spdf@data <- spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf@data <- spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))

# Convert SpatialPolygonsDataFrame to a data frame
spdf_df <- fortify(spdf)

# Calculate the centroid of each hexagon to add the label:
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id = spdf@data$iso3166_2), group = spdf_df$group, row.names = NULL)

spdf_fortified <- spdf_df %>%
  left_join(. , centers, by=c("group"="group")) 
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="skyblue", color="white") +
  geom_text(data=centers, aes(x=x, y=y, label=id)) +
  theme_void() +
  coord_map()

# Now I can plot this shape easily as described before:

state_count <- read.csv('/Users/yuhanburgess/Documents/GitHub/DataMungingProject2/CollegeScorecard_Raw_Data_08032021/MERGED2019_20_PP.csv')%>%
  count(MERGED2019_20_PP,STABBR)

colnames(state_count)[1] <- "id.y"

state_data <- left_join(spdf_fortified, state_count, by = "id.y")

df_unique <- state_data %>%
  group_by(id.y) %>%
  slice(1)

## ERROR: polygons doesn't do the correct gradient level
ggplot() +
  geom_polygon(data = state_data, aes(x = long, y = lat, group = group, fill = freq), size = 0, alpha = 1.0) +
  geom_text(data = state_data, aes(x = x, y = y, label = id.y), color = "white", size = 3, alpha = 0.6) +
  scale_fill_gradient(low = "#69b3a2", high = "#FF0000", name = "Frequency") +
  labs(x = NULL, y = NULL) +
  theme_void()

