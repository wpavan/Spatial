library(tidyverse)
library(plotly)
library(raster)
library(viridis)  # For color scale
library(RColorBrewer)
library(leaflet)

tz_rae <- read_csv("Tanzania-Rae.csv")
tz_polygon <- read_csv("Tanzania-polygon.csv")

plot <- ggplot() + 
  geom_polygon(data = tz_polygon, aes(x = long, y = lat), fill="white", color="gray") +
  coord_fixed(1.3) +
  geom_point(data = na.omit(tz_rae), aes(x = Longitude, y = Latitude, color = rae), shape=15, size=1) +
  labs(x = "Longitude", y = "Latitude", title = "Relative agronomic efficiency (RAE)") +
  theme_minimal() +
  scale_color_distiller("RAE", palette = "Spectral", limits=c(0,100),na.value="darkblue", direction = 1) 
plot

# Heatmap transforming data to raster
# Raster from X, Y, and Z
raster <- rasterFromXYZ(tz_rae %>% dplyr::select(lon = Longitude, lat = Latitude, z = rae))
crs(raster) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
raster_dis <- disaggregate(raster, 5, method='bilinear')

# Ploting raster
plot(raster_dis)
# or Plot the raster using ggplot
ggplot(data = raster_df) +
  geom_tile(aes(x = x, y = y, fill = z)) + 
  scale_fill_distiller(
    palette = "Spectral",   # Use the Spectral palette
    direction = 1,           # Set to -1 to reverse colors
    na.value = "transparent" # Make NA values transparent
  ) +           
  labs(
    title = "Relative agronomic efficiency (RAE)",
    x = "Longitude",
    y = "Latitude",
    fill = "RAE"       # Legend title
  ) +
  theme_minimal()
   
#### Using leaflet
# Create a palette using the original Spectral colors
col <- brewer.pal(11, "Spectral")

# Reverse the domain for the color mapping
palLegend <- colorNumeric(
  palette = col,
  domain = rev(seq(0, 100, 10)),  # Reverse the domain
  na.color = "transparent"
)

# Plot the raster with the inverted mapping
leaflet() %>% 
  addTiles() %>%
  addRasterImage(raster_dis, colors = palLegend, opacity = 0.6) %>%
  addLegend(
    pal = palLegend,
    values = seq(0, 100, 25),  # Keep the legend values in original order
    title = "RAE"
  ) 
