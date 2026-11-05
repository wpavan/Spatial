library(tidyverse)
library(lubridate)

# Define custom column names to align with the data structure
custom_col_names <- c("DATE", "T2M", "TMIN", "TMAX", "TDEW", "RH2M", "RAIN", "WIND", "SRAD", "RH90")

# Read the file, skipping the problematic header
data <- read_table(
  file = "161100.WTH",
  skip = 16,                
  col_names = custom_col_names # Assign proper column names
)

# Convert DATE to standard date format
data <- data %>%
  mutate(DATE = as.Date(as.character(DATE), format = "%Y%j")) %>%
  filter(DATE >= as.Date("2008-01-01"), DATE < as.Date("2009-01-01"))

# Select only the DATE column and the variables of interest, then pivot
data_long <- data %>%
  dplyr::select(DATE, TMIN, TMAX, RAIN, WIND) %>%  # Keep only the necessary columns
  pivot_longer(cols = c(TMIN, TMAX, RAIN, WIND),  # Pivot the variables
               names_to = "Variable", 
               values_to = "Value")
# Create a new column to group TMIN and TMAX under "Temperature"
data_long <- data_long %>%
  mutate(Facet = case_when(
    Variable %in% c("TMIN", "TMAX") ~ "Temperature",
    Variable == "RAIN" ~ "Rainfall",
    Variable == "WIND" ~ "Wind Speed"
  ))

# Plot with three facets
plot <- ggplot(data_long, aes(x = DATE, y = Value)) +
  # Line chart for temperature (TMIN and TMAX)
  geom_line(data = filter(data_long, Facet == "Temperature"),
            aes(color = Variable)) +
  # Bar chart for rainfall (RAIN)
  geom_col(data = filter(data_long, Facet == "Rainfall"),
           fill = "blue", alpha = 0.5, show.legend = FALSE) +
  # Line chart for wind speed (WIND)
  geom_line(data = filter(data_long, Facet == "Wind Speed"),
            color = "green", show.legend = FALSE) +
  # Facet by the new "Facet" column
  facet_wrap(~Facet, scales = "free_y", ncol = 1) +
  # Add labels and titles
  labs(title = "Weather Data Visualization",
       x = "Date",
       y = "Value",
       color = "Temperature") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

plot
