# Authors: Andrew Latimer, Jenny Cribbs
# Date: 08 December 2024
# Input: prism_data_monthly from Box now in treepatrol.org/Analysis/Data (downloaded for 11/25/2024 lab meeting) and preliminary field data from SEKI
# Code Description: (1) Script to demonstrate loading and summarizing monthly climate data layers. (2) Figure out if our climate strata match the sampling objectives for the CALFIRE grant. 
# Output: An annual summary of ppt with tidy columns in a dataframe. 

library(tidyverse)
library(dplyr)
library(terra)
library(sf)
library(purrr)
library(tmap)
library(flextable)
library(ggplot2)
library(tmap)

#### Constants

# Path to the directory containing the monthly climate data layers
CLIM_DIR = "/Users/jennifercribbs/Documents/TreePatrol.org/Analysis/Data/prism_data_monthly"

# partially entered field data
fieldDataPlots <- read_csv("/Users/jennifercribbs/Documents/TreePatrol.org/Analysis/SEKIanalysis/SEKI_2024_PlotData_20241208.csv")

# remove veg only plot with no coordinates entered
fieldDataPlots <- fieldDataPlots %>%  
  dplyr::select(Plot_ID, Beg_Easting, Beg_Northing) %>% 
  filter(Beg_Easting != NA & Beg_Northing != NA & End_Easting != NA & End_Northing != NA) # not sure why we are loosing so many plots--maybe just NAs and the one PIMO7r
# convert to numeric--seems to already be
# fieldDataPlots$Beg_Easting <- as.numeric(fieldDataPlots$Beg_Easting)
# fieldDataPlots$Beg_Northing <- as.numeric(fieldDataPlots$Beg_Northing)

# Convert data frame to sf object with UTM projection (zone 11 NAD83)
utm_sf <- st_as_sf(fieldDataPlots, coords = c("Beg_Easting", "Beg_Northing"), crs = 26911) # zone 11, NAD83

# Convert to latitude/longitude (WGS84)
latlong_sf <- st_transform(utm_sf, crs = 4326)

# View the result
st_coordinates(latlong_sf)
plot(latlong_sf)

# check
summary(fieldDataPlots[, c("Beg_Easting", "Beg_Northing")])
st_crs(utm_sf)
st_bbox(utm_sf)

# Find park boundary
# Read the geodatabase
gdb_path <- "/Users/jennifercribbs/Downloads/Administrative_Boundaries_of_National_Park_System_Units.gdb/Administrative_Boundaries_of_National_Park_System_Units.gdb"

# List all layers in the geodatabase
layers <- st_layers(gdb_path)
print(layers)

# Read in all NPS boundaries
nps <- st_read("/Users/jennifercribbs/Documents/TreePatrol.org/Analysis/Data/nps_boundary/nps_boundary.shp") 
# filter out sequoia and kings canyon
SEKI <- nps %>% filter(UNIT_CODE == "SEQU" | UNIT_CODE == "KICA")
# quick look
plot(SEKI)
# Check the structure of the sf object
str(SEKI)

# Step 1: check crs and convert to lat/long
st_crs(SEKI) #6269 for SEKI 4326 for lat long
SEKI_wgs84 <- st_transform(SEKI, crs = 4326)
st_crs(latlong_sf)

# Step 2: Load the field data points
field_data <- read.csv("path/to/field_data.csv") # replace with actual path
field_data_sf <- st_as_sf(field_data, coords = c("longitude", "latitude"), crs = 4326)

# Step 3: Plot the map
ggplot() +
  geom_sf(data = SEKI, fill = "lightblue", color = "black", lwd = 0.5) +
  geom_sf(data = field_data_sf, color = "red", size = 3) +
  theme_minimal() +
  ggtitle("Field Data Points within SEKI Boundary")


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~not adapted
# create a table
# Extract plot name, latitude, and longitude
pila_plot_data <- st_coordinates(pila) %>%
  as.data.frame() %>%
  mutate(plot_name = pila$Name) %>%
  select(plot_name, X = X, Y = Y)  # Rename columns for clarity

# View the table
print(pila_plot_data)



# Create a flextable object
ft <- flextable(pila_plot_data)

# Set column names
colnames(ft) <- c("Plot Name", "Longitude", "Latitude")

# Print the flextable
ft

# save the table for report--can't remember what to use for flextable
#ggsave(ft, "/Users/jennifercribbs/Documents/Graduate School/Beetles Project/EMF_permit/pila_table.png")

# Check if 'pila' is a spatial points object
if (!inherits(pila, "sf")) {
  stop("'pila' is not a valid sf object. Please check your data.")
}

# Check if the directory exists
output_dir <- "/Users/jennifercribbs/Documents/Graduate School/Beetles Project/Waypoints/"
if (!dir.exists(output_dir)) {
  stop("Output directory does not exist. Please specify a valid directory path.")
}

# Convert to 2D geometry
pila <- st_zm(pila, drop = TRUE)

# Write to shapefile
st_write(pila, paste0(output_dir, "pila.shp"), append = FALSE)







## Extracting values from the rasters at point (e.g., plot) locations

# Load plot locations
plots = st_read(PLOTS_FILE) # 58 plots

# Extract values from the rasters at the plot locations
extracted = extract(r, plots, method = "bilinear")

# The extracted data does not have any of the attributes (or even geospatial locations) of the
# original plot data:
head(extracted)

# So we need to bind the extracted data onto the plot data. But first let's make the column names
# more intelligible.

# Drop the "ID" column at the start of the extracted data, so that we only have actual extracted
# data
extracted = extracted[, -1]

# A function to take the 5th part of a string (split by "_")
get_5th = function(x)#### Workflow
  
  ## Load the climate data layers
  
  # Get a list of all the relevant files
  rast_files = list.files(CLIM_DIR, pattern = "[0-9]{6}_bil.bil$") # pattern gets any thing with 6 digits before _bil.bil
# there are 6 plots/sites with 6 files each with a different file type that work together similar to a shapefile

# Load them all as a big many-layer raster
r = rast(paste0(CLIM_DIR, "/", rast_files))

## Manually inspecting the layers

# Example of viewing a single layer (i.e. one month)
plot(r[[7]]) # 7 is July

# What is the name of that layer?
names(r)[[1]] # first layer is January

# You could do the same thing for any of the layers. If you want to look up which layer to display,
# you can display all the names:
names(r) {
  parts = strsplit(x, "_")
  fifth = map(parts, 5)
  unlist(fifth)
}

# names now corresponds to YYYYMM
names(extracted) = get_5th(names(extracted))

# Prepend these names with tmean (or ppt_ etc)
names(extracted) = paste0("tmean_", names(extracted))
head(extracted)

# Join these columns onto the plot data. The order of the rows in the extracted data is the same as
# the order of the plot data.
plots = cbind(plots, extracted)
head(plots)

# Now we have the climate data for our plots in a tabluar format! Now let's do some tabular data
# wrangling.


## Tabular data wrangling

# Remove the geospatial data
d = st_drop_geometry(plots)
head(d)

# For data summaries, some operations are easier in a "long" format
d_long = pivot_longer(d, cols = starts_with("tmean_"), names_to = "prism_layer", values_to = "value")

head(d_long)

# Now we will split the "tmean" and "YYYYMM" part of the column names into their own columns
d_long = d_long  |>
  separate(prism_layer, into = c("clim_var", "yearmonth"), sep = "_")
head(d_long)

# Now we will split out the "YYYY" and "MM" parts of the "yearmonth" column
d_long = d_long  |>
  separate(yearmonth, into = c("year", "month"), sep = 4)

# Now we have a nice way of operating on the monthly climate data for each plot. For example, we can
# pull out tmean for February 2020
d_long |>
  filter(clim_var == "tmean", year == "2020", month == "02")

# Or we can get annual summaries for each plot and climate variable
annual_summary = d_long |>
  group_by(id, year, clim_var) |>
  summarize(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE))
head(annual_summary)

# May want to start with an annual summary for 2023 for YOSE and an average for 2022-2024 for SEKI
# Could also consider the complex WPBR life cycle and think about when temp and precip are most relevant in this context. # Then we could pull specific months or seasons of interest in light of disease biology.
# Finally, could look at climate as a time series variable, which might be more informative when thinking about multiple disturbances (e.g. 