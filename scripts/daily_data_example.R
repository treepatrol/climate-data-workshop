# Script to demonstrate loading and summarizing daily data layers from gridmet

library(tidyverse)
library(terra)
library(sf)
library(downloader)
library(stringr)

#### Constants

# Path to the directory containing the monthly climate data layers
CLIM_DIR = "~/Library/CloudStorage/Box-Box/climate-data-workshop/gridmet_data_daily"

PLSS_DIR = "~/Library/CloudStorage/Box-Box/climate-data-workshop/plss_data"


#### Workflow

## Load the climate data layers

## Many climate layers, including daily gridmet data, are normally stored in compact nbinary format called netCDF 

# Download netcdf file from the THREDDs climate data server

# First set the timeout option to a longer time to make sure that the download function has enough time to get the whole file 
options(timeout = max(300, getOption("timeout"))) 

# Download the netCDF file for one year of daily maximum temperature data
downloader::download(url = str_c("http://www.northwestknowledge.net/metdata/data/tmmx_2023.nc"), destfile = paste0(CLIM_DIR, "/tmmx_2023.nc"), mode = "wb")

# Load the netCDF file as one big, multilayer raster
list.files(CLIM_DIR)
tmax <- rast(paste(CLIM_DIR, list.files(CLIM_DIR)[1], sep = "/"))
head(names(tmax))

# Plot the first few layers of the raster
plot(tmax[[1:9]])

# What is the name of the layers?
names(tmax)[1:9]

# Join these columns onto the plot data. The order of the rows in the extracted data is the same as
# the order of the plot data.
plots = cbind(plots, extracted)
head(plots)


#### Loading, looking, at, finding centroids of spatial feature objects  

# Load PLSS grid for CA 
plss_grid <- st_read(paste0(PLSS_DIR, "/plss_grid.gpkg"))
ggplot(plss_grid[1:2000,]) + geom_sf()

# Subset this grid -- for now using some arbitrary locations 
plss_grid_subset = plss_grid |> dplyr::filter(MTRS %in% c("MDM-T15N-R10E-15", "MDM-T15N-R15E-15", "MDM-T20N-R10E-15", "MDM-T20N-R15E-15"))
plot(plss_grid_subset[1])

# More usefully, maybe, we could subset the grid based on a single seed zone and elevation band combination 
seed_zones <- st_read(paste(PLSS_DIR, "seed_zones.gpkg", sep = "/"))
# Take a look at one or more seed zones 
seed_zones |> 
  filter(SEED_ZONE == "531") |> 
  ggplot() + geom_sf()
# Now grab the PLSS grid cells that intersect with this seed zone 
# Transform the seed zones to the same projection as the plss grid 
seed_zones = st_transform(seed_zones, st_crs(plss_grid))
plss_grid_subset = st_intersection(plss_grid, seed_zones[seed_zones$SEED_ZONE == 531,])
ggplot(plss_grid_subset) + geom_sf()

# Get the centroid of our subset of TRS grid squares

focal_centroids <- plss_grid_subset |> st_centroid()

# Extract the value of the climate raster at the centroid of the PLSS grid
extracted <- terra::extract(tmax, focal_centroids, method = "bilinear")

# Combine the extracted data with the PLSS grid data
d = cbind(plss_grid_subset, extracted)

# Optionally remove the geospatial data
#d = st_drop_geometry(plots)
#head(d)

# Now we have the climate data for our plots in a tabluar format! Now let's do some tabular data
# wrangling.

# Convert the data from wide to long data format 
d_long = pivot_longer(d, cols = starts_with("air_"), names_to = "gridmet_layer", values_to = "value")

# Split the label column into info about the variable and the date
d_long = d_long  |>
  separate_wider_delim(gridmet_layer, names = c("clim_var", "day_1900"), delim = ".")
head(d_long)

# Convert the day format (years since 1/1/1900) to more interpretable date info
d_long = d_long  |>
  mutate(date = lubridate::ymd("1900-01-01") + as.numeric(day_1900), tmax_c = value - 273.15) |> 
  mutate(year = year(date), month = month(date), day = day(date), julian_date = lubridate::yday(date))
head(d_long) 

# Plot the time series
ggplot(d_long, aes(x = date, y = tmax_c, group = ID, color = ID)) + geom_line() + labs(title = "Time series of tmax", x = "Date", y = "Temperature (C)") + theme_minimal()

# We could then define any kind of function we want to use to make summaries of the daily data -- such as climate indices

# Example: growing degree days
calculate_gdd <- function(temp, base_temp) {
  gdd = sum(pmax(temp - base_temp, 0))
  return(gdd)
}

# Use the climate index function to summarize by year and point location
d_annual <- d_long |> 
  group_by(year, ID) |> 
  summarize(gdd = calculate_gdd(tmax_c, 10)) |>
  cbind(focal_centroids)
  
# To visualize the results, plot growing degree days for the extracted points as a map
ggplot(d_annual) + geom_sf(aes(geometry = geom, color = gdd)) + scale_color_viridis_c(option = "plasma") + labs(title = "Growing degree days", fill = "GDD")


################
# To retrieve these we need to set the time period and the bounding box of the area we are interested in
# For now let's just work with data from one year which means we only have to access a single netcdf file. We could automate accessing them all later!
#clim_var <- "tmmx" # maximum temperature for example
#clim_var_name <- "air_temperature"
#clim_year <- 2023
#bbox <- c(40, -120, -118,  39) # west, east, south, north
#system(paste0("wget -P ", CLIM_DIR, " http://thredds.northwestknowledge.net:8080/thredds/ncss/MET/", clim_var, "/", clim_var, "_", clim_year, ".nc?var=", clim_var_name, "&north=", bbox[1], "&west=", bbox[2], "&east=", bbox[3], "&south=", bbox[4], "&disableProjSubset=on&horizStride=1&time_start=2024-01-01T00%3A00%3A00Z&time_end=2024-11-27T00%3A00%3A00Z&timeStride=1&accept=netcdf"))