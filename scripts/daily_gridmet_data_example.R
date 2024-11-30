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
# see http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_MET_catalog.html 

# First we might need to set the "timeout option" to a longer time to make sure that the download function doesn't just give up before getting the whole file 
options(timeout = max(300, getOption("timeout"))) 

# Download the netCDF file for one year of daily maximum temperature data
#downloader::download(url = str_c("http://www.northwestknowledge.net/metdata/data/tmmx_2023.nc"), destfile = paste0(CLIM_DIR, "/tmmx_2022.nc"), mode = "wb")

# Load a netCDF file of daily data for one year as one big, multilayer raster
list.files(CLIM_DIR)
r <- rast(paste0(CLIM_DIR, "/tmmx_2023.nc"))
dim(r)

# Plot the first few layers of the raster
plot(r[[1:9]])

# What is the name of the layers?
names(r)[1:9]


# Now we can extract data from the raster using spatial features (points, polygons)

# One source of data might be a field GPS, like Derek's example last week. 

# Another source is polygons from a map such as fire perimeters or PLSS data. Using PLSS data as an example, let's try using centroids of spatial feature polygons to extract data.   

# Load PLSS grid for CA 
plss_grid <- st_read(paste0(PLSS_DIR, "/plss_grid.gpkg"))

# Check out some random set of grid cells 
ggplot(plss_grid[1:2000,]) + geom_sf()

# Scenario 1: we have a few specific TRS grid cells in mind to look at. 
# Subset this grid -- for now using some arbitrary locations 
plss_grid_subset = plss_grid |> dplyr::filter(MTRS %in% c("MDM-T15N-R10E-15", "MDM-T18N-R12E-15", "MDM-T20N-R10E-15", "MDM-T22N-R15E-15"))
ggplot(plss_grid_subset) + geom_sf()

# Scenario 2: We could also subset the grid based on a region like a single seed zone. 

# Load the USFS seed zones layer
seed_zones <- st_read(paste(PLSS_DIR, "seed_zones.gpkg", sep = "/"))

# Transform the seed zones to the same projection as the plss grid 
seed_zones = st_transform(seed_zones, st_crs(plss_grid))

# Take a look at one or more seed zones 
seed_zones |> 
  filter(SEED_ZONE == "531") |> 
  ggplot() + geom_sf()

# Now grab the PLSS grid cells that intersect with this seed zone
plss_grid_subset = st_intersection(plss_grid, seed_zones[seed_zones$SEED_ZONE == 531,])
ggplot(plss_grid_subset) + geom_sf()

# Get the centroids of our subset of TRS grid squares
focal_centroids = plss_grid_subset |> st_centroid()

# Extract the value of the climate raster at the centroid of the PLSS grid
extracted = terra::extract(r, focal_centroids, method = "bilinear")

# Add the site/grid information to the extracted data to keep it all together
d = cbind(focal_centroids, extracted)

# Now we have the climate data for our plots in a tabular format! Now let's do some tabular data wrangling.

names(d) # data are very wide because there's a column for each day! 

# Convert the data from wide to long data format 
d_long = pivot_longer(d, cols = starts_with("air_"), names_to = "climate_layer", values_to = "value")
names(d_long)

# Check out the "climate_layer" variable 
d_long$climate_layer[1:10]

# Split this column into info about the variable (air_temperature_day) and the date (days since 1/1/1900)
d_long = d_long  |>
  separate_wider_delim(climate_layer, names = c("clim_var", "days_since_1900"), delim = ".")

d_long |> select(clim_var, days_since_1900) |> head()

# Note that because this data set has a row for each day for each location, it's pretty big!
dim(d_long)

# Convert the "days_since_1900" date info and degrees K data to more understandable formats
d_long = d_long  |>
  mutate(date = lubridate::ymd("1900-01-01") + as.numeric(days_since_1900),
        tmax_c = value - 273.15) |> 
  mutate(year = year(date), month = month(date), day = day(date), 
         julian_date = lubridate::yday(date))

d_long |> select(days_since_1900, date, year, month, day, julian_date) |> head()

# Plot the time series for a few locations
ggplot(d_long |> filter(ID < 21), aes(x = date, y = tmax_c, group = ID, color = ID)) + geom_line() + labs(title = "Time series of tmax", x = "Date", y = "Temperature (C)") + theme_minimal()


#### Converting daily time series data into annual weather indices 

# We can define any kind of function we want!

# Example: growing degree days
calculate_gdd <- function(temp, base_temp = 10) {
  if (is.null(temp)) stop("No temperature data provided")
  gdd = sum(pmax(temp - base_temp, 0))
  return(gdd)
}

# Use the climate index function to summarize by year and point location
d_annual <- d_long |> 
  group_by(year, ID) |> 
  summarize(gdd = calculate_gdd(tmax_c)) |>
  cbind(focal_centroids)
  
# To visualize the results, plot growing degree days for the extracted points as a map
ggplot(d_annual) + geom_sf(aes(geometry = geom, color = gdd)) + scale_color_viridis_c(option = "plasma") + labs(title = "Growing degree days", fill = "GDD", x = "Longitude (decimal degrees E)", y = "Latitude (decimal degrees N)") + theme_minimal()


#### Next steps: we could automate downloading and extracting from netCDF files for many different years and multiple climate variables! 

