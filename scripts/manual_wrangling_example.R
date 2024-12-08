# Author: Andrew Latimer
# Date: Fall Quarter 2024
# Input:
# Code Description: Script to demonstrate loading and summarizing monthly climate data layers
# Output: 

library(tidyr)
library(dplyr)
library(terra)
library(sf)
library(purrr)

#### Constants

# Path to the directory containing the monthly climate data layers
CLIM_DIR = "/Users/jennifercribbs/Documents/TreePatrol.org/Analysis/Data/prism_data_monthly"

# Path to a file containing points of interest
PLOTS_FILE = "/Users/jennifercribbs/Documents/TreePatrol.org/Analysis/Data/plot_locs/ofo-plots.gpkg"


#### Workflow

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
names(r)

# Then you can plot layer(s) by name
plot(r[["PRISM_tmean_stable_4kmM3_202210_bil"]]) # october

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
get_5th = function(x) {
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
# Finally, could look at climate as a time series variable, which might be more informative when thinking about multiple disturbances (e.g. cycles of drought and fire with potentially long lag effects)