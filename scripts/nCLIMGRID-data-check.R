# Download and look at example data from nCLIMGRID

library(terra)

ncml/nclimgrid-daily/2023/ncdd-202301-grd-scaled.nc

downloader::download(url = "https://www.ncei.noaa.gov/pub/data/daily-grids/v1-0-0/grids/2023/ncdd-202301-grd-scaled.nc", destfile ="./raw-data/ncdd-202301-grd-scaled.nc", mode = "wb")

r = rast("./raw-data/ncdd-202301-grd-scaled.nc")                     
r
names(r)
# Note nCLIMGRID files come with all the variables together as layers, one month at a time. The file we downloaded here was for Jan 2023. It has a layer for each of 31 days of the month for tmin, tmax, prcp, and tavg. 

plot(r$prcp_17)

# crop to extent of California
ext = c(-124.5, -114.1, 32.3, 42.1)
r_ca = crop(r, ext)

plot(r_ca$prcp_11) # look at an example precipitation day
