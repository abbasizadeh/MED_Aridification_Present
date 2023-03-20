# extend for MED 
# min_lon = -10; max_lon = 40; min_lat = 30; max_lat = 45

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting

# precip_gpcp_data <- nc_open("../shared/data/obs/precip/raw/gpcp_tp_mm_global_197901_202205_025_monthly.nc")
# precip_gpcp_data <- nc_open("../shared/data_projects/med_datasets/test.nc")



crop_space_time <- function(dataset, start, end, crop_box){
  time_filter <- which(getZ(dataset) >= start & 
                         (getZ(dataset) <= end))
  filtered <- subset(dataset, time_filter)
  cropped <- crop(filtered, crop_box)
  dummie_names <- names(cropped)
  dummie_Z <- as.Date(dummie_names, format = "X%Y.%m.%d")
  cropped[cropped <= -9999] <- NA
  cropped <- setZ(cropped, dummie_Z)
  return(cropped)
}

# capture lon lat and time
lon <- ncvar_get(precip_gpcp_data, "lon")
lat <- ncvar_get(precip_gpcp_data, "lat")
t <- ncvar_get(precip_gpcp_data, "time")
tp <- ncvar_get(precip_gpcp_data, "tp")


#precip <- ncvar_get(precip_gpcp_data, "tp")

test <- brick("../shared/data/obs/precip/raw/gpcp_tp_mm_global_197901_202205_025_monthly.nc")
# r_brick <- brick(
#   tp,
#   xmn = min(lon),
#   xmx = max(lon),
#   ymn = min(lat),
#   ymx = max(lat),
#   #crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
# )

# b_precip <- brick(precip_gpcp_data)
study_area <- extent(-10, 
                     40, 
                     30, 
                     45)


ITHACA_PERIOD_START <- as.Date("2000-01-01")
ITHACA_PERIOD_END <- as.Date("2000-02-01")
crop_data <-crop_space_time(test, ITHACA_PERIOD_START, ITHACA_PERIOD_END, study_area)

plot(crop_data[[1:2]])


# write the raster layer (tmpin)
outfile <- "test_raster.nc"
# crs(crop_data) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
writeRaster(crop_data, outfile, overwrite=TRUE, format="CDF", varname="tmp", varunit="z-scores", 
            longname="test variable -- raster layer to netCDF", xname="lon", yname="lat")







