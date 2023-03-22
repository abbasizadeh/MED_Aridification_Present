library(ncdf4) 
library(raster) 
library(rgdal) 
library(ggplot2) 

precip_files <- list.files("../shared/data_projects/med_datasets/2000_2019_data/sim/budyko/budyko_precip")
evap_files <- list.files("../shared/data_projects/med_datasets/2000_2019_data/sim/budyko/budyko_evap")

precip_files[2]
evap_files[5]

precip_names_table <- read.table(text = precip_files, sep = "_", as.is = TRUE)
precip_data_list <- list()
precip_raster_list <- list()

for(name in 1:length(precip_files)){
  # open nc (precip)
  precip_data_list[[name]] <- assign(paste0("precip_", precip_names_table$V1[name]),
                                      nc_open(
                                        paste0(
                                          "../shared/data_projects/med_datasets/2000_2019_data/sim/budyko/budyko_precip/",
                                          precip_files[name]
                                        )
                                      ))

  # get lat and long
  lon <- ncvar_get(precip_data_list[[name]], "lon")
  lat <- ncvar_get(precip_data_list[[name]], "lat") #, verbose = F)
  t <- ncvar_get(precip_data_list[[name]], "time")
  
  # get precip data from nc
  precip.array <- ncvar_get(precip_data_list[[name]], "tp")
  dim(precip.array)
  
  # check out the gap fills
  fillvalue <- ncatt_get(precip_data_list[[name]], "tp", "_FillValue")
  fillvalue
  
  nc_close(precip_data_list[[name]])
  
  precip.array[precip.array == fillvalue$value] <- NA
  
  # replace -ve values with NA
  precip.array[precip.array < 0] <- NA
  
  # check
  min(precip.array, na.rm = TRUE)
  max(precip.array, na.rm = TRUE)
  dim(precip.array)
  # convert the array into raster
  precip_raster_list[[name]] <-
    raster(
      t(precip.array),
      xmn = min(lon),
      xmx = max(lon),
      ymn = min(lat),
      ymx = max(lat)
    )
}
plot(precip_raster_list[[3]])


evap_terraclimate <- nc_open(paste0("../shared/data_projects/med_datasets/2000_2019_data/sim/budyko_evap/", evap_files[5]))
print(precip_merra2)

# get lat and long
lon_e <- ncvar_get(evap_terraclimate, "lon")
lat_e <- ncvar_get(evap_terraclimate, "lat", verbose = F)
t_e <- ncvar_get(evap_terraclimate, "time")

# get precip data from nc
evap_terraclimate.array <- ncvar_get(evap_terraclimate, "e") # store the data in a 3-dimensional array
dim(evap_terraclimate.array)

# check out the gap fills
fillvalue <- ncatt_get(evap_terraclimate, "tp", "_FillValue")
fillvalue

nc_close(evap_terraclimate)

evap_terraclimate.array[evap_terraclimate.array == fillvalue$value] <- NA

# replace -ve values with NA
evap_terraclimate.array[evap_terraclimate.array < 0] <- NA
# check
min(evap_terraclimate.array, na.rm = TRUE)
max(evap_terraclimate.array, na.rm = TRUE)
dim(evap_terraclimate.array)

# convert the array into raster
evap_terraclimate_raster <-
  raster(
    t(evap_terraclimate.array),
    xmn = min(lon_e),
    xmx = max(lon_e),
    ymn = min(lat_e),
    ymx = max(lat_e)
    # crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
  )

plot(evap_terraclimate_raster)

evaporative_index <- evap_terraclimate_raster/merra_precip_raster
dim(evaporative_index)
plot(evaporative_index)

crs(evaporative_index) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
writeRaster(
  evaporative_index,
  "../shared/data_projects/med_datasets/2000_2019_data/sim/budyko/evaporative_aridity_indices/evaporative_p-merra2_e-terraclimate.nc",
  overwrite = TRUE,
  format = "CDF",
  varname = "evap_index",
  varunit = "-",
  longname = "evaporative index",
  xname = "lon",
  yname = "lat"
)



precip_data_list

