

brick_slopes <- function(dummie_brick, annual = NULL){
  if(!is.null(annual)){
    dummie_dates <- as.Date(names(dummie_brick), format = "X%Y.%m.%d")
    dummie_start <- dummie_dates[1]
    dummie_end <- tail(dummie_dates, 1)
    if ((month(dummie_start) != 1) & (month(dummie_end) != 12)){
      start_year <- as.Date(paste0(year(dummie_start) + 1,'-01-01'), format = '%Y-%m-%d')
      end_year <- as.Date(paste0(year(dummie_end) - 1,'-12-01'), format = '%Y-%m-%d')
    } else if ((month(dummie_start) != 1) & (month(dummie_end) == 12)){
      start_year <- as.Date(paste0(year(dummie_start) + 1,'-01-01'), format = '%Y-%m-%d')
      end_year <- dummie_end
    } else if ((month(dummie_start) == 1) & (month(dummie_end) != 12)){
      start_year <- dummie_start
      end_year <- as.Date(paste0(year(dummie_end) - 1,'-12-01'), format = '%Y-%m-%d')
    } else {
      start_year <- dummie_start
      end_year <- dummie_end
    }
    dummie_brick <- subset(dummie_brick, which(getZ(dummie_brick) >= start_year & (getZ(dummie_brick) <= end_year)))
    dummie_brick <- setZ(dummie_brick, seq(start_year, end_year, by = 'month'))
    dummie_brick <- zApply(dummie_brick, by = year,
                           fun = match.fun(annual), na.rm = TRUE)
    dummie_brick <- setZ(dummie_brick, seq(start_year, end_year, by = 'year'))
  }
  dummie_time <- 1:nlayers(dummie_brick)
  X <- cbind(1, dummie_time)
  invXtX <- solve(t(X) %*% X) %*% t(X)
  quickfun <- function(y) (invXtX %*% y)[2]
  dummie_slopes <- calc(dummie_brick, quickfun)
  return(dummie_slopes)
}

#' Lake mask
#'
#' Function to mask the lakes in a data set



# lake_mask <- function(x){
#   lsmask <- raster("~/shared/data_projects/ithaca/misc/water-bodies-mask_global_025.nc")
#   mask_ext <- extent(lsmask)
#   x_ext <- extent(x)
#   if (x_ext < mask_ext){
#     lsmask <- crop(lsmask, x_ext)
#   }
#   dummie <- mask(x, lsmask)
#   return(dummie)
# }

# Subset a brick object over space and time
crop_time <- function(dataset, start, end){
  time_filter <- which(getZ(dataset) >= start & 
                         (getZ(dataset) <= end))
  cropped <- subset(dataset, time_filter)
  dummie_names <- names(cropped)
  dummie_Z <- as.Date(dummie_names, format = "X%Y.%m.%d")
  cropped[cropped <= -9999] <- NA
  cropped <- setZ(cropped, dummie_Z)
  return(cropped)
}

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

# Transform a brick object to data.table format

brick_to_dt <- function(x){
  x_dt <- as.data.frame(x, xy = TRUE, long = TRUE, na.rm = TRUE)
  x_dt <- as.data.table(x_dt)
  setnames(x_dt, colnames(x_dt)[3], 'time')
  x_dt[, time := as.Date(time)]
  return(x_dt)
}

#' Grid area of each cell
#'
#' Function to compute area of each cell in m2
#' 

grid_area <- function(x){
  x$value <- 1
  coordinates(x) <- ~ lon + lat
  gridded(x) <- TRUE
  x <- raster(x)
  proj4string(x) <- CRS("+proj=longlat +datum=WGS84")
  dummie <- area(x, na.rm = TRUE)
  dummie <- as.data.frame(dummie, xy = TRUE, long = TRUE, na.rm = TRUE)
  dummie <- as.data.table(dummie)
  dummie <- dummie[, .(x, y, value)][, value := value * 1000000]
  setnames(dummie, c("x", "y", "value"), c("lon", "lat", "area"))
  return(dummie)
}

#' Spatial weights
#'
#' Function to compute weights of each cell in a given region
#' 

spatial_weight <- function(x){
  x$value <- 1
  coordinates(x) <- ~ lon + lat
  gridded(x) <- TRUE
  x <- raster(x)
  proj4string(x) <- CRS("+proj=longlat +datum=WGS84")
  dummie <- area(x, na.rm = TRUE, weights = TRUE)
  dummie <- as.data.frame(dummie, xy = TRUE, long = TRUE, na.rm = TRUE)
  dummie <- as.data.table(dummie)
  dummie <- dummie[, .(x, y, value)]
  setnames(dummie, c("x", "y", "value"), c("lon", "lat", "weight"))
  return(dummie)
}

#' Save .nc file
#'
#' Function to save data compatible with pRecipe in .nc file
#' 


save_nc <- function(dummie_nc, nc_out){
  lon <- xFromCol(dummie_nc) %>% round(., 4)
  lat <- yFromRow(dummie_nc) %>% round(., 4)
  time <- getZ(dummie_nc)
  if (is.character(time) | is.numeric(time)) {
    if (is.numeric(time)) {
      time <- as.character(time)
    }
    if (length(time[time == "00"]) >= 1) {
      time <- sub("^00$", "", time)
      time <- time[time != ""]
      time <- as.Date(time)
    } else if (!Reduce("|",grepl("-01", time))) {
      time <- as.numeric(time)
      dummie_origin <- "1970-01-01 00:00:00"
      time <- as.Date(time, origin = dummie_origin)
    } else {
      time <- as.Date(time)
    }
  }
  tp <- getValues(dummie_nc)
  tp[is.na(tp)] <- -9999
  deflon <- ncdim_def("lon", vals = lon, longname = "longitude",
                      units = "degrees_east")
  deflat <- ncdim_def("lat", vals = lat, longname = "latitude",
                      units = "degrees_north")
  deftime <- ncdim_def("time", vals = as.numeric(time), longname = "time",
                       units = "days since 1970-01-01 00:00:0.0",
                       calendar = "standard",
                       unlim = TRUE)
  deftp <- ncvar_def(name = "tp", units = "mm", 
                     list(deflon, deflat, deftime), 
                     missval = -9999,
                     compression = 4,
                     longname = "Total monthly precipitation",
                     prec = "float")
  ncoutput <- nc_create(nc_out, list(deftp), force_v4 = TRUE, verbose = FALSE)
  ncvar_put(ncoutput, deftp, tp)
  ncatt_put(ncoutput,"lon","axis","X") 
  ncatt_put(ncoutput,"lat","axis","Y")
  ncatt_put(ncoutput,"time","axis","T")
  nc_close(ncoutput)
}


# mask land
landmask <- function(x, keep_land = TRUE){
  lsmask <- raster("~/shared/data/geodata/masks/final/land_ocean/mask_land_ocean_025.nc")
  inv_mask <- !keep_land
  mask_ext <- extent(lsmask)
  x_ext <- extent(x)
  if (x_ext < mask_ext){
    lsmask <- crop(lsmask, x_ext)
  }
  dummie <- mask(x, lsmask, inverse = inv_mask)
  return(dummie)
}

# Function to convert monthly monthly nc to annual
# Z is not in time format
monthly_to_annual <- function(nc_file){
  
  dummie_brick <- brick(nc_file)
  
  indices <- format(as.Date(names(dummie_brick), format = "X%Y.%m.%d"), format = "%Y")
  indices <- as.numeric(indices)
  
  #sum layers
  annual_precip <- zApply(dummie_brick, indices, fun = sum)
  
  return(annual_precip)
}


precip_category_fun <-function(comb) {
  if(str_detect(comb, 'p_mswep|p_merra2|p_jra55|p_ncep-ncar|p_era5')){
    # cat <- 'reanalysis'
    return('reanalysis')
  }else if(str_detect(comb, 'p_terraclimate|p_em-earth|p_gpcc')){
    # cat <- 'observational'
    return('observational')
  }else if(str_detect(comb, 'p_cmorph|p_persiann|p_chirps')){
    # cat <- 'satellite_based'
    return('satellite_based')
  }
}

pet_category_fun <-function(comb) {
  if(str_detect(comb, 'pet_gleam|pet_terraclimate')){
    # cat <- 'reanalysis'
    return('combinational')
  }else if(str_detect(comb, 'pet_em-earth-od|pet_em-earth-hs|pet_em-earth-mb')){
    # cat <- 'observational'
    return('temperature_based')
  }else{
    return('-')
  }
}


# max entropy; calculated based on natural logarithm
max_entropy <- function(sample_size, bin_size){
  if(sample_size > bin_size){
    
    modulo <- sample_size %% bin_size
    repeatition <- sample_size %/% bin_size
    
    max_H <- modulo * (repeatition + 1)/sample_size * log((repeatition + 1)/sample_size) + 
      (bin_size - modulo) * ((repeatition)/sample_size)* log(repeatition/sample_size)
    
    return(-1 * max_H)
    
  }else{
    
    max_H <- log(sample_size)
    
    return(max_H)
    
  }
}
