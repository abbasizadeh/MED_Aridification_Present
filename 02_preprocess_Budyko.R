library(ncdf4) 
library(raster) 
library(rgdal) 
library(ggplot2)
library(data.table)

p_files <- list.files("../shared/data_projects/med_datasets/2000_2019_data/sim/budyko/budyko_precip")
evap_files <- list.files("../shared/data_projects/med_datasets/2000_2019_data/sim/budyko/budyko_evap")

p_files[2]
evap_files[5]

# precip data
p_names_table <- read.table(text = p_files, sep = "_", as.is = TRUE)
p_names_table$V1 <- gsub("-", "_", x = p_names_table$V1)



# data frame to save the rasters and their names
p_columns = c("p_raster_name", "raster") 
p_data_frame = data.frame(matrix(nrow = length(p_files), ncol = length(p_columns))) 
colnames(p_data_frame) = p_columns
p_files_dir <- "../shared/data_projects/med_datasets/2000_2019_data/sim/budyko/budyko_precip/"

for(name in 1:length(p_files)){
  p_data_frame$p_raster_name[name] <- paste0("p_", p_names_table$V1[name])
  p_data_frame$raster[name] <- list(raster(paste0(p_files_dir, p_files[name])))
}

p_data_frame$p_raster_name[1]
plot(p_data_frame$raster[1][[1]])


# evap data
evap_names_table <- read.table(text = evap_files, sep = "_", as.is = TRUE)
evap_names_table$V1 <- gsub("-", "_", x = evap_names_table$V1)


# data frame to save the rasters and their names
e_columns = c("e_raster_name", "raster") 
e_data_frame = data.frame(matrix(nrow = length(evap_files), ncol = length(e_columns))) 
colnames(e_data_frame) = e_columns
e_files_dir <- "../shared/data_projects/med_datasets/2000_2019_data/sim/budyko/budyko_evap/"


for(name in 1:length(evap_files)){
  e_data_frame$e_raster_name[name] <- paste0(evap_names_table$V2[name], "_", evap_names_table$V1[name])
  e_data_frame$raster[name] <- list(raster(paste0(e_files_dir, evap_files[name])))
}

# check
e_data_frame$e_raster_name[2]
plot(e_data_frame$raster[[2]])

tes_era5 <- as.data.frame(e_data_frame$raster[[2]],
                xy = TRUE,
                long = TRUE,
                na.rm = TRUE)
# zApply()
# ?calc()
min(tes_era5$value)

colume_names = c("x", "y", "layer", "value", "combination") 
evap_index_data_frame = data.table(matrix(nrow = 0, ncol = length(colume_names))) 
arid_index_data_frame = data.table(matrix(nrow = 0, ncol = length(colume_names))) 
colnames(evap_index_data_frame) = colume_names
colnames(arid_index_data_frame) = colume_names


for(p_itr in 1:length(p_data_frame$p_raster_name)) {
  for (e_itr in 1:length(e_data_frame$e_raster_name)) {
    
    # calculation of pet/p and e/p
    evp_ard_index <- overlay(
      p_data_frame$raster[p_itr][[1]],
      e_data_frame$raster[e_itr][[1]],
      fun = function(r1, r2) {return(r1 / r2)}
    )
    
    # convert the raster into data.frame
    dummie <-
      as.data.frame(evp_ard_index,
                    xy = TRUE,
                    long = TRUE,
                    na.rm = TRUE)
    
    # add column of associated combination
    dummie$combination <-
      rep(paste0(
        e_data_frame$e_raster_name[e_itr],
        "_",
        p_data_frame$p_raster_name[p_itr]
      ))
    
    if (startsWith(e_data_frame$e_raster_name[e_itr], "pet")) {
      arid_index_data_frame <- rbind(arid_index_data_frame, dummie)
    } else{
      evap_index_data_frame <- rbind(evap_index_data_frame, dummie)
    }
    
  }
}

arid_index_data_frame
arid_index_data_frame$layer <- NULL
arid_index_data_frame

evap_index_data_frame
evap_index_data_frame$layer <- NULL
evap_index_data_frame

names(arid_index_data_frame) <- c("x", "y", "arid_index", "combination")
names(evap_index_data_frame) <- c("x", "y", "evap_index", "combination")

unique(evap_index_data_frame$combination)
plot(evap_index_data_frame$evap_index, type = "l")

plot(evap_index_data_frame[combination == "e_era5_p_era5" ]$evap_index, type = "l")
plot(evap_index_data_frame[combination == "e_gleam_p_merra2" ]$evap_index, type = "l")
plot(evap_index_data_frame[combination == "e_terraclimate_p_ncep_ncar" ]$evap_index, type = "l")
plot(evap_index_data_frame[combination == "e_gleam_p_era5" ]$evap_index, type = "l")
plot(evap_index_data_frame[combination == "e_terraclimate_p_merra2" ]$evap_index, type = "l")
plot(evap_index_data_frame[combination == "e_terraclimate_p_era5" ]$evap_index, type = "l")
plot(evap_index_data_frame[combination == "e_era5_p_ncep_ncar" ]$evap_index, type = "l")
plot(evap_index_data_frame[combination == "e_era5_p_merra2" ]$evap_index, type = "l")
plot(evap_index_data_frame[combination == "e_gleam_p_ncep_ncar" ]$evap_index, type = "l")


# remove the negative values
# arid_index_data_frame[arid_index < 0, arid_index := arid_index*-1]
arid_index_data_frame[arid_index >= 20, arid_index := NA]

unique(arid_index_data_frame$combination)
plot(arid_index_data_frame[combination == "pet_era5_p_era5" ]$arid_index, type = "l", )
plot(arid_index_data_frame[combination == "pet_gleam_p_merra2" ]$arid_index, type = "l")
plot(arid_index_data_frame[combination == "pet_terraclimate_p_ncep_ncar" ]$arid_index, type = "l")
plot(arid_index_data_frame[combination == "pet_gleam_p_era5" ]$arid_index, type = "l")
plot(arid_index_data_frame[combination == "pet_terraclimate_p_merra2" ]$arid_index, type = "l")
plot(arid_index_data_frame[combination == "pet_terraclimate_p_era5" ]$arid_index, type = "l")
plot(arid_index_data_frame[combination == "pet_era5_p_ncep_ncar" ]$arid_index, type = "l")
plot(arid_index_data_frame[combination == "pet_era5_p_merra2" ]$arid_index, type = "l")
plot(arid_index_data_frame[combination == "pet_gleam_p_ncep_ncar" ]$arid_index, type = "l")



path_save <- "../shared/data_projects/med_datasets/2000_2019_data/sim/budyko/evaporative_aridity_indices/"
saveRDS(object = arid_index_data_frame, file = paste0(path_save, "aridity_index.rds"))
saveRDS(object = evap_index_data_frame = paste0(path_save, "evaporative_index.rds"))




test_era5 <-  brick("../shared/data_projects/med_datasets/2000_2019_data/sim/evap/era5_pet_mm_med_200001_201912_025_monthly.nc")

plot(test_era5[[1]])
t_df <- as.data.frame(test_era5[[1]],xy = TRUE,
                      long = TRUE,
                      na.rm = TRUE)

which(t_df[t_df$value == min(t_df$value)])

test_nc <- nc_open("../shared/data_projects/med_datasets/2000_2019_data/sim/evap/era5_pet_mm_med_200001_201912_025_monthly.nc")
fillvalue <- ncatt_get(test_nc,"pet","_FillValue")
fillvalue$value
tmp_array <- ncvar_get(test_nc,"pet")
tmp_array[tmp_array==fillvalue$value] <- NA
tmp_slice <- tmp_array[,,1]
plot(raster(tmp_slice))

# writeRaster(evp_ard_index, path_save)

# dummie <- as.data.frame(evp_ard_index, xy = TRUE, long = TRUE, na.rm = TRUE)
# plot(evp_ard_index)
# 
# 
# 
# crs(evp_ard_index) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
# writeRaster(
#   evp_ard_index,
#   "../shared/data_projects/med_datasets/2000_2019_data/sim/budyko/evaporative_aridity_indices/evaporative_p-merra2_e-terraclimate.nc",
#   overwrite = TRUE,
#   format = "CDF",
#   varname = "evap_index",
#   varunit = "-",
#   longname = "evaporative index",
#   xname = "lon",
#   yname = "lat"
# )
# 
# 
# 
# p_data_list

