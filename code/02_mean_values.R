# load global variables and packages
source("./code/source/global_variables.R")

# load functions
source('./code/source/functions.R')


precip_obs_files <- list.files("~/shared/data_projects/med_datasets/2000_2019_data/obs/precip/test")
precip_sim_files <- list.files("~/shared/data_projects/med_datasets/2000_2019_data/sim/precip/test")
evap_sim_files <- list.files("~/shared/data_projects/med_datasets/2000_2019_data/sim/evap/test")

PATH_OBS_PRECIP <- "~/shared/data_projects/med_datasets/2000_2019_data/obs/precip/test/"
PATH_SIM_PRECIP <- "~/shared/data_projects/med_datasets/2000_2019_data/sim/precip/test/"
PATH_SIM_EVAP <- "~/shared/data_projects/med_datasets/2000_2019_data/sim/evap/test/"


# calculation of mean annual observed precipitation for the whole period 2000-2019
for (precip_data_index in 1:length(precip_obs_files)) {
  
  # calculation of annual precipitation
  dummie_brick <- monthly_to_annual(paste0(PATH_OBS_PRECIP, precip_obs_files[[precip_data_index]]))
  
  # calculation of mean annual precipitation for the whole period
  dummie_brick <- mean(dummie_brick)
  
  # manipulation of the names
  p_names_table <-
    read.table(text = precip_obs_files[[precip_data_index]],
               sep = "_",
               as.is = TRUE)
  p_names_table$V5 <- "2000"
  p_names_table$V6 <- "2019"
  p_names_table$V8 <- "mean.nc"
  new_name <-
    with(p_names_table, paste(V1, V2, V3, V4, V5, V6, V7, V8, sep = "_"))
  
  save_dir <- paste0(PATH_OUTFILES, "/obs/budyko/budyko_precip/", new_name)
  dummie_brick@z[[1]] <- as.Date("2019-12-31", format = "%Y-%m-%d")
  save_nc(dummie_brick, paste0(PATH_OUTFILES, "/obs/budyko/budyko_precip/", new_name))
  
}



# calculation of mean annual simulation precipitation for the whole period 2000-2019
for (precip_data_index in 1:length(precip_sim_files)) {
  
  # calculation of annual precipitation
  dummie_brick <- monthly_to_annual(paste0(PATH_SIM_PRECIP, precip_sim_files[[precip_data_index]]))
  
  # calculation of mean annual precipitation for the whole period
  dummie_brick <- mean(dummie_brick)
  
  # manipulation of the names
  p_names_table <-
    read.table(text = precip_sim_files[[precip_data_index]],
               sep = "_",
               as.is = TRUE)
  p_names_table$V5 <- "2000"
  p_names_table$V6 <- "2019"
  p_names_table$V8 <- "mean.nc"
  new_name <-
    with(p_names_table, paste(V1, V2, V3, V4, V5, V6, V7, V8, sep = "_"))
  
  save_dir <- paste0(PATH_OUTFILES, "/sim/budyko/budyko_precip/")
  
  dummie_brick@z[[1]] <- as.Date("2019-12-31", format = "%Y-%m-%d")
  save_nc(dummie_brick, paste0(PATH_OUTFILES, "/sim/budyko/budyko_precip/test/", new_name))
  
 
}




# calculation of mean annual simulation evaporation for the whole period 2000-2019
for (evap_data_index in 1:length(evap_sim_files)) {
  
  # calculation of annual evaporation
  dummie_brick <- monthly_to_annual(paste0(PATH_SIM_EVAP, evap_sim_files[[evap_data_index]]))
  
  # calculation of mean annual evaporation for the whole period
  dummie_brick <- mean(dummie_brick)
  
  # manipulation of the names
  e_names_table <-
    read.table(text = evap_sim_files[[evap_data_index]],
               sep = "_",
               as.is = TRUE)
  e_names_table$V5 <- "2000"
  e_names_table$V6 <- "2019"
  e_names_table$V8 <- "mean.nc"
  new_name <-
    with(e_names_table, paste(V1, V2, V3, V4, V5, V6, V7, V8, sep = "_"))
  
  save_dir <- paste0(PATH_OUTFILES, "/sim/budyko/budyko_evap/test/")
  dummie_brick@z[[1]] <- as.Date("2019-12-31", format = "%Y-%m-%d")
  save_nc(dummie_brick, paste0(PATH_OUTFILES, "/sim/budyko/budyko_evap/test/", new_name))

}


# a <- brick("~/shared/data_projects/med_datasets/2000_2019_data/sim/budyko/budyko_precip/merra2_tp_mm_med_land.nc")
# b <- brick("~/shared/data_projects/med_datasets/2000_2019_data/sim/budyko/budyko_precip/test/merra2_tp_mm_mediterranean_2000_2019_25_mean.nc")
# 
# plot(a)
# plot(b)
# print(nc_open(paste0(PATH_SIM_EVAP, evap_sim_files[[evap_data_index]])))
# nc_close(paste0(PATH_SIM_EVAP, evap_sim_files[[evap_data_index]]))
# 
# 
# era_e <- brick(paste0(PATH_SIM_EVAP, evap_sim_files[[1]]))
# era_pet <- brick(paste0(PATH_SIM_EVAP, evap_sim_files[[2]]))
# plot(era_e$X2000.01.01)
# plot(era_pet$X2000.01.01)
# 
# evap_sim_files[1]
# a <- brick(paste0(PATH_OUTFILES, "/sim/budyko/budyko_evap/","terraclimate_e_mm_mediterranean_2000_2019_25_mean.nc"))
# b <- brick(paste0(PATH_OUTFILES, "/sim/budyko/budyko_evap/", "terraclimate_e_mm_med.nc"))
# plot(a)
# plot(b)
     