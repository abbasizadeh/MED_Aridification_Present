# # crop data for Mediterranean region for the period 2000 to 2019 
# min_lon = -10; max_lon = 40; min_lat = 30; max_lat = 45


# load global variables and packages
source("./code/source/global_variables.R")

# load functions
source('./code/source/functions.R')


# read obs precipitaion files
obs_precip <- list.files(path = "~/shared/data/obs/precip/raw/",
                         full.names = T)
obs_precip

# find the directory of required obs precip data in the obs_precip
index_obs_precip <- c()
for (dir in 1:length(obs_precip)) {
  if (grepl("em-earth|chirps|cmorph|gmp-im|persiann|gpcc", obs_precip[dir]) &
      grepl("monthly", obs_precip[dir]) &
      grepl("land", obs_precip)[dir]){
    index_obs_precip <- append(index_obs_precip, dir)
  }
}

precip_obs_dir <- obs_precip[index_obs_precip]


# cropping the observed precipitation datasets for Mediterranean region
for (p_data in 1:length(precip_obs_dir)) {
  
  # manipulating the name of the cropped dataset
  dataset_name <-
    str_match(precip_obs_dir[p_data], "//\\s*(.*?)\\s*\\.nc")
  p_names_table <-
    read.table(text = dataset_name[, 2],
               sep = "_",
               as.is = TRUE)
  
  p_names_table$V4 <- "mediterranean"
  p_names_table$V5 <- "200001"
  p_names_table$V6 <- "201901"
  p_names_table$V7 <- "025"
  
  new_name <-
    with(p_names_table, paste(V1, V2, V3, V4, V5, V6, V7, V8, sep = "_"))
  new_name <- paste0(new_name, ".nc")
  
  nc_to_brick <- brick(precip_obs_dir[p_data])
  
  # crop
  crop_data <-
    crop_space_time(nc_to_brick,
                    STUDY_PERIOD_START,
                    STUDY_PERIOD_END,
                    STUDY_AREA)
  
  save_nc(crop_data, paste0(PATH_OUTFILES, "/obs/precip/",new_name))
}




# sim precip files
sim_precip <- list.files(path = "~/shared/data/sim/precip/raw/",
                         full.names = T)
sim_precip

index_sim_precip <- c()
for (dir in 1:length(sim_precip)) {
  if (grepl("era5|merra2|ncep-ncar|terraclimate", sim_precip[dir]) &
      grepl("monthly", sim_precip[dir]) &
      grepl("land", sim_precip)[dir]) {
    index_sim_precip <- append(index_sim_precip, dir)
  }
}

precip_sim_dir <- sim_precip[index_sim_precip]


# cropping the observed precipitation datasets for Mediterranean region
for(p_data in 1:length(precip_sim_dir)) {
  
  # manipulating the name of the cropped dataset
  # gsub("-", "_", x = precip_sim_dir[p_data])
  dataset_name <-
    str_match(precip_sim_dir[p_data], "//\\s*(.*?)\\s*\\.nc")
  
  
  p_names_table <-
    read.table(text = dataset_name[, 2],
               sep = "_",
               as.is = TRUE)
  
  p_names_table$V4 <- "mediterranean"
  p_names_table$V5 <- "200001"
  p_names_table$V6 <- "201901"
  p_names_table$V7 <- "025"
  
  new_name <-
    with(p_names_table, paste(V1, V2, V3, V4, V5, V6, V7, V8, sep = "_"))
  new_name <- paste0(new_name, ".nc")
  
  nc_to_brick <- brick(precip_sim_dir[p_data])
  # crop
  crop_data <-
    crop_space_time(nc_to_brick,
                    STUDY_PERIOD_START,
                    STUDY_PERIOD_END,
                    STUDY_AREA)
  
  save_nc(crop_data, paste0(PATH_OUTFILES, "/sim/precip/",new_name))
  
}



# Evap, PET
# sim e and pet files
sim_evap <-  list.files(path = "~/shared/data/sim/evap/raw/",
                        full.names = T
                        )
sim_evap

index_sim_evap <- c()
for (dir in 1:length(sim_evap)) {
  if (grepl("era5|terraclimate|gleam", sim_evap[dir]) &
      grepl("_e_|pet", sim_evap[dir])){
    index_sim_evap <- append(index_sim_evap, dir)
  }
}

evap_sim_dir <- sim_evap[index_sim_evap]

# save_dir <- paste0(PATH_OUTFILES, "/sim/evap/test/")
# cropping the observed precipitation datasets for Mediterranean region
for(e_data in 1:length(evap_sim_dir)) {
  
  # manipulating the name of the cropped dataset
  dataset_name <-
    str_match(evap_sim_dir[e_data], "//\\s*(.*?)\\s*\\.nc")
  
  
  p_names_table <-
    read.table(text = dataset_name[, 2],
               sep = "_",
               as.is = TRUE)
  
  p_names_table$V4 <- "mediterranean"
  p_names_table$V5 <- "200001"
  p_names_table$V6 <- "201901"
  p_names_table$V7 <- "025"
  
  new_name <-
    with(p_names_table, paste(V1, V2, V3, V4, V5, V6, V7, V8, sep = "_"))
  new_name <- paste0(new_name, ".nc")
  
  nc_to_brick <- brick(evap_sim_dir[e_data])
  # crop
  crop_data <-
    crop_space_time(nc_to_brick,
                    STUDY_PERIOD_START,
                    STUDY_PERIOD_END,
                    STUDY_AREA)
  
  save_nc(crop_data, paste0(PATH_OUTFILES, "/sim/evap/",new_name))
  
}



# # mask era5 data for land
# link_pet_ear5 <- 
#   "~/shared/data_projects/med_datasets/2000_2019_data/sim/evap/test/era5_pet_mm_mediterranean_200001_201901_025_monthly.nc"
# era5_pet <- brick(link_pet_ear5)
# dummie_time <- getZ(era5_pet)
# plot(era5_pet)
# era5_pet <- landmask(era5_pet)
# plot(era5_pet)
# era5_pet <- era5_pet * (-1)
# plot(era5_pet)
# era5_pet <- setZ(era5_pet, dummie_time)
# link_save_pet <- 
#   "~/shared/data_projects/med_datasets/2000_2019_data/sim/evap/test/era5_pet_mm_mediterranean_200001_201901_025_monthly.nc"
# save_nc(era5_pet, link_save_pet)
# 
# 
# link_e_ear5 <- 
#   "~/shared/data_projects/med_datasets/2000_2019_data/sim/evap/test/era5_e_mm_mediterranean_200001_201901_025_monthly.nc"
# era5_e <- brick(link_e_ear5)
# plot(era5_e)
# era5_e <- landmask(era5_e)
# plot(era5_e)
# link_save_e <- 
#   "~/shared/data_projects/med_datasets/2000_2019_data/sim/evap/test/era5_e_mm_mediterranean_200001_201901_025_monthly.nc"
# 
# save_nc(era5_e, link_save_e)



# t <- brick(link_save_e)
# plot(t)


# # crop KG class
# KG <- raster("~/MED_Aridification_Present/data/archive/Beck_KG_present_025.tif")
# # crop
# crop_data <-
#   crop_space_time(KG,
#                   STUDY_PERIOD_START,
#                   STUDY_PERIOD_END,
#                   STUDY_AREA)
# 
# plot(crop_data)
# # save nc
# writeRaster(
#   crop_data,
#   "~/MED_Aridification_Present/data/archive/KG_classes/Beck_KG_present_mediterranian_025.tif",
#   overwrite = TRUE,
#   format = "GTiff",
# )
# plot(KG)
