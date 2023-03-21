# # crop data for Mediterranean region for the period 2000 to 2019 
# min_lon = -10; max_lon = 40; min_lat = 30; max_lat = 45

library(ncdf4) 
library(raster) 
library(rgdal) 
library(ggplot2) 


OUTFILES_DIR <- "../shared/data_projects/med_datasets/2000_2019_data"
STUDY_PERIOD_START <- as.Date("2000-01-01")
STUDY_PERIOD_END <- as.Date("2019-12-31")
STUDY_AREA <- extent(-10.25, 40.25, 29.75, 45.25)

# crop function space-time
source('../ithaca/source/geo_functions.R')


# crop_save function
crop_save_nc <-
  function(link_to_raw_nc,
           link_plus_name_for_save,
           long_name,
           var_name,
           var_unit) {
    # obs precipitation data
    nc_to_brick <- brick(link_to_raw_nc)
    # crop
    crop_data <-
      crop_space_time(nc_to_brick,
                      STUDY_PERIOD_START,
                      STUDY_PERIOD_END,
                      STUDY_AREA)
    outfile <- paste0(OUTFILES_DIR, link_plus_name_for_save)
    writeRaster(
      crop_data,
      outfile,
      overwrite = TRUE,
      format = "CDF",
      varname = var_name,
      varunit = var_unit,
      longname = long_name,
      xname = "lon",
      yname = "lat"
    )
  }


# obs precip files
obs_precip <-
  list.files(
    path = "../shared/data/obs/precip/raw/",
    full.names = T
  )
obs_precip

# find the directory of required obs precip data in the obs_precip
index_obs_precip <- c()
for (dir in 1:length(obs_precip)) {
  if (grepl("ghcn|em-earth|chirps|cmorph|gmp-im|persiann|gpcc", obs_precip[dir]) &
      grepl("monthly", obs_precip[dir]) &
      grepl("land", obs_precip)[dir]){
    index_obs_precip <- append(index_obs_precip, dir)
  }
}

precip_obs_dir <- obs_precip[index_obs_precip]


# crop and save em-earth_tp_mm_land_195001_201912_025_monthly.nc
crop_save_nc(
  link_to_raw_nc = precip_obs_dir[1],
  link_plus_name_for_save = "/obs/precip/chirps_tp_mm_med_land_200001_201912_025_monthly.nc",
  long_name = "precipitation",
  var_name = "tp",
  var_unit = "mm"
)
 
# crop and save cmorph_tp_mm_land60s60n_199801_202112_025_monthly.nc
crop_save_nc(
  link_to_raw_nc = precip_obs_dir[2],
  link_plus_name_for_save = "/obs/precip/cmorph_tp_mm_med_land_200001_201912_025_monthly.nc",
  long_name = "precipitation",
  var_name = "tp",
  var_unit = "mm"
)

# crop and save em-earth_tp_mm_land_195001_201912_025_monthly.nc
crop_save_nc(
  link_to_raw_nc = precip_obs_dir[3],
  link_plus_name_for_save = "/obs/precip/em-earth_tp_mm_med_land_200001_201912_025_monthly.nc",
  long_name = "precipitation",
  var_name = "tp",
  var_unit = "mm"
)

# crop and save ghcn_tp_mm_land_190001_201505_025_monthly.nc" 
crop_save_nc(
  link_to_raw_nc = precip_obs_dir[4],
  link_plus_name_for_save = "/obs/precip/ghcn_tp_mm_med_land_200001_201912_025_monthly.nc",
  long_name = "precipitation",
  var_name = "tp",
  var_unit = "mm"
)

# crop and save gpcc_tp_mm_land_189101_201912_025_monthly.nc
crop_save_nc(
  link_to_raw_nc = precip_obs_dir[5],
  link_plus_name_for_save = "/obs/precip/gpcc_tp_mm_med_land_200001_201912_025_monthly.nc",
  long_name = "precipitation",
  var_name = "tp",
  var_unit = "mm"
)

# crop and save persiann_tp_mm_land60s60n_198301_202206_025_monthly.nc 
crop_save_nc(
  link_to_raw_nc = precip_obs_dir[6],
  link_plus_name_for_save = "/obs/precip/persiann_tp_mm_med_land_200001_201912_025_monthly.nc",
  long_name = "precipitation",
  var_name = "tp",
  var_unit = "mm"
)

# sim precip files
sim_precip <-
  list.files(
    path = "../shared/data/sim/precip/raw/",
    full.names = T
  )
sim_precip

index_sim_precip <- c()
for (dir in 1:length(sim_precip)) {
  if (grepl("era5|merra2|ncep-ncar", sim_precip[dir]) &
      grepl("monthly", sim_precip[dir]) &
      grepl("land", sim_precip)[dir]){
    index_sim_precip <- append(index_sim_precip, dir)
  }
}

precip_sim_dir <- sim_precip[index_sim_precip]

# crop and save era5_tp_mm_land_195901_202112_025_monthly.nc 
crop_save_nc(
  link_to_raw_nc = precip_sim_dir[1],
  link_plus_name_for_save = "/sim/precip/era5_tp_mm_med_land_200001_201912_025_monthly.nc",
  long_name = "precipitation",
  var_name = "tp",
  var_unit = "mm"
)

# crop and save merra2_tp_mm_land_198001_202301_025_monthly.nc 
crop_save_nc(
  link_to_raw_nc = precip_sim_dir[2],
  link_plus_name_for_save = "/sim/precip/merra2_tp_mm_med_land_200001_201912_025_monthly.nc",
  long_name = "precipitation",
  var_name = "tp",
  var_unit = "mm"
)

# crop and save merra2_tp_mm_land_198001_202301_025_monthly.nc 
crop_save_nc(
  link_to_raw_nc = precip_sim_dir[3],
  link_plus_name_for_save = "/sim/precip/ncep-ncar_tp_mm_med_land_200001_201912_025_monthly.nc",
  long_name = "precipitation",
  var_name = "tp",
  var_unit = "mm"
)


# Evap, PET

# sim e and pet files
sim_evap <-
  list.files(
    path = "../shared/data/sim/evap/raw/",
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

# crop and save era5_e_mm_global_195901_202112_025_monthly.nc 
crop_save_nc(
  link_to_raw_nc = evap_sim_dir[1],
  link_plus_name_for_save = "/sim/evap/era5_e_mm_med_200001_201912_025_monthly.nc",
  long_name = "evaporation",
  var_name = "e",
  var_unit = "mm"
)


# crop and save era5_pet_mm_global_195901_202112_025_monthly.nc
crop_save_nc(
  link_to_raw_nc = evap_sim_dir[2],
  link_plus_name_for_save = "/sim/evap/era5_pet_mm_med_200001_201912_025_monthly.nc",
  long_name = "potential evaporation",
  var_name = "pet",
  var_unit = "mm"
)

# crop and save gleam_e_mm_land_198001_202112_025_monthly.nc
crop_save_nc(
  link_to_raw_nc = evap_sim_dir[3],
  link_plus_name_for_save = "/sim/evap/gleam_e_mm_med_200001_201912_025_monthly.nc",
  long_name = "evaporation",
  var_name = "e",
  var_unit = "mm"
)

# crop and save gleam_pet_mm_land_198001_202112_025_monthly.nc
crop_save_nc(
  link_to_raw_nc = evap_sim_dir[4],
  link_plus_name_for_save = "/sim/evap/gleam_pet_mm_med_200001_201912_025_monthly.nc",
  long_name = "potential evaporation",
  var_name = "pet",
  var_unit = "mm"
)

# crop and save terraclimate_e_mm_land_195801_202112_025_monthly.nc
crop_save_nc(
  link_to_raw_nc = evap_sim_dir[5],
  link_plus_name_for_save = "/sim/evap/terraclimate_e_mm_med_200001_201912_025_monthly.nc",
  long_name = "evaporation",
  var_name = "e",
  var_unit = "mm"
)

# crop and save terraclimate_pet_mm_land_195801_202112_025_monthly.nc
crop_save_nc(
  link_to_raw_nc = evap_sim_dir[6],
  link_plus_name_for_save = "/sim/evap/terraclimate_pet_mm_med_200001_201912_025_monthly.nc",
  long_name = "potential evaporation",
  var_name = "pet",
  var_unit = "mm"
)
