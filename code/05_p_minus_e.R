# load global variables and packages
source("./code/source/global_variables.R")

# load functions
source('./code/source/functions.R')

save_dir <- "~/shared/data_projects/med_datasets/2000_2019_data/p_minus_e"


precip_sim_files <- list.files("~/shared/data_projects/med_datasets/2000_2019_data/sim/precip")
precip_obs_files <- list.files("~/shared/data_projects/med_datasets/2000_2019_data/obs/precip")

evap_sim_files <- list.files("~/shared/data_projects/med_datasets/2000_2019_data/sim/evap")

precip_sim_files
evap_sim_files

# precip data
p_names_table <- read.table(text = precip_sim_files, sep = "_", as.is = TRUE)
p_names_table <- rbind(p_names_table, read.table(text = precip_obs_files, sep = "_", as.is = TRUE))
# p_names_table$V1 <- gsub("-", "_", x = p_names_table$V1)


# data frame to save the rasters and their names
p_columns = c("p_raster_name", "raster") 
p_data_frame = data.frame(matrix(nrow = length(precip_sim_files) + length(precip_obs_files), ncol = length(p_columns))) 
colnames(p_data_frame) = p_columns
precip_sim_files_dir <- "~/shared/data_projects/med_datasets/2000_2019_data/sim/precip/"
precip_obs_files_dir <- "~/shared/data_projects/med_datasets/2000_2019_data/obs/precip/"


# save precipitation data in p_data_frame data frame
for(name in 1:length(precip_sim_files)){
  p_data_frame$p_raster_name[name] <- paste0("p_", p_names_table$V1[name])
  p_data_frame$raster[name] <- list(brick(paste0(precip_sim_files_dir, precip_sim_files[name])))
}

itr_start <- length(precip_sim_files) + 1
itr_end <- length(precip_sim_files) + length(precip_obs_files)
for(name in  itr_start:itr_end){
  p_data_frame$p_raster_name[name] <- paste0("p_", p_names_table$V1[name])
  index <- name - 4
  p_data_frame$raster[name] <- list(brick(paste0(precip_obs_files_dir, precip_obs_files[[index]])))
}


p_data_frame$p_raster_name[1]
plot(p_data_frame$raster[1][[1]])


# evap data
evap_names_table <- read.table(text = evap_sim_files, sep = "_", as.is = TRUE)

# data frame to save the rasters and their names
e_columns <- c("e_raster_name", "raster") 
e_data_frame <- data.frame(matrix(nrow = length(evap_sim_files), ncol = length(e_columns))) 
colnames(e_data_frame) = e_columns
e_files_dir <- "~/shared/data_projects/med_datasets/2000_2019_data/sim/evap/"


for(name in 1:length(evap_sim_files)){
  if(!startsWith(paste0(evap_names_table$V2[name], "_", evap_names_table$V1[name]), "pet")){
    e_data_frame$e_raster_name[name] <- paste0(evap_names_table$V2[name], "_", evap_names_table$V1[name])
    e_data_frame$raster[name] <- list(brick(paste0(e_files_dir, evap_sim_files[name])))
  }
}
e_data_frame <- na.omit(e_data_frame)
# check
e_data_frame
# zApply()
# ?calc()


save_dir <- "~/shared/data_projects/med_datasets/2000_2019_data/p_minus_e/"

# ghcn has problem
for(p_itr in 1:length(p_data_frame$p_raster_name)) {
  for (e_itr in 1:length(e_data_frame$e_raster_name)) {
    
    # calculation of pet/p and e/p
    p_minus_e_dummie <- 
      overlay(
        p_data_frame$raster[p_itr][[1]],
        e_data_frame$raster[e_itr][[1]], 
        fun = function(r1, r2) {
        return(r1 - r2)})
    
    time_dummie <- getZ(p_data_frame$raster[p_itr][[1]] )
    # p_minus_e_dummie <- r1 - r2
    setZ(p_minus_e_dummie, time_dummie)
    
    p_minus_e_dummie@z <- list(time_dummie) 
    
    # add column of associated combination
    new_name <- paste0(p_data_frame$p_raster_name[p_itr], "_", "minus", "_", e_data_frame$e_raster_name[e_itr], "_","monthly")
    # data from era5 has been removed due to problem in pet_era5 (negative numbers)
    save_nc(p_minus_e_dummie, paste0(save_dir, new_name, ".nc"))
    
    
  }
}

