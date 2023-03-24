# load global variables and packages
source("./code/source/global_variables.R")

# load functions
source('./code/source/functions.R')


precip_files <- list.files("../shared/data_projects/med_datasets/2000_2019_data/sim/budyko/budyko_precip")
evaprecip_files <- list.files("../shared/data_projects/med_datasets/2000_2019_data/sim/budyko/budyko_evap")

precip_files[2]
evaprecip_files[5]

# precip data
p_names_table <- read.table(text = precip_files, sep = "_", as.is = TRUE)
# p_names_table$V1 <- gsub("-", "_", x = p_names_table$V1)


# data frame to save the rasters and their names
p_columns = c("p_raster_name", "raster") 
p_data_frame = data.frame(matrix(nrow = length(precip_files), ncol = length(p_columns))) 
colnames(p_data_frame) = p_columns
precip_files_dir <- "../shared/data_projects/med_datasets/2000_2019_data/sim/budyko/budyko_precip/"

for(name in 1:length(precip_files)){
  p_data_frame$p_raster_name[name] <- paste0("p_", p_names_table$V1[name])
  p_data_frame$raster[name] <- list(raster(paste0(precip_files_dir, precip_files[name])))
}

p_data_frame$p_raster_name[1]
plot(p_data_frame$raster[1][[1]])


# evap data
evap_names_table <- read.table(text = evaprecip_files, sep = "_", as.is = TRUE)
# evap_names_table$V1 <- gsub("-", "_", x = evap_names_table$V1)


# data frame to save the rasters and their names
e_columns = c("e_raster_name", "raster") 
e_data_frame = data.frame(matrix(nrow = length(evaprecip_files), ncol = length(e_columns))) 
colnames(e_data_frame) = e_columns
e_files_dir <- "../shared/data_projects/med_datasets/2000_2019_data/sim/budyko/budyko_evap/"


for(name in 1:length(evaprecip_files)){
  e_data_frame$e_raster_name[name] <- paste0(evap_names_table$V2[name], "_", evap_names_table$V1[name])
  e_data_frame$raster[name] <- list(raster(paste0(e_files_dir, evaprecip_files[name])))
}

# check
e_data_frame$e_raster_name[2]
# plot(e_data_frame$raster[[2]])
# 
# tes_era5 <- as.data.frame(e_data_frame$raster[[2]],
#                 xy = TRUE,
#                 long = TRUE,
#                 na.rm = F)
# e_era5 <- as.data.frame(e_data_frame$raster[[2]],
#                 xy = TRUE,
#                 long = TRUE,
#                 na.rm = F)
# e_gleam_era5 <- as.data.frame(e_data_frame$raster[[3]],
#                 xy = TRUE,
#                 long = TRUE,
#                 na.rm = F)

# min(tes_era5$value)


# zApply()
# ?calc()


# creating the evaporative and aridity indices for each pixel and save it to its corresponding data frame/table
colume_names = c("x", "y", "layer", "value", "combination") 
evap_index_data_frame = data.table(matrix(nrow = 0, ncol = length(colume_names))) 
arid_index_data_frame = data.table(matrix(nrow = 0, ncol = length(colume_names))) 
colnames(evap_index_data_frame) = colume_names
colnames(arid_index_data_frame) = colume_names


for(p_itr in 1:length(p_data_frame$p_raster_name)) {
  for (e_itr in 1:length(e_data_frame$e_raster_name)) {
    
    # calculation of pet/p and e/p
    evp_ard_index <- overlay(
      e_data_frame$raster[e_itr][[1]],
      p_data_frame$raster[p_itr][[1]],
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
    # data from era5 has been removed due to problem in pet_era5 (negative numbers)
    if (startsWith(e_data_frame$e_raster_name[e_itr], "pet") & !(e_data_frame$e_raster_name[e_itr] == "pet_era5") &
        !(startsWith(p_data_frame$p_raster_name[p_itr], "p_ncep-ncar"))) {
      arid_index_data_frame <- rbind(arid_index_data_frame, dummie)
    } else if(startsWith(e_data_frame$e_raster_name[e_itr], "e_") & !(e_data_frame$e_raster_name[e_itr] == "e_era5") &
              !(startsWith(p_data_frame$p_raster_name[p_itr], "p_ncep-ncar"))){
      evap_index_data_frame <- rbind(evap_index_data_frame, dummie)
    }
    
  }
}

# tidy up the data frames
arid_index_data_frame
arid_index_data_frame$layer <- NULL
arid_index_data_frame

evap_index_data_frame
evap_index_data_frame$layer <- NULL
evap_index_data_frame

names(arid_index_data_frame) <- c("x", "y", "arid_index", "arid_comb")
names(evap_index_data_frame) <- c("x", "y", "evap_index", "evap_comb")

# checking the values
unique(evap_index_data_frame$evap_comb)
plot(evap_index_data_frame$evap_index, type = "l")

plot(evap_index_data_frame[evap_comb == "e_era5_p_era5" ]$evap_index, type = "l") # e_era5 is removed
plot(evap_index_data_frame[evap_comb == "e_gleam_p_merra2" ]$evap_index, type = "l")
plot(evap_index_data_frame[evap_comb == "e_terraclimate_p_ncep-ncar" ]$evap_index, type = "l")
plot(evap_index_data_frame[evap_comb == "e_gleam_p_era5" ]$evap_index, type = "l")
plot(evap_index_data_frame[evap_comb == "e_terraclimate_p_merra2" ]$evap_index, type = "l")
plot(evap_index_data_frame[evap_comb == "e_terraclimate_p_era5" ]$evap_index, type = "l")
plot(evap_index_data_frame[evap_comb == "e_era5_p_ncep-ncar" ]$evap_index, type = "l") # e_era5 is removed
plot(evap_index_data_frame[evap_comb == "e_era5_p_merra2" ]$evap_index, type = "l") # e_era5 is removed
plot(evap_index_data_frame[evap_comb == "e_gleam_p_ncep-ncar" ]$evap_index, type = "l")


# remove the negative values
# arid_index_data_frame[arid_index < 0, arid_index := arid_index*-1]
# arid_index_data_frame[arid_index >= 20 | arid_index < 0, arid_index := NA]
# length(which(is.na(arid_index_data_frame$arid_index)))


unique(arid_index_data_frame$arid_comb)
plot(arid_index_data_frame[arid_comb == "pet_era5_p_era5", arid_index], type = "l", ) # pet_era5 is removed
plot(arid_index_data_frame[arid_comb == "pet_gleam_p_merra2", arid_index], type = "l")
plot(arid_index_data_frame[arid_comb == "pet_gleam_p_era5", arid_index], type = "l")
plot(arid_index_data_frame[arid_comb == "pet_terraclimate_p_merra2", arid_index], type = "l")
plot(arid_index_data_frame[arid_comb == "pet_terraclimate_p_era5", arid_index], type = "l")
plot(arid_index_data_frame[arid_comb == "pet_era5_p_merra2", arid_index], type = "l", ylim = c(-20, 20)) # pet_era5 is removed
plot(arid_index_data_frame[arid_comb == "pet_gleam_p_ncep_ncar", arid_index], type = "l")
plot(arid_index_data_frame[arid_comb == "pet_terraclimate_p_ncep-ncar", arid_index], type = "l")
plot(arid_index_data_frame[arid_comb == "pet_era5_p_ncep-ncar", arid_index], type = "l") # pet_era5 is removed


budyko_data <- cbind(arid_index_data_frame,evap_index_data_frame[, 3])
budyko_data <- budyko_data[ , c("x", "y", "arid_index", "evap_index", "arid_comb")]
names(budyko_data) <- c("x", "y", "arid_index", "evap_index", "combination")
path_save <- "../shared/data_projects/med_datasets/2000_2019_data/sim/budyko/evaporative_aridity_indices/"

saveRDS(object = budyko_data, file = paste0(path_save, "budyko_data.rds"))
# saveRDS(object = evap_index_data_frame, file = paste0(path_save, "evaporative_index.rds"))







