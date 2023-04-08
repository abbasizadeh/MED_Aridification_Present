# load global variables and packages
source("./code/source/global_variables.R")

# load functions
source('./code/source/functions.R')


path_p_minus_e <- "~/shared/data_projects/med_datasets/2000_2019_data/p_minus_e/"
path_save <- "~/shared/data_projects/med_datasets/2000_2019_data/slopes_p_minus_e/"

#reading the p-e files 
p_minus_e <- list.files(path_p_minus_e,full.names = T)


# calculate and save slopes 
for (i in 1:length(p_minus_e)) {
    
  brick_file <- brick(p_minus_e[i])   #brick the nc file into raster layers  

  slope_file <- brick_slopes(brick_file)  #calculation of slopes by slope function 
    
  nc_out = paste0(path_save, str_match(p_minus_e[i], "//\\s*(.*?)\\s*\\.nc")[2], "_slope.nc")
    
  slope_file@z[[1]] <- as.Date("2019-12-31", format = "%Y-%m-%d")
    
  save_nc(slope_file, nc_out)
}



