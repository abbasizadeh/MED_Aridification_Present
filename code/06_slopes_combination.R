# load global variables and packages
source("./code/source/global_variables.R")

# load functions
source('./code/source/functions.R')



#reading the p-e files 
p_minus_e <- list.files("~/shared/data_projects/med_datasets/2000_2019_data/p_minus_e/test/",full.names = T)
save_dir <- "~/shared/data_projects/med_datasets/2000_2019_data/slopes_p_minus_e/test/"


# calculation of slopes for the nc file
for (i in 1:length(p_minus_e)) {
    brick_file <- brick(p_minus_e[i])   #brick the nc file into raster layers  
    slope_file <- brick_slopes(brick_file)  #calculation of slopes by slope function 
    
    nc_out = paste0(save_dir,str_match(p_minus_e[i], "//\\s*(.*?)\\s*\\.nc")[2], "_slope.nc")
    slope_file@z[[1]] <- as.Date("2019-12-31", format = "%Y-%m-%d")
    save_nc(slope_file, nc_out)
}

# save_nc(slope_file, sub(".*//","",p_minus_e[i]))

# ss <- as.data.table(as.data.frame(slope_file,xy = TRUE, na.rm= TRUE))

