# load global variables and packages
source("./code/source/global_variables.R")

# load functions
source('./code/source/functions.R')


# Redefining saving nc function
saving_nc <- function(
         raster_data,
         nc_out ,
         var_name,
         var_unit,
         long_name) {
  # save nc
  writeRaster(
    raster_data,
    nc_out,
    overwrite = TRUE,
    format = "CDF",
    varname = var_name,
    varunit = var_unit,
    longname = long_name,
    xname = "lon",
    yname = "lat"
  )
}

#reading the p-e files 
p_e <- list.files("~/shared/data_projects/med_datasets/2000_2019_data/p_e/",full.names = T)
save_dir <- "~/shared/data_projects/med_datasets/2000_2019_data/slopes/"
long_name <- "slope of p-e"
var_name = "slope"
var_unit = "mm/year"

# calculation of slopes for the nc file
for (i in 1:length(p_e)) {
    brick_file <- brick(p_e[i])   #brick the nc file into raster layers  
    slope_file <- brick_slopes(brick_file)  #calculation of slopes by slope function 
    saving_nc(raster_data = slope_file,
              nc_out = paste0(save_dir, sub(".*//","",p_e[i])) ,
              var_name= var_name,
              var_unit = var_unit,
              long_name = long_name)
}

# save_nc(slope_file, sub(".*//","",p_e[i]))
# plot(slope_file)
# ss <- as.data.table(as.data.frame(slope_file,xy = TRUE, na.rm= TRUE))
# colnames(ss)[1:3] <- c("lon", "lat", "slope")
# ss
