# load global variables and packages
source("./code/source/global_variables.R")

# load functions
source('./code/source/functions.R')

path_slope_files <- ("~/shared/data_projects/med_datasets/2000_2019_data/slopes_p_minus_e/")
budyko_data <- readRDS( "~/shared/data_projects/med_datasets/2000_2019_data/sim/budyko/evaporative_aridity_indices/budyko_data_04_1.rds")
save_dir <-  "~/shared/data_projects/med_datasets/2000_2019_data/sim/budyko/evaporative_aridity_indices/"



unique(budyko_data$combination)
slope_files <- list.files(path_slope_files)

# define the data frame to save the slope data
# col_names <- c('x', 'y', 'combination', 'slope', 'arid_index', 'evap_index', 'kg_code', 'estimated_omega')
col_names <- c('x', 'y', 'value', 'combination')
slope_data <- data.table(matrix(nrow = 0, ncol = length(col_names)))
names(slope_data) <- col_names


for (i in 1:length(slope_files)) {
  # open the nc file and convert it into the raster format
  raster_data_frame_dummie <- raster(paste0(path_slope_files, slope_files[i])) %>%
    as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE)
  
  # read the name of each file
  name_dummie <- read.table(text = slope_files[i], sep = "_", as.is = TRUE)
  
  # create combination name using the name of each file
  combination_dummie <- paste0("pet", "_", name_dummie$V5, "_", "p", "_", name_dummie$V2)
  
  # add the combination name to the data frame
  raster_data_frame_dummie$combination <- rep(combination_dummie, length(raster_data_frame_dummie$x))
  # raster_data_frame_dummie$x <- round(raster_data_frame_dummie$x, digits = 3)
  # raster_data_frame_dummie$y <- round(raster_data_frame_dummie$y, digits = 3)
  
  
  # remove extra the column
  raster_data_frame_dummie$layer <- NULL
  raster_data_frame_dummie$Z <- NULL
  # data_frame_dummie <- merge(raster_data_frame_dummie, budyko_data, by = c('x', 'y', 'combination'))
  # names(data_frame_dummie) <- col_names
  # add the results to the slope data frame
  slope_data <- rbind(slope_data, raster_data_frame_dummie)
  
  
}


# merge the slope data with budyko data
names(slope_data) <- c('x', 'y', 'slope', 'combination')
# saveRDS(slope_data, "slope_data")
# slope_budyko_data <- slope_data[budyko_data, on = .(x, y, combination), allow.cartesian = TRUE]

slope_budyko_data <- merge(budyko_data, slope_data, by = c('x', 'y', 'combination'))

slope_budyko_data[combination == "pet_terraclimate_p_persiann"]
slope_data[combination == "pet_terraclimate_p_persiann"]
budyko_data[combination == "pet_terraclimate_p_persiann"]

unique(slope_budyko_data$combination)
unique(slope_data$combination)
unique(budyko_data$combination)


saveRDS(slope_budyko_data, paste0(save_dir, "slope_budyko_data_07.rds"))

