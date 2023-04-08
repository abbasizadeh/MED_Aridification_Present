# Calculation of entropy on the Budyko space (2D entropy)
library("entropy")


# load global variables and packages
source("./code/source/global_variables.R")

# load functions
source('./code/source/functions.R')


path_budyko_data <- "~/shared/data_projects/med_datasets/2000_2019_data/budyko/"
path_Koppen_Geiger_raster <- "~/shared/data_projects/med_datasets/2000_2019_data/KG_classes/Beck_KG_present_025.tif"


# load aridity and evaporative indices from preprocess  
budyko_data <- readRDS(file = paste0(path_budyko_data, "03_budyko_data.rds"))
unique(budyko_data$combination)


# check the data
budyko_data[variable == 'arid_index'][value == max(value, na.rm = T), ]

# summary(budyko_data[variable == 'arid_index', value])
# 
# budyko_data[variable  == 'evap_index',][value == max(value, na.rm = T),]
# 
# summary(budyko_data[variable == 'evap_index', value])
# unique(budyko_data$combination)

# add Koppen Geiger (KG) classes to the budyko_data 
# convert the Budyko dataframe and into the shapefile
budyko_shape_file <- st_as_sf(budyko_data,
                              coords = c("x", "y"), 
                              crs = 4326)

# read Koppen Geiger raster
kg_raster <- raster(path_Koppen_Geiger_raster)

# extract the KG classes
kg_extract <-
  raster::extract(kg_raster, budyko_shape_file, method = 'simple', buffer = NULL,
                  small = FALSE, cellnumbers = FALSE, fun = NULL, na.rm = TRUE, layer, nl,
                  df = FALSE, factors = FALSE)

# add KG classes to the budyko data frame file
budyko_data$kg_code <- kg_extract


budyko_data[kg_code == 0, kg_code := NA]
unique(budyko_data$kg_code)
budyko_data <- na.omit(budyko_data)
# budyko_data <- na.omit(budyko_data)


# extract the Koppen Geiger (KG) codes in Mediterranean region and save it to med_kg_codes data frame
med_kg_codes <- c(unique(budyko_data$kg_code))


# split the budyko datafrme into two separate onces for aridity and evap indices
# aridity dataframe
budyko_data_arid <- budyko_data[variable == 'arid_index',]
budyko_data_arid <- spread(budyko_data_arid, key = variable, value = value)
names(budyko_data_arid) <- c('x', 'y', 'arid_comb', 'precip_cat_arid', 'pet_category', 'kg_code', 'arid_index')
saveRDS(budyko_data_arid, paste0(path_budyko_data, "04_aridity_data.rds"))


# evaporative dataframe
budyko_data_evap <- budyko_data[variable == 'evap_index',]
budyko_data_evap <- spread(budyko_data_evap, key = variable, value = value)
names(budyko_data_evap) <- c('x', 'y', 'evap_comb', 'precip_cat_evap', 'e_category', 'kg_code', 'evap_index')
saveRDS(budyko_data_evap, paste0(path_budyko_data, "04_evaporative_data.rds"))


# joint 
budyko_data <- join(budyko_data_arid, budyko_data_evap, by = c('x', 'y', 'kg_code'), type = "left")

# add some informative columns
unique(budyko_data$arid_comb)

budyko_data[, combination := paste0(arid_comb, '_&_',evap_comb)] 

unique(budyko_data$combination)
# budyko_data$arid_comb = NULL 
# budyko_data$evap_comb = NULL

budyko_data[, precip_cat := paste0('arid_', precip_cat_arid, '_&_', 'evap_',precip_cat_evap)] 

# budyko_data$arid_precip_cat = NULL 
# budyko_data$evap_precip_cat = NULL

budyko_data


saveRDS(budyko_data, paste0(path_budyko_data, "04_budyko_data.rds"))

