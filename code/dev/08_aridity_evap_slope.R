# load global variables and packages
source("./code/source/global_variables.R")

# load functions
source('./code/source/functions.R')


slope_budyko_data <- 
  readRDS( "~/shared/data_projects/med_datasets/2000_2019_data/budyko/07_slope_entropy_budyko_data.rds")

slope_indices_budyko_data <- slope_budyko_data[, {
  linear_model <- lm(prec_sum ~ year); coef(linear_model)[2]
}, by = .(lon, lat, dataset)]
setnames(prec_warm_slopes, "V1", "slope")


