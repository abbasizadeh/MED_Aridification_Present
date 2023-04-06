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
budyko_data <- na.omit(budyko_data)
budyko_data[arid_index == max(arid_index, na.rm = T), ]
summary(budyko_data$arid_index)
budyko_data[evap_index == max(evap_index, na.rm = T), ]
summary(budyko_data$evap_index)
unique(budyko_data$combination)

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

# define a data frame (entropy_data_frame) to save entropy values for each KG class
colume_names = c("kg_code", 
                 "joint_entropy_kg", 
                 "number_kg", 
                 # "max_entropy_kg", 
                 # "mutual_information_kg",
                  "arid_entropy_kg",
                 "evap_entropy_kg")
entropy_data_frame = data.table(matrix(nrow = length(med_kg_codes), ncol = length(colume_names))) 
colnames(entropy_data_frame) = colume_names

# define bins for entropy
arid_bin <- c(0, 1.5, 2, 5, 33, ceiling(budyko_data[, max(arid_index)])) #seq(from = 0, to = ceiling(budyko_data_dummie[, max(arid_index)]), by = 1)
evap_bin <- c(0, 0.5, 1, 1.5, 2, 3, 5, 6, 7, 8, 9, 
              seq(from = 10, to = ceiling(budyko_data[, max(evap_index)])+ ceiling(budyko_data[, max(evap_index)])%%5 
                  , by = 5))
# calculate the number of each point in the each bin as a table

# calculate entropy of points on the Budyko space
for(kg_ite in 1:length(med_kg_codes)){
  
  budyko_data_dummie <- budyko_data[kg_code == med_kg_codes[kg_ite], ]
  
  freq_tbl <-
    table(cut(budyko_data_dummie[, arid_index], breaks = arid_bin), 
          cut(budyko_data_dummie[, evap_index], breaks = evap_bin))
  
  freq_tbl_arid <-
    table(cut(budyko_data_dummie[, arid_index], breaks = arid_bin))
  
  freq_tbl_evap <-
    table(cut(budyko_data_dummie[, evap_index], breaks = evap_bin))
  
  # KG code
  entropy_data_frame$kg_code[kg_ite] <- med_kg_codes[kg_ite]
  
  # joint entropy
  entropy_data_frame$joint_entropy_kg[kg_ite] <- entropy(freq_tbl)
  
  # number of grid cells
  entropy_data_frame$number_kg[kg_ite] <-
    length(budyko_data[kg_code == med_kg_codes[kg_ite], arid_index])
  
  # entropy_data_frame$max_possible_entropy[kg_ite] <- log(length(arid_bin)* length(evap_bin))
  # entropy_data_frame$max_entropy_kg[kg_ite] <- log(length(freq_tbl_evap) * length(freq_tbl_arid))
  
  # # mutual information
  # # compute marginal entropies
  # H1 = entropy(rowSums(freq_tbl))
  # H2 = entropy(colSums(freq_tbl))
  # # mutual entropy
  # entropy_data_frame$mutual_information_kg[kg_ite]  <- H1 + H2- entropy_data_frame$joint_entropy[kg_ite]
  
  entropy_data_frame$arid_entropy_kg[kg_ite] <- entropy(freq_tbl_arid)
  entropy_data_frame$evap_entropy_kg[kg_ite] <- entropy(freq_tbl_evap)
  
  
}

entropy_data_frame 
ggplot(entropy_data_frame) + geom_point(aes(x = number_kg, y = arid_entropy_kg))
# normalize the values of entropy according to the number of points (grids) in each KG class
# entropy_data_frame[, normalized_entropy:= entropy*number/sum(number)]


budyko_data <- merge(budyko_data, entropy_data_frame, by = 'kg_code')

budyko_data[kg_code == 0,]

head(budyko_data)
tail(budyko_data)


# saveRDS(budyko_data, paste0(path_budyko_data, "04_1_budyko_data_joint_entropy_KG.rds"))
saveRDS(entropy_data_frame, paste0(path_budyko_data, "04_1_joint_entropy_KG.rds"))





# entropy of each combination

# extract the combination of data frames
med_combination <- c(unique(budyko_data$combination))

# define a data frame (entropy_data_frame) to save entropy values for each combination
colume_names = c("combination", 
                 "joint_entropy_comb", 
                 "number_comb", 
                 # "mutual_info_comb",
                 "arid_entropy_comb",
                 "evap_entropy_comb"
                 )
entropy_data_comb_frame = data.table(matrix(nrow = length(med_combination), ncol = length(colume_names))) 
colnames(entropy_data_comb_frame) = colume_names

# the bins for all combinations are the same as the number of pixels is the same
# arid_bin_comb <- seq(from = 0, to = ceiling(budyko_data[, max(arid_index)]), by = 1)
# evap_bin_comb <- seq(from = 0, to = ceiling(budyko_data[, max(evap_index)]), by = 1)

# calculate entropy of points on the Budyko space
for(comb_ite in 1:length(med_combination)){
  
  budyko_data_dummie <- budyko_data[combination == med_combination[comb_ite], ]
  
  # calculate the number of each point in the each bin as a table
  freq_tbl <-
    table(cut(budyko_data_dummie[, arid_index], breaks = arid_bin), 
          cut(budyko_data_dummie[, evap_index], breaks = evap_bin))
  
  freq_tbl_arid <-
    table(cut(budyko_data_dummie[, arid_index], breaks = arid_bin))
  
  freq_tbl_evap <-
    table(cut(budyko_data_dummie[, evap_index], breaks = evap_bin))
  
  # joint entropy
  entropy_data_comb_frame$joint_entropy_comb[comb_ite] <- entropy(freq_tbl)
  
  entropy_data_comb_frame$number_comb[comb_ite] <-
    length(budyko_data[combination == med_combination[comb_ite], arid_index])
  
  entropy_data_comb_frame$combination[comb_ite] <- med_combination[comb_ite]
  
  # # mutual information
  # # compute marginal entropies
  # H1 = entropy(rowSums(freq_tbl))
  # H2 = entropy(colSums(freq_tbl))
  # # mutual entropy
  # entropy_data_comb_frame$mutual_info_comb[comb_ite]  <- H1 + H2- entropy_data_comb_frame$entropy[comb_ite]
  
  entropy_data_comb_frame$arid_entropy_comb[comb_ite] <- entropy(freq_tbl_arid)
  entropy_data_comb_frame$evap_entropy_comb[comb_ite] <- entropy(freq_tbl_evap)
  
}


entropy_data_comb_frame 
budyko_data <- merge(budyko_data, entropy_data_comb_frame, by = 'combination')

budyko_data[kg_code == 0,]

head(budyko_data)
tail(budyko_data)


saveRDS(budyko_data, paste0(path_budyko_data, "04_1_budyko_data_joint_entropy.rds"))
saveRDS(entropy_data_comb_frame, paste0(path_budyko_data, "04_1_joint_entropy_comb.rds"))




# normalize the values of entropy according to the number of points (grids) in each KG class
# entropy_data_frame[, normalized_entropy:= entropy*number/sum(number)]

# 
# ggplot(data = budyko_data[kg_code == 4,], aes(x = evap_index)) +
#   geom_histogram(aes(y=..density..), binwidth=.1, colour="black", fill="white") +
#   geom_density(alpha=.2, fill="#FF6666") +
#   facet_wrap(vars(factor(kg_code)))
# 
# ggplot(data = budyko_data[kg_code == 4 & arid_index < 5 & arid_index > 0, ], aes(x = arid_index)) +
#   geom_histogram(aes(y=..density..), binwidth=.1, colour="black", fill="white") +
#   geom_density(alpha=.2, fill="#FF6666") +
#   facet_wrap(vars(factor(kg_code)))
# 
# 
# 
# 

