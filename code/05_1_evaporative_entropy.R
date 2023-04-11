# Calculation of entropy on the Budyko space (2D entropy)
library("entropy")


# load global variables and packages
source("./code/source/global_variables.R")

# load functions
source('./code/source/functions.R')


path_budyko_data <- "~/shared/data_projects/med_datasets/2000_2019_data/budyko/"

evaporative_data <- readRDS(paste0(path_budyko_data, '04_evaporative_data.rds'))

evaporative_data[, evap_comb := paste0(e_data, '_', precip_data)]

# define bins for entropy
evap_bin <- c(seq(from = 0, to = (ceiling(max(evaporative_data[,evap_index])) + ceiling(max(evaporative_data[,evap_index]))%%5) 
                  , by = 0.5))

# entropy for each kg and each comb
evaporative_data[, entropy_comb_kg := 
               {freq_tbl_evap <- table(cut(evap_index, breaks = evap_bin))
                entropy(freq_tbl_evap)
                 }, by = .(evap_comb, kg_code)]


# entropy for each kg
evaporative_data[, entropy_kg := 
               {freq_tbl_evap <- table(cut(evap_index, breaks = evap_bin))
                entropy(freq_tbl_evap)
                 }, by = .(kg_code)]


# evaporative_data[, entropy_kg := 
#                {freq_tbl_evap <- table(cut(evap_index, breaks = evap_bin))
#                 entropy(freq_tbl_evap)
#                  }, by = .(kg_code)]

# entropy for each comb for the whole med
evaporative_data[, entropy_comb := 
               {freq_tbl_evap <- table(cut(evap_index, breaks = evap_bin))
                entropy(freq_tbl_evap)
                 }, by = .(evap_comb)]

# entropy for precip dataset category for the whole med
evaporative_data[, entropy_precip_cat_evap := 
               {freq_tbl_evap <- table(cut(evap_index, breaks = evap_bin))
                entropy(freq_tbl_evap)
                 }, by = .(precip_category)]

# entropy for evap dataset category for the whole med
# evaporative_data[, entropy_pet_category := 
#                {freq_tbl_evap <- table(cut(evap_index, breaks = evap_bin))
#                 entropy(freq_tbl_evap)
#                  }, by = .(e_category)]

# # entropy for precip and evap datasets category for the whole med
# evaporative_data[, entropy_precip_cat_e_cat := 
#                {freq_tbl_evap <- table(cut(evap_index, breaks = evap_bin))
#                 entropy(freq_tbl_evap)
#                  }, by = .(precip_cat_evap, e_category)]


# entropy for precip dataset category for the whole med
evaporative_data[, entropy_precip_cat_evap_kg := 
               {freq_tbl_evap <- table(cut(evap_index, breaks = evap_bin))
                entropy(freq_tbl_evap)
                 }, by = .(precip_category, kg_code)]


# # entropy for evap dataset category for the whole med
# evaporative_data[, entropy_e_category_kg := 
#                {freq_tbl_evap <- table(cut(evap_index, breaks = evap_bin))
#                 entropy(freq_tbl_evap)
#                  }, by = .(e_category, kg_code)]

# # entropy for precip and evap datasets category for the whole med
# evaporative_data[, entropy_precip_cat_e_cat_kg := 
#                {freq_tbl_evap <- table(cut(evap_index, breaks = evap_bin))
#                 entropy(freq_tbl_evap)
#                  }, by = .(precip_cat_evap, e_category, kg_code)]

evaporative_data

head(evaporative_data)
tail(evaporative_data)


# saveRDS(budyko_data, paste0(path_budyko_data, "04_1_budyko_data_joint_entropy_KG.rds"))
saveRDS(evaporative_data, paste0(path_budyko_data, "05_1_evaporative_entropy_KG.rds"))
