# Calculation of entropy on the Budyko space (2D entropy)
library("entropy")


# load global variables and packages
source("./code/source/global_variables.R")

# load functions
source('./code/source/functions.R')


path_budyko_data <- "~/shared/data_projects/med_datasets/2000_2019_data/budyko/"

aridity_data <- readRDS(paste0(path_budyko_data, '04_aridity_data.rds'))


# define bins for entropy
arid_bin <- c(0, 1, 2, 5, 20, ceiling(max(aridity_data [,arid_index]))) #seq(from = 0, to = ceiling(budyko_data_dummie[, max(arid_index)]), by = 1)


# entropy for each kg and each comb
aridity_data[, entropy_kg := 
               {freq_tbl_arid <- table(cut(arid_index, breaks = arid_bin))
                entropy(freq_tbl_arid)
                }, by = .(kg_code)]


# entropy for each kg and each comb
aridity_data[, entropy_comb_kg := 
               {freq_tbl_arid <- table(cut(arid_index, breaks = arid_bin))
                entropy(freq_tbl_arid)
                 }, by = .(arid_comb, kg_code)]


# aridity_data[, entropy_kg := 
#                {freq_tbl_arid <- table(cut(arid_index, breaks = arid_bin))
#                 entropy(freq_tbl_arid)
#                  }, by = .(kg_code)]

# entropy for each comb for the whole med
aridity_data[, entropy_comb := 
               {freq_tbl_arid <- table(cut(arid_index, breaks = arid_bin))
                entropy(freq_tbl_arid)
                 }, by = .(arid_comb)]

# entropy for precip dataset category for the whole med
aridity_data[, entropy_precip_cat_arid := 
               {freq_tbl_arid <- table(cut(arid_index, breaks = arid_bin))
                entropy(freq_tbl_arid)
                 }, by = .(precip_cat_arid)]

# entropy for evap dataset category for the whole med
aridity_data[, entropy_pet_category := 
               {freq_tbl_arid <- table(cut(arid_index, breaks = arid_bin))
                entropy(freq_tbl_arid)
                 }, by = .(pet_category)]

# entropy for precip and evap datasets category for the whole med
aridity_data[, entropy_precip_cat_pet_cat := 
               {freq_tbl_arid <- table(cut(arid_index, breaks = arid_bin))
                entropy(freq_tbl_arid)
                 }, by = .(precip_cat_arid, pet_category)]


# entropy for precip dataset category for the whole med
aridity_data[, entropy_precip_cat_arid_kg := 
               {freq_tbl_arid <- table(cut(arid_index, breaks = arid_bin))
                entropy(freq_tbl_arid)
                 }, by = .(precip_cat_arid, kg_code)]

# entropy for evap dataset category for the whole med
aridity_data[, entropy_pet_category_kg := 
               {freq_tbl_arid <- table(cut(arid_index, breaks = arid_bin))
                entropy(freq_tbl_arid)
                 }, by = .(pet_category, kg_code)]

# entropy for precip and evap datasets category for the whole med
aridity_data[, entropy_precip_cat_pet_cat_kg := 
               {freq_tbl_arid <- table(cut(arid_index, breaks = arid_bin))
                entropy(freq_tbl_arid)
                 }, by = .(precip_cat_arid, pet_category, kg_code)]


aridity_data



# # define a data frame (entropy_data_frame) to save entropy values for each KG class
# med_kg_codes <- unique(aridity_data$kg_code)
# colume_names = c("kg_code", "kg_number", "kg_arid_entropy")
# entropy_data_kg = data.table(matrix(nrow = length(med_kg_codes), ncol = length(colume_names))) 
# colnames(entropy_data_kg) = colume_names
# 
# # calculate entropy of points on the Budyko space
# for(kg_ite in 1:length(med_kg_codes)){
#   
#   budyko_data_dummie <- aridity_data[kg_code == med_kg_codes[kg_ite], ]
#   
#   freq_tbl_arid <-
#     table(cut(budyko_data_dummie[,arid_index], breaks = arid_bin))
#   
#   # KG code
#   entropy_data_kg$kg_code[kg_ite] <- med_kg_codes[kg_ite]
#   
#   # number of grid cells
#   entropy_data_kg$kg_number[kg_ite] <- length(budyko_data_dummie[arid_comb == 'pet_em-earth-hs_p_cmorph', kg_code])
#   
#   # entropy_data_kg$max_possible_entropy[kg_ite] <- log(length(arid_bin)* length(evap_bin))
#   # entropy_data_kg$max_entropy_kg[kg_ite] <- log(length(freq_tbl_evap) * length(freq_tbl_arid))
#   entropy_data_kg$kg_arid_entropy[kg_ite] <- entropy(freq_tbl_arid)
#   
#   
#   
# }

# entropy_data_kg 
# ggplot(entropy_data_kg) + geom_point(aes(x = kg_number, y = kg_arid_entropy))
# normalize the values of entropy according to the number of points (grids) in each KG class
# entropy_data_kg[, normalized_entropy:= entropy*number/sum(number)]


# budyko_data_arid <- merge(budyko_data, entropy_data_kg, by = 'kg_code')

# budyko_data_arid[kg_code == 0,]

head(aridity_data)
tail(aridity_data)


# saveRDS(budyko_data, paste0(path_budyko_data, "04_1_budyko_data_joint_entropy_KG.rds"))
saveRDS(aridity_data, paste0(path_budyko_data, "05_0_aridity_entropy_KG.rds"))
