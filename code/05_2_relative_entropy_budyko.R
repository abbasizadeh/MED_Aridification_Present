# Calculation of entropy on the Budyko space (2D entropy)
library("entropy")


# load global variables and packages
source("./code/source/global_variables.R")

# load functions
source('./code/source/functions.R')


path_budyko_data <- "~/shared/data_projects/med_datasets/2000_2019_data/budyko/"


budyko_data <- readRDS(paste0(path_budyko_data, '04_budyko_data.rds'))



# define bins for entropy
arid_bin <- c(0, 1, 2, 5, 20, ceiling(max(budyko_data[,arid_index]))) 
evap_bin <- c(seq(from = 0, 
                  to = (ceiling(max(budyko_data[,evap_index])) + ceiling(max(budyko_data[,evap_index]))%%5) , 
                  by = 0.5))


# relative entropy for each kg and each comb
budyko_data[, relaive_entropy_comb_kg := 
                   {
                     freq_tbl <-
                       table(cut(arid_index, breaks = arid_bin), 
                             cut(evap_index, breaks = evap_bin))
                     entropy(freq_tbl)
                   }, 
            by = .(combination, kg_code)]

budyko_data[, relaive_entropy_comb := 
                   {
                     freq_tbl <-
                       table(cut(arid_index, breaks = arid_bin), 
                             cut(evap_index, breaks = evap_bin))
                     entropy(freq_tbl)
                   }, 
            by = .(combination)]


# relative entropy for each kg 
budyko_data[, relaive_entropy_precip_cat_arid_kg:= 
                   {
                     freq_tbl <-
                       table(cut(arid_index, breaks = arid_bin), 
                             cut(evap_index, breaks = evap_bin))
                     entropy(freq_tbl)
                   }, 
            by = .(precip_cat_arid, kg_code)]

budyko_data[, relaive_entropy_precip_cat_arid:= 
                   {
                     freq_tbl <-
                       table(cut(arid_index, breaks = arid_bin), 
                             cut(evap_index, breaks = evap_bin))
                     entropy(freq_tbl)
                   }, 
            by = .(precip_cat_arid)]


# relative entropy for each kg
budyko_data[, relaive_entropy_precip_cat_kg:= 
                   {
                     freq_tbl <-
                       table(cut(arid_index, breaks = arid_bin), 
                             cut(evap_index, breaks = evap_bin))
                     entropy(freq_tbl)
                   }, 
            by = .(precip_cat, kg_code)]

budyko_data[, relaive_entropy_precip_cat:= 
                   {
                     freq_tbl <-
                       table(cut(arid_index, breaks = arid_bin), 
                             cut(evap_index, breaks = evap_bin))
                     entropy(freq_tbl)
                   }, 
            by = .(precip_cat)]


head(budyko_data)

saveRDS(budyko_data, paste0(path_budyko_data, "05_2_budyko_joint_entropy_comb.rds"))


# define a data frame (entropy_data_frame) to save entropy values for each KG class
# colume_names = c("kg_code", 
#                  "kg_relative_entropy", 
#                  "kg_arid_entropy",
#                  "kg_evap_entropy")
# 
# 
# entropy_data_frame = data.table(matrix(nrow = length(med_kg_codes), ncol = length(colume_names))) 
# colnames(entropy_data_frame) = colume_names


# calculate the number of each point in the each bin as a table

# # calculate entropy of points on the Budyko space
# for(kg_ite in 1:length(med_kg_codes)){
#   
#   budyko_data_dummie <- budyko_data[kg_code == med_kg_codes[kg_ite], ]
#   
#   freq_tbl <-
#     table(cut(budyko_data_dummie[variable == "arid_index", value], breaks = arid_bin), 
#           cut(budyko_data_dummie[variable == "evap_index", value], breaks = evap_bin))
#   
#   freq_tbl_arid <-
#     table(cut(budyko_data_dummie[variable == "arid_index", value], breaks = arid_bin))
#   
#   freq_tbl_evap <-
#     table(cut(budyko_data_dummie[variable == "evap_index", value], breaks = evap_bin))
#   
#   # KG code
#   entropy_data_frame$kg_code[kg_ite] <- med_kg_codes[kg_ite]
#   
#   # joint entropy
#   entropy_data_frame$joint_entropy_kg[kg_ite] <- entropy(freq_tbl)
#   
#   # number of grid cells
#   entropy_data_frame$number_kg[kg_ite] <-
#     length(budyko_data[kg_code == med_kg_codes[kg_ite] & variable == "arid_index", kg_code])
#   
#   # entropy_data_frame$max_possible_entropy[kg_ite] <- log(length(arid_bin)* length(evap_bin))
#   # entropy_data_frame$max_entropy_kg[kg_ite] <- log(length(freq_tbl_evap) * length(freq_tbl_arid))
#   
#   # # mutual information
#   # # compute marginal entropies
#   # H1 = entropy(rowSums(freq_tbl))
#   # H2 = entropy(colSums(freq_tbl))
#   # # mutual entropy
#   # entropy_data_frame$mutual_information_kg[kg_ite]  <- H1 + H2- entropy_data_frame$joint_entropy[kg_ite]
#   
#   entropy_data_frame$arid_entropy_kg[kg_ite] <- entropy(freq_tbl_arid)
#   entropy_data_frame$evap_entropy_kg[kg_ite] <- entropy(freq_tbl_evap)
#   
#   
# }
# 
# entropy_data_frame 
# ggplot(entropy_data_frame) + geom_point(aes(x = number_kg, y = arid_entropy_kg))
# # normalize the values of entropy according to the number of points (grids) in each KG class
# # entropy_data_frame[, normalized_entropy:= entropy*number/sum(number)]
# 
# 
# budyko_data_arid <- merge(budyko_data, entropy_data_frame, by = 'kg_code')
# 
# budyko_data_arid[kg_code == 0,]
# 
# head(budyko_data_arid)
# tail(budyko_data)
# 
# 
# # saveRDS(budyko_data, paste0(path_budyko_data, "04_1_budyko_data_joint_entropy_KG.rds"))
# saveRDS(entropy_data_frame, paste0(path_budyko_data, "04_1_joint_entropy_KG.rds"))
# 
# 
# 
# 
# 
# # entropy of each combination
# 
# # extract the combination of data frames
# med_combination <- c(unique(budyko_data$combination))
# 
# # define a data frame (entropy_data_frame) to save entropy values for each combination
# colume_names = c("combination", 
#                  # "joint_entropy_comb", 
#                  'var_name_comb',
#                  "number_comb", 
#                  'entropy_value_comb'
#                  # "mutual_info_comb",
#                  # "arid_entropy_comb",
#                  # "evap_entropy_comb"
#                  )
# entropy_data_comb_frame = data.table(matrix(nrow = length(med_combination), ncol = length(colume_names))) 
# colnames(entropy_data_comb_frame) = colume_names
# 
# # the bins for all combinations are the same as the number of pixels is the same
# # arid_bin_comb <- seq(from = 0, to = ceiling(budyko_data[, max(arid_index)]), by = 1)
# # evap_bin_comb <- seq(from = 0, to = ceiling(budyko_data[, max(evap_index)]), by = 1)
# 
# # calculate entropy of points on the Budyko space
# for(comb_ite in 1:length(med_combination)){
#   
#   budyko_data_dummie <- budyko_data[combination == med_combination[comb_ite], ]
#   
#   # calculate the number of each point in the each bin as a table
#   # freq_tbl <-
#   #   table(cut(budyko_data_dummie[, arid_index], breaks = arid_bin), 
#   #         cut(budyko_data_dummie[, evap_index], breaks = evap_bin))
#   
#   if(startsWith(med_combination[comb_ite], 'pet')){
#     
#     freq_tbl_arid <-
#       table(cut(budyko_data_dummie[, value], breaks = arid_bin))
#     entropy_data_comb_frame$entropy_value_comb[comb_ite] <- entropy(freq_tbl_arid)
#     entropy_data_comb_frame$var_name_comb[comb_ite] <- 'arid_entropy'
#     
#   }else{
#     freq_tbl_evap <-
#       table(cut(budyko_data_dummie[, value], breaks = evap_bin))
#     entropy_data_comb_frame$entropy_value_comb[comb_ite] <- entropy(freq_tbl_evap)
#     entropy_data_comb_frame$var_name_comb[comb_ite] <- 'evap_entropy'
#   }
#   # joint entropy
#   # entropy_data_comb_frame$joint_entropy_comb[comb_ite] <- entropy(freq_tbl)
#   
#   entropy_data_comb_frame$number_comb[comb_ite] <-
#     length(budyko_data[combination == med_combination[comb_ite], value])
#   
#   entropy_data_comb_frame$combination[comb_ite] <- med_combination[comb_ite]
#   
#   # # mutual information
#   # # compute marginal entropies
#   # H1 = entropy(rowSums(freq_tbl))
#   # H2 = entropy(colSums(freq_tbl))
#   # # mutual entropy
#   # entropy_data_comb_frame$mutual_info_comb[comb_ite]  <- H1 + H2- entropy_data_comb_frame$entropy[comb_ite]
#   
#   
#   
# }
# 
# entropy_data_comb_frame 
# budyko_data_evap <- merge(budyko_data, entropy_data_comb_frame, by = 'combination')
# 
# head(budyko_data_evap)
# head(budyko_data_arid)
# 
# 
# budyko_final <- merge(budyko_data_evap, budyko_data_arid, by = c('x','y','variable', 'value', 'kg_code', 'combination'))
# 


# budyko_final[kg_code == 0,]

# head(budyko_final)
# tail(budyko_final)
# budyko_final[, c('x', 'y', 'kg_code', 'combination', 'variable', 'value',
#                  'var_name_comb', 'entropy_value_comb', 'number_comb',
#                  'joint_entropy_kg', 'evap_entropy_kg', 'number_kg', 'arid_entropy_kg')]

# saveRDS(budyko_final, paste0(path_budyko_data, "04_1_budyko_data_joint_entropy.rds"))
# saveRDS(entropy_data_comb_frame, paste0(path_budyko_data, "04_1_joint_entropy_comb.rds"))



# testt <-readRDS('~/shared/data_projects/med_datasets/2000_2019_data/budyko/04_1_budyko_data_joint_entropy.rds')
# 
# head(testt[x == -1.125 & y == 38.125 & combination == 'e_pet_gleam_p_chirps',])
# head(budyko_final[x == -1.125 & y == 38.125 & combination == 'e_gleam_p_chirps',])
# unique(budyko_final$combination)
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

