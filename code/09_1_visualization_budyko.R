library(psych)
# load global variables and packages
source("./code/source/global_variables.R")

# load functions
source('./code/source/functions.R')


budyko_data <- 
  readRDS( "~/shared/data_projects/med_datasets/2000_2019_data/budyko/05_2_budyko_joint_entropy_comb.rds")

evap_slope_data <- 
  readRDS( "~/shared/data_projects/med_datasets/2000_2019_data/budyko/08_evaporative_entropy_KG_slope.rds")

aridity_data <- 
  readRDS( "~/shared/data_projects/med_datasets/2000_2019_data/budyko/05_0_aridity_entropy_KG.rds")


kg_name <- c('BWh', 'BSh', 'BSk', 'Csa', 'Csb', 'Dsb', 'Cfb', 'BWk', 'Dfb',
             'Cfa', 'Dfc', 'ET', 'Dfa', 'Dsa', 'Dsc')

# add KG names to the datasets
budyko_data[,10] <- NULL

kg_code <- unique(budyko_data$kg_code)
kg_data_frame <- data.table(kg_code, kg_name)
budyko_data <- merge(budyko_data, kg_data_frame, by = 'kg_code')

# evap_slope_data <- merge(evap_slope_data, kg_data_frame, by = 'kg_code')

KG_class <- c("#FF0000", "#FF9696" , "#F5A500", "#FFDC64", 
                       "#FFFF00" , "#C8C800" , "#C8FF50", "#64FF50", "#FF00FF", 
                       "#C800C8", "#963296", "#00FFFF", "#37C8FF", "#007D7D","#B2B2B2" )

KG_class_names <- c('BWh'= "#FF0000", 'BWk' = "#FF9696" , 'BSh' = "#F5A500",
                    'BSk' = "#FFDC64", 'Csa' = "#FFFF00" , 'Csb' = "#C8C800" , 
                    'Cfa' = "#C8FF50", 'Cfb' = "#64FF50", 'Dsa' = "#FF00FF", 
                    'Dsb' =  "#C800C8", 'Dsc' =  "#963296", 'Dfa' = "#00FFFF", 'Dfb' = "#37C8FF", 
                    'Dfc' = "#007D7D",'ET' = "#B2B2B2" )

                       
arid_bin <- c(1, 2, 5, 20) #seq(from = 0, to = ceiling(budyko_data_dummie[, max(arid_index)]), by = 1)
evap_bin <- c(0.5, 1, 1.5, 2, 2.5, 3)

head(budyko_data)
# second min entropy
# min_relative_entropy_kg <- budyko_data[, sort(unique(relaive_entropy_comb))[1], by = kg_code]


# remove the combinations where aridity < evaporative
budyko_data[arid_index < evap_index, arid_index := NA]
remove_vec <- unique(budyko_data[is.na(arid_index), combination])
budyko_data_physical <- subset(budyko_data, !(combination %in% remove_vec))

# unique(budyko_data_physical$pet_category)

# budyko_data_physical <- budyko_data

min_relative_entropy_kg <- budyko_data_physical[evap_index <= 1, min(relaive_entropy_comb_kg), by = kg_code]
min_relative_entropy_kg <- merge(min_relative_entropy_kg, kg_data_frame, by = 'kg_code')
# unique(sort(budyko_data_physical[kg_code == 4, relaive_entropy_precip_cat_kg]))
# unique(budyko_data_physical[kg_code == 4, precip_cat_arid])



min_budyko <- data.table(matrix(ncol = ncol(budyko_data_physical), nrow =  0))
names(min_budyko) <- names(budyko_data_physical)

for (i in 1:length(min_relative_entropy_kg$kg_code)) {
  min_budyko_bummie <- budyko_data_physical[evap_index <= 1 & kg_code == min_relative_entropy_kg$kg_code[i] & 
                                              relaive_entropy_comb_kg == min_relative_entropy_kg$V1[i], ]  
  min_budyko <- rbind(min_budyko, min_budyko_bummie)
}


# sometimes the entropy of kg classes with small number of grid cells are equal to each other
# so they should be checked and choose the best combination
# kg 17, 19, 25 and 29 have this condition

unique(min_budyko$combination)
unique(min_budyko$kg_code)
unique(min_budyko$kg_name)
# 9  8 18 15  7 26  5 14  6 27 29  4 25 17 19
unique(min_budyko[kg_code == 17, combination])
# visual assessment 
# check for kg 29
unique(min_budyko[kg_code == 29, combination])
ggplot(data = min_budyko[kg_code == 29, ]) +
  geom_point(aes(x = arid_index, y = evap_index, color = factor(kg_code) ), size = 0.6) +
  scale_color_manual(values =  KG_class) + 
  geom_abline(intercept = 0, slope = 1) +
  geom_hline(yintercept = evap_bin, color = "#FFA559") +
  geom_vline(xintercept = arid_bin, color = "#FFA559") +
  ylim(c(0, 1)) + xlim(c(0, 1)) + labs(x = "Aridity Index [PET/P]", y = "Evaporative Index [E/P]", color = "KG_class") +
  geom_segment(aes(x = 1, xend = 20, y = 1, yend = 1)) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme_bw() + 
  facet_wrap(vars(combination))
# best  "p_em-earth_pet_terraclimate_e_terraclimate" 



# check for kg 25
unique(min_budyko[kg_code == 25, combination])
ggplot(data = min_budyko[kg_code == 25, ]) +
  geom_point(aes(x = arid_index, y = evap_index, color = factor(kg_code) ), size = 0.6) +
  scale_color_manual(values =  KG_class) + 
  geom_abline(intercept = 0, slope = 1) +
  geom_hline(yintercept = evap_bin, color = "#FFA559") +
  geom_vline(xintercept = arid_bin, color = "#FFA559") +
  ylim(c(0, 1)) + xlim(c(0, 2)) + labs(x = "Aridity Index [PET/P]", y = "Evaporative Index [E/P]", color = "KG_class") +
  geom_segment(aes(x = 1, xend = 20, y = 1, yend = 1)) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme_bw() + 
  facet_wrap(vars(combination))
# best  "p_gpcc_pet_terraclimate_e_terraclimate"


# check for kg 19
unique(min_budyko[kg_code == 19, combination])
ggplot(data = min_budyko[kg_code == 19, ]) +
  geom_point(aes(x = arid_index, y = evap_index, color = factor(kg_code) ), size = 0.6) +
  scale_color_manual(values =  KG_class) + 
  geom_abline(intercept = 0, slope = 1) +
  geom_hline(yintercept = evap_bin, color = "#FFA559") +
  geom_vline(xintercept = arid_bin, color = "#FFA559") +
  ylim(c(0, 2)) + xlim(c(0, 2)) + labs(x = "Aridity Index [PET/P]", y = "Evaporative Index [E/P]", color = "KG_class") +
  geom_segment(aes(x = 1, xend = 20, y = 1, yend = 1)) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme_bw() + 
  facet_wrap(vars(combination))
# best "p_terraclimate_pet_terraclimate_e_terraclimate"  



# check for kg 17
unique(min_budyko[kg_code == 17, combination])
ggplot(data = min_budyko[kg_code == 17, ]) +
  geom_point(aes(x = arid_index, y = evap_index, color = factor(kg_code) ), size = 0.6) +
  scale_color_manual(values =  KG_class) + 
  geom_abline(intercept = 0, slope = 1) +
  geom_hline(yintercept = evap_bin, color = "#FFA559") +
  geom_vline(xintercept = arid_bin, color = "#FFA559") +
  ylim(c(0, 2)) + xlim(c(0, 5)) + labs(x = "Aridity Index [PET/P]", y = "Evaporative Index [E/P]", color = "KG_class") +
  geom_segment(aes(x = 1, xend = 20, y = 1, yend = 1)) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme_bw() + 
  facet_wrap(vars(combination))
# best "pet_em-earth-mb_p_jra55_&_e_terraclimate_p_mswep" 


# min_budyko[kg]

# min_budyko <- subset(min_budyko, !(kg_code == 17 & combination != "pet_em-earth-mb_p_jra55_&_e_terraclimate_p_mswep"))
min_budyko <- subset(min_budyko, !(kg_code == 19 & combination != "p_terraclimate_pet_terraclimate_e_terraclimate"))
min_budyko <- subset(min_budyko, !(kg_code == 25 & combination != "p_gpcc_pet_terraclimate_e_terraclimate"))
min_budyko <- subset(min_budyko, !(kg_code == 29 & combination != "p_em-earth_pet_terraclimate_e_terraclimate"))


winner_combinations <- min_budyko[, unique(kg_code), by =combination]
names(winner_combinations) <- c('combination', 'kg_code')
winner_combinations <- merge(winner_combinations, kg_data_frame, by = 'kg_code')
winner_combinations <- winner_combinations[, c('kg_code', 'kg_name', 'combination')]


# ==============================================================================

ggplot(data = min_budyko) +
  geom_point(aes(x = arid_index, y = evap_index, color = kg_name), size = 0.8) +
  scale_color_manual(values =  KG_class_names) + 
  geom_abline(intercept = 0, slope = 1) +
  geom_hline(yintercept = evap_bin, color = "grey") +
  ylim(c(0, 2)) +  labs(x = "Aridity Index [PET/P]", y = "Evaporative Index [E/P]", color = "KG_class") +
  geom_segment(aes(x = 1, xend = 20, y = 1, yend = 1)) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  ggtitle('The combinations of the best performing P, PET and E datasets (the lowest relative entropy)') +
  geom_vline(aes(xintercept = 1), color = "#0047AC", size = 0.8) +
  geom_vline(aes(xintercept = 2), color = "#1497D4", size = 0.8) +
  geom_vline(aes(xintercept = 5), color = "#FFD301", size = 0.8) +
  geom_vline(aes(xintercept = 20), color = "#FFB921", size = 0.8) +
  scale_x_continuous(breaks = c(0, 1, 2, 5, 20), limits = c(0, 21)) +
  theme_bw() + 
  theme(
    plot.title = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15)
  ) 


# ggplot(data = slope_budyko_data_physical) +
#   geom_point(aes(x = arid_index, y = evap_index, color = factor(kg_code) ), size = 0.1) +
#   scale_color_manual(values =  KG_class) + 
#   geom_abline(intercept = 0, slope = 1) +
#   geom_hline(yintercept = evap_bin, color = "#FFA559") +
#   geom_vline(xintercept = arid_bin, color = "#FFA559") +
#   ylim(c(0, 3)) + xlim(c(0, 20)) + labs(x = "Aridity Index [PET/P]", y = "Evaporative Index [E/P]", color = "KG_class") +
#   geom_segment(aes(x = 1, xend = 20, y = 1, yend = 1)) +
#   guides(color = guide_legend(override.aes = list(size = 5))) +
#   facet_wrap(vars(combination)) +
#   theme_bw()

# maximum relative entropy
max_relative_entropy_kg <- budyko_data_physical[, max(relaive_entropy_comb_kg), by = kg_code]

# unique(sort(budyko_data_physical[kg_code == 4, relaive_entropy_precip_cat_kg]))
# unique(budyko_data_physical[kg_code == 4, precip_cat_arid])

max_budyko <- data.table(matrix(ncol = ncol(budyko_data_physical), nrow =  0))
names(max_budyko) <- names(budyko_data_physical)


for (i in 1:length(max_relative_entropy_kg$kg_code)) {
  max_budyko_bummie <- budyko_data_physical[kg_code == max_relative_entropy_kg$kg_code[i] & 
                                              relaive_entropy_comb_kg == max_relative_entropy_kg$V1[i], ]  
  max_budyko <- rbind(max_budyko, max_budyko_bummie)
}
unique(max_budyko$combination)
unique(max_budyko$kg_code)



ggplot(data = max_budyko) +
  geom_point(aes(x = arid_index, y = evap_index, color = kg_name ), size = 0.8) +
  scale_color_manual(values =  KG_class_names) + 
  geom_abline(intercept = 0, slope = 1) +
  geom_hline(yintercept = evap_bin, color = "grey") +
  ylim(c(0, 4)) + labs(x = "Aridity Index [PET/P]", y = "Evaporative Index [E/P]", color = "KG_class") +
  geom_segment(aes(x = 1, xend = 20, y = 1, yend = 1)) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  ggtitle('The combinations of P, PET and E with the highest relative entopy') +
  geom_vline(aes(xintercept = 1), color = "#0047AC", size = 0.8) +
  geom_vline(aes(xintercept = 2), color = "#1497D4", size = 0.8) +
  geom_vline(aes(xintercept = 5), color = "#FFD301", size = 0.8) +
  geom_vline(aes(xintercept = 20), color = "#FFB921", size = 0.8) +
  scale_x_continuous(breaks = c(0, 1, 2, 5, 20), limits = c(0, 21)) +
  theme_bw() + 
  theme(
    plot.title = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15)
    
  )


loser_combinations <- max_budyko[, unique(kg_code), by =combination]
names(loser_combinations) <- c('combination', 'kg_code')
loser_combinations <- merge(loser_combinations, kg_data_frame, by = 'kg_code')


# see the precip categories
ggplot(data = budyko_data_physical) +
  geom_point(aes(x = arid_index, y = evap_index, color = kg_name), size = 0.2) +
  scale_color_manual(values =  KG_class_names) + 
  geom_abline(intercept = 0, slope = 1) +
  ylim(c(0, 4)) + labs(x = "Aridity Index [PET/P]", y = "Evaporative Index [E/P]", color = "KG_class") +
  geom_segment(aes(x = 1, xend = 20, y = 1, yend = 1)) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  geom_vline(aes(xintercept = 1), color = "#0047AC", size = 0.8) +
  geom_vline(aes(xintercept = 2), color = "#1497D4", size = 0.8) +
  geom_vline(aes(xintercept = 5), color = "#FFD301", size = 0.8) +
  geom_vline(aes(xintercept = 20), color = "#FFB921", size = 0.8) +
  scale_x_continuous(breaks = c(0, 1, 2, 5, 20), limits = c(0, 21)) +
  theme_bw() + 
  ggtitle('The performance of precipiation datasets on budyko space') +
  facet_wrap(vars(precip_category))


# budyko_data_physical[, p_pet_cat := paste0('P_arid_', precip_cat_arid, '_P_evpo_', precip_cat_evap, '_PET_',pet_category)]

# ggplot(data = budyko_data_physical) +
#   geom_point(aes(x = arid_index, y = evap_index, color = factor(kg_code) ), size = 0.6) +
#   scale_color_manual(values =  KG_class) + 
#   geom_abline(intercept = 0, slope = 1) +
#   geom_hline(yintercept = evap_bin, color = "#FFA559") +
#   geom_vline(xintercept = arid_bin, color = "#FFA559") +
#   ylim(c(0, 4)) + xlim(c(0, 30)) + labs(x = "Aridity Index [PET/P]", y = "Evaporative Index [E/P]", color = "KG_class") +
#   geom_segment(aes(x = 1, xend = 30, y = 1, yend = 1)) +
#   guides(color = guide_legend(override.aes = list(size = 5))) +
#   theme_bw() + 
#   ggtitle('The performance of P and PET datasets on budyko space') +
#   facet_wrap(vars(pet_category))

# unique(budyko_data_physical[p_pet_cat == "P_arid_satellite_based_P_evpo_observational_PET_temperature_based", combination])

# budyko_data[, p_pet_cat := paste0('P_arid_', precip_cat_arid, '_P_evpo_', precip_cat_evap, '_PET_',pet_category)]

ggplot(data = budyko_data_physical) +
  geom_point(aes(x = arid_index, y = evap_index, color = kg_name), size = 0.4) +
  scale_color_manual(values =  KG_class_names) + 
  geom_abline(intercept = 0, slope = 1) +
  # geom_hline(yintercept = evap_bin, color = "#FFA559", linetype = 'dash', size) +
  # geom_vline(xintercept = arid_bin, color = "#FFA559") +
  ylim(c(0, 4)) + labs(x = "Aridity Index [PET/P]", y = "Evaporative Index [E/P]", color = "KG_class") +
  geom_segment(aes(x = 1, xend = 21, y = 1, yend = 1)) +
  geom_vline(aes(xintercept = 1), color = "#0047AC", size = 0.8) +
  geom_vline(aes(xintercept = 2), color = "#1497D4", size = 0.8) +
  geom_vline(aes(xintercept = 5), color = "#FFD301", size = 0.8) +
  geom_vline(aes(xintercept = 20), color = "#FFB921", size = 0.8) +
  scale_x_continuous(breaks = c(0, 1, 2, 5, 20), limits = c(0, 21)) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme_bw() + 
  theme(
    strip.text = element_text(size = 8, face = 'bold'),
    strip.background = element_rect(fill = NA)
  ) +
  ggtitle('The performance of P, PET and E datasets on the Budyko space') +
  facet_wrap(vars(combination), ncol = 5)




ggplot(data = budyko_data) +
  geom_point(aes(x = arid_index, y = evap_index, color = kg_name), size = 0.6) +
  scale_color_manual(values =  KG_class_names) + 
  geom_abline(intercept = 0, slope = 1) +
  # geom_hline(yintercept = evap_bin, color = "#FFA559", linetype = 'dash', size) +
  # geom_vline(xintercept = arid_bin, color = "#FFA559") +
  ylim(c(0, 4)) + labs(x = "Aridity Index [PET/P]", y = "Evaporative Index [E/P]", color = "KG_class") +
  geom_segment(aes(x = 1, xend = 21, y = 1, yend = 1)) +
  geom_vline(aes(xintercept = 1), color = "#0047AC", size = 0.8) +
  geom_vline(aes(xintercept = 2), color = "#1497D4", size = 0.8) +
  geom_vline(aes(xintercept = 5), color = "#FFD301", size = 0.8) +
  geom_vline(aes(xintercept = 20), color = "#FFB921", size = 0.8) +
  scale_x_continuous(breaks = c(0, 1, 2, 5, 20), limits = c(0, 21)) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme_bw() + 
  theme(
    strip.text = element_text(size = 6, face = 'bold'),
    strip.background = element_rect(fill = NA)
  ) +
  ggtitle('The performance of P, PET and E datasets on the Budyko space') +
  facet_wrap(vars(combination))

