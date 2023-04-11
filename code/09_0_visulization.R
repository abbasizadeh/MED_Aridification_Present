library(psych)
# load global variables and packages
source("./code/source/global_variables.R")

# load functions
source('./code/source/functions.R')


# budyko_data <- 
#   readRDS( "~/shared/data_projects/med_datasets/2000_2019_data/budyko/05_2_budyko_joint_entropy_comb.rds")

evap_slope_data <- 
  readRDS( "~/shared/data_projects/med_datasets/2000_2019_data/budyko/08_evaporative_entropy_KG_slope.rds")

aridity_data <- 
  readRDS( "~/shared/data_projects/med_datasets/2000_2019_data/budyko/05_0_aridity_entropy_KG.rds")


kg_name <- c('BWh', 'BSh', 'BSk', 'Csa', 'Csb', 'Dsb', 'Cfb', 'BWk', 'Dfb',
             'Cfa', 'Dfc', 'ET', 'Dfa', 'Dsa', 'Dsc')

# add KG names to the datasets
kg_code <- unique(aridity_data$kg_code)
kg_data_frame <- data.table(kg_code, kg_name)
aridity_data <- merge(aridity_data, kg_data_frame, by = 'kg_code')
evap_slope_data <- merge(evap_slope_data, kg_data_frame, by = 'kg_code')



# ==============================================================================
plot_slope_kg <- ggplot(data = evap_slope_data, aes(x = slope_p_minus_e, y = factor(kg_name))) +
  geom_boxplot(outlier.shape = 19, outlier.size = 0.5) +
  stat_summary(fun.y=mean, geom="point", size=2, color="red") +
  xlab("slope in P-E [mm/month]") +
  ylab("Koppen Geiger class") +
  geom_vline(xintercept = 0, color = "red") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 20),
    axis.line   = element_blank(),
    axis.text = element_text(size = 20),
    # panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()) 
plot_slope_kg
# calculate normalized entropy for aridity index
arid_bin <- c(0, 1, 2, 5, 20, ceiling(max(aridity_data[,arid_index]))) 
bin_number <- length(arid_bin) - 1
p2_data <- data.table(kg_code = unique(aridity_data$kg_name), entropy_kg = unique(aridity_data$entropy_kg))
kg_number <- aridity_data[arid_comb == 'pet_terraclimate_p_merra2', length(arid_index), by = kg_code]
p2_data[, number :=  kg_number$V1][, bin_size := bin_number]
p2_data[, .(number, bin_size) ]
p2_data[,max_entropy_kg := NA]
for (i in 1:length(p2_data$number)) {
    p2_data$max_entropy_kg[i] <- max_entropy(sample_size = p2_data$number[i], bin_size = 5)}
p2_data[, normalized_entropy := entropy_kg/max_entropy_kg]


# length(aridity_data[arid_comb == 'pet_em-earth-hs_p_chirps' & kg_code == 25, arid_index])
# entropy of data
plot_normalized_entrpy_kg <- ggplot() + 
  geom_col(data = p2_data, aes(y = factor(kg_code), x = normalized_entropy), fill = 'white', color = 'black') +
  scale_x_reverse() +  
  scale_y_discrete(position = "right") +
  xlab("normalized enropy of PET/P") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 20),
    axis.title.y = element_blank(),
    axis.text = element_text(size=20),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
    # panel.border = element_blank(),
  ) 

plot_normalized_entrpy_kg 
plot_grid(plot_slope_kg, plot_normalized_entrpy_kg, ncol = 2, align = "h", rel_widths = c(3, 1)) 



# ==============================================================================
# visualize evaporative entropy vs slope kg
# calculate normalized entropy for evap index
evap_bin <- c(seq(from = 0, 
                  to = (ceiling(max(evap_slope_data[,evap_index])) + 
                          ceiling(max(evap_slope_data[,evap_index]))%%5), by = 0.5))

bin_number <- length(evap_bin) - 1
p3_data <- data.table(kg_code = unique(evap_slope_data$kg_code), entropy_kg = unique(evap_slope_data$entropy_kg))
kg_number <- evap_slope_data[evap_comb == 'e_terraclimate_p_merra2', length(evap_index), by = kg_code]
p3_data[, number :=  kg_number$V1][, bin_size := bin_number]
p3_data[, .(number, bin_size) ]
p3_data[,max_entropy_kg := NA]
for (i in 1:length(p3_data$number)) {
  p3_data$max_entropy_kg[i] <- max_entropy(sample_size = p3_data$number[i], bin_size = 64)
  }
p3_data[, normalized_entropy := entropy_kg/max_entropy_kg]


plot_normalized_entrpy_evap_kg <- 
  ggplot(data = p3_data, aes(y = factor(kg_code), x = normalized_entropy)) +
  geom_col(fill = 'white', color = 'black') +
  scale_x_reverse() +  
  scale_y_discrete(position = "right") +
  xlab("normalized enropy of E/P") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 20),
    axis.title.y = element_blank(),
    axis.text = element_text(size=20),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),

  )

plot_normalized_entrpy_evap_kg

plot_grid(plot_slope_kg, plot_normalized_entrpy_evap_kg, ncol = 2, align = "h", 
          rel_widths = c(3, 1)) 



# ==============================================================================
# visualize the categories of precip datasets


plot_precip_category <- 
  ggplot(data = evap_slope_data, aes(x = precip_cat_evap, y = slope_p_minus_e, fill = precip_cat_evap)) +
  geom_violin() + 
  geom_boxplot(width=0.1, fill = 'white', outlier.shape = 19, outlier.size = 0.5) +
  stat_summary(fun.y=mean, geom="point", size=2, color="red") +
  xlab("category of precipitation datasets") +
  ylab("slope in P-E [mm/month]") +
  ylim(c(-0.07, 0.07)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_hline(yintercept = -0.07, color = "black") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 15),
    axis.line = element_blank(),
    # panel.border = element_blank(),
    axis.text = element_text(size = 15),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = 'none',#'bottom',
    legend.title = element_blank(),
    
  )

plot_precip_category

# bin_number <- length(evap_bin) - 1
p4_data <- data.table(category = unique(evap_slope_data$precip_cat_evap), 
                      entropy_kg = unique(evap_slope_data$entropy_precip_cat_evap))
cat_number <- evap_slope_data[, length(evap_index), by = precip_cat_evap]
bin_number <- length(evap_bin) - 1
p4_data[, number :=  cat_number$V1][, bin_size := bin_number]
p4_data[,max_entropy_kg := NA]
for (i in 1:length(p4_data$number)) {
  p4_data$max_entropy_kg[i] <- max_entropy(sample_size = p4_data$number[i], bin_size = 64)
}
p4_data[, normalized_entropy := entropy_kg/max_entropy_kg]


# plot_entrpy_precip_category_data <- 
#   data.table(kg_code = unique(evap_slope_data$kg_code), entropy_kg = unique(evap_slope_data$entropy_kg))

plot_entrpy_precip_category <- 
  ggplot() +
  geom_col(data = p4_data, aes(x = factor(category), y = normalized_entropy, fill = category), 
           color = 'black', width = 0.4) +
  scale_y_reverse() +
  scale_x_discrete(position = "top") +
  ylab("normalized enropy of E/P") +
  geom_hline(aes(yintercept = 0), color = "black") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 15),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 15),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    # axis.line.y = element_blank(),
    # panel.border = element_blank(),
    # axis.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = 'none'
  ) 

plot_entrpy_precip_category


plot_grid(plot_entrpy_precip_category, plot_precip_category,  nrow = 2, 
          axis = "l", align = "v", rel_heights = c(1, 2.5)) 



descriptive_evap <- evap_slope_data[, describe(evap_index), by = precip_cat_evap]
descriptive_slope <- evap_slope_data[, describe(slope_p_minus_e), by = precip_cat_evap]



#=============================================================================== 
# aridity dataset categories for the whole med

p4_data <- data.table(entropy_comb = unique(aridity_data$entropy_cat_comb), 
                      cat_comb = unique(aridity_data$cat_comb))


plot_aridity <-  ggplot(data = aridity_data, aes(x = cat_comb, y = arid_index, fill = cat_comb)) +
  geom_violin() + 
  geom_boxplot(width=0.1, outlier.shape = 19, outlier.size = 0.5,fill = "white") +
  labs(y = 'Aridity index PET/P', x ="Dataset categories") +
  stat_summary(fun.y=mean, geom="point", size=2, color="red") +
  # ylim(c(0, 20)) + #xlab('aridity index PET/P') +
  geom_hline(aes(yintercept = 0), color = "black") +
  geom_hline(aes(yintercept = 1), color = "#0047AC") +
  geom_hline(aes(yintercept = 2), color = "#1497D4") +
  geom_hline(aes(yintercept = 5), color = "#FFD301") +
  geom_hline(aes(yintercept = 20), color = "#FFB921") +
  scale_y_continuous(breaks = c(0, 1, 2, 5, 20), limits = c(0, 21)) +
  # annotate("text", x=1, y=0, label="Some text", angle=90)
  theme_bw() +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size= 15),
    # axis.ticks.y = element_blank(),
    # axis.line  = element_blank(),
    # panel.border = element_blank()
    # legend.position = c(0.75, 0.5),
    legend.title = element_blank(),
    axis.text.x = element_text(size = 10, face = 'bold'),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = 'none'#c(0.8, 0.2)
  )

  # guides(shape = guide_legend(override.aes = list(size = 0.5)))
plot_aridity 

# plot_aridity_entropy <-  ggplot(data = p4_data, aes(x = entropy_comb, y = cat_comb, fill = cat_comb)) + 
#     geom_bar(stat="identity") +
#     scale_x_reverse() +
#   # xlim(c(2,0)) +
#     # scale_x_discrete(position = "top") +
#     xlab("enropy of PET/P") +
#     theme(
#       # axis.title.x = element_blank(),
#       axis.title.y = element_blank(),
#       axis.text.y = element_blank(),
#       axis.ticks.y = element_blank(),
#       axis.line   = element_blank(),
#       # panel.border = element_blank(),
#       # axis.text = element_text(size = 10),
#       panel.grid.major.x = element_blank(),
#       panel.grid.minor.x = element_blank(),
#       legend.position = "none"
#     ) 
#   
# # plot_aridity_entropy
# 
# plot_grid(plot_aridity, plot_aridity_entropy,  nrow = 1, 
#           axis = "l", align = "h", rel_widths = c(2, 1)) 







#=============================================================================== 
# aridity dataset categories for each KG

p5_data <- aridity_data[, .(entropy_cat_comb_kg, cat_comb), by = kg_name]
p5_data[cat_comb == "combinational_satellite_based" & kg_name == 'Dsc',  
        entropy_cat_comb_kg := entropy_cat_comb_kg + 0.0000001]

unique(p5_data$entropy_cat_comb_kg)
unique(p5_data$kg_name)
# length(unique(p5_data[kg_name == 'Dsc', entropy_cat_comb_kg]))
# unique(p5_data[kg_name == 'Dsc' , entropy_cat_comb_kg, by =  cat_comb])

unique(p5_data$cat_comb)


p5_data <- p5_data[, .(unique(cat_comb), unique(entropy_cat_comb_kg)), by = kg_name]
# test <- p5_data[, unique(entropy_cat_comb_kg), by = kg_name]

names(p5_data) <- c('kg_name', 'cat_comb', 'entropy_cat_comb_kg')

# p4_data <- data.table(entropy_comb = unique(aridity_data$entropy_cat_comb), cat_comb = unique(aridity_data$cat_comb))
name_kg <- unique(p5_data$kg_name)

plot_list <- list()

font_size <- 5
for (i in 1:length(name_kg)) {plot_aridity_kg <-  ggplot(data = aridity_data[kg_name == name_kg[i],]) +
    geom_boxplot(aes(fill = cat_comb, x = arid_index),  outlier.shape = 19, outlier.size = 0.5) +
    # xlim(c(0, 20)) + #xlab('aridity index PET/P') +
    xlab('aridity index PET/P') +
    # labs(x = 'aridity index PET/P', fill ="dataset categories") +
  geom_vline(aes(xintercept = 0), color = "black", size = 2) +
  geom_vline(aes(xintercept = 1), color = "#0047AC", size = 2) +
  geom_vline(aes(xintercept = 2), color = "#1497D4", size = 2) +
  geom_vline(aes(xintercept = 5), color = "#FFD301", size = 2) +
  geom_vline(aes(xintercept = 20), color = "#FFB921", size = 2) +
  scale_x_continuous(breaks = c(0, 1, 2, 5, 20), limits = c(0, 21)) +
    # guides(color = guide_legend(override.aes = list(size = 5))) +
  theme_bw() +
    theme(
      plot.title = element_text(size=30, face = 'bold'),
      axis.title = element_text(size = 15, face = 'bold'),
      axis.text.x = element_text(size = 15),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line  = element_blank(),
      # panel.border = element_blank()
      # legend.position = c(0.75, 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = 'none'# c(0.9, 0.1)
    ) + ggtitle(name_kg[i]) 
    
  plot_aridity_kg
  # guides(shape = guide_legend(override.aes = list(size = 0.5)))
  
  plot_aridity_entropy_cat_kg <-  
    ggplot(data = p5_data[kg_name == name_kg[i],], 
                                         aes(x = entropy_cat_comb_kg, y = cat_comb, fill = cat_comb)) + 
    geom_bar(stat="identity") + 
    scale_x_reverse() +
    # xlim(c(2,0)) +
    # scale_x_discrete(position = "top") +
    xlab("enropy of PET/P") +
    theme_bw() +
    theme(
      axis.title = element_text(size = 15, face = 'bold'),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 15),
      axis.text.y = element_blank(),
      axis.line   = element_blank(),
      # panel.border = element_blank(),
      # axis.text = element_text(size = 10),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = "none"
      ) +
    annotate("text", x = 0.5, y = 6, label = 'atop(bold("p: satellite-based; pet: temperature-based"))', parse = TRUE, size = font_size) +
    annotate("text", x = 0.5, y = 5, label = 'atop(bold("p: satellite-based; pet: combinational"))', parse = TRUE, size = font_size) +
    annotate("text", x = 0.5, y = 4, label = 'atop(bold("p: reanalysis; pet: temperature-based"))', parse = TRUE, size = font_size) +
    annotate("text", x = 0.5, y = 3, label = 'atop(bold("p: reanalysis; pet: combinational"))', parse = TRUE, size = font_size) +
    annotate("text", x = 0.5, y = 2, label = 'atop(bold("p: observational; pet: temperature-based"))', parse = TRUE, size = font_size) +
    annotate("text", x = 0.5, y = 1, label = 'atop(bold("p: observational; pet: combinational"))', parse = TRUE, size = font_size) 
    
  
  
  # plot_aridity_entropy_cat_kg
  
  # plot_aridity_entropy
  # plot_list[[i]] <- plot_grid(plot_aridity_kg, plot_aridity_entropy_cat_kg,  nrow = 1, 
  #           axis = "l", align = "h", rel_widths = c(2, 1)) 
  assign(name_kg[i], plot_grid(plot_aridity_kg, plot_aridity_entropy_cat_kg,  nrow = 1, 
                               axis = "l", align = "h", rel_widths = c(2, 1)) )
  }

plot_grid(BSh, BSk, nrow = 1, ncol = 2, 
          axis = "l", align = "vh", rel_widths = c(1, 1))

name_kg

BWh
BWk
BSh
BSk
Csa
Csb
Cfa
Cfb
Dsa
Dsb
Dsc
Dfa
Dfb
Dfc
ET
#=============================================================================== 













# p3 <- ggplot() +
#   geom_col(data = budyko_entopy_KG, aes(y = factor(kg_code), x = mutual_information_kg)) +
#   scale_x_reverse() +
#   scale_y_discrete(position = "right") +
#   xlab("Mutual Infromation of PET/P and E/P") +
#   theme(
#     axis.title.y = element_blank(),
#     # axis.text.x = element_blank(),
#     # axis.ticks.x = element_blank(),
#     axis.line   = element_blank(),
#     panel.border = element_blank(),
#     axis.text = element_text(size = 10),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#   )
# 
# 
# plot_grid(p1, p3, ncol = 2, align = "h", rel_widths = c(3, 1))
# 
# 
# p4 <- ggplot() +
#   geom_col(data = budyko_entopy_KG, aes(y = factor(kg_code), x = arid_entrpy_kg)) +
#   scale_x_reverse() +
#   scale_y_discrete(position = "right") +
#   xlab("Entropy of PET/P") +
#   theme(
#     axis.title.y = element_blank(),
#     # axis.text.x = element_blank(),
#     # axis.ticks.x = element_blank(),
#     axis.line   = element_blank(),
#     panel.border = element_blank(),
#     axis.text = element_text(size = 10),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#   )
# 
# plot_grid(p1, p4, ncol = 2, align = "h", rel_widths = c(3, 1))
# 
# p5 <- ggplot() +
#   geom_col(data = budyko_entopy_KG, aes(y = factor(kg_code), x = evap_entropy_kg)) +
#   scale_x_reverse() +
#   scale_y_discrete(position = "right") +
#   xlab("Entropy of E/P") +
#   theme(
#     axis.title.y = element_blank(),
#     # axis.text.x = element_blank(),
#     # axis.ticks.x = element_blank(),
#     axis.line   = element_blank(),
#     panel.border = element_blank(),
#     axis.text = element_text(size = 10),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#   )
# 
# plot_grid(p1, p5, ncol = 2, align = "h", rel_widths = c(3, 1))
# 
# 
# budyko_entopy_KG_ment <- melt(budyko_entopy_KG, id.vars = c('kg_code', 'number_kg'))
# 
# ggplot(data = budyko_entopy_KG_ment, aes(x = factor(kg_code), value)) +
#   geom_bar(aes(fill = variable), position = "dodge", stat = "identity") 
# # +
# #   scale_fill_manual(values = c("#FF0000", "#C800C8" , "#F5A500", "#37C8FF"))
# 
# 
# 
# 
# 
# # for each combination
# p1 <- ggplot(data = slope_budyko_data) + 
#   geom_boxplot(aes(x = slope, y = combination)) + 
#   xlab("slope in P-E [mm/month]") +
#   ylab("Combination of datasets") +
#   geom_vline(xintercept = 0, color = "red") +
#   theme(
#     # axis.title.x = element_blank(),
#     # axis.text.x = element_blank(),
#     # axis.ticks.x = element_blank(),
#     axis.line   = element_blank(),
#     panel.border = element_blank(),
#     axis.text = element_text(size = 10),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank()
#   )
# 
# 
# p2 <- ggplot() +
#   geom_col(data = budyko_entopy_comb, aes(y = combination, x = joint_entropy_comb)) +
#   scale_x_reverse() +  
#   scale_y_discrete(position = "right") +
#   xlab("Joint enropy of PET/P and E/P") +
#   theme(
#     axis.title.y = element_blank(),
#     # axis.text.x = element_blank(),
#     # axis.ticks.x = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.line   = element_blank(),
#     panel.border = element_blank(),
#     axis.text = element_text(size = 10),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#   )
# 
# plot_grid(p1, p2, ncol = 2, align = "h", rel_widths = c(3, 1)) 
# 
# 
# # p3 <- ggplot() +
# #   geom_col(data = budyko_entopy_comb, aes(y = combination, x = mutual_info_comb)) +
# #   scale_x_reverse() +
# #   scale_y_discrete(position = "right") +
# #   xlab("Mutual information") +
# #   theme(
# #     axis.title.y = element_blank(),
# #     # axis.text.x = element_blank(),
# #     # axis.ticks.x = element_blank(),
# #     axis.text.y = element_blank(),
# #     axis.ticks.y = element_blank(),
# #     axis.line   = element_blank(),
# #     panel.border = element_blank(),
# #     axis.text = element_text(size = 10),
# #     panel.grid.major.x = element_blank(),
# #     panel.grid.minor.x = element_blank(),
# #   )
# # 
# # plot_grid(p1, p3, ncol = 2, align = "h", rel_widths = c(3, 1))
# 
# 
# 
# p4 <- ggplot() +
#   geom_col(data = budyko_entopy_comb, aes(y = factor(combination), x = arid_entropy_comb)) +
#   scale_x_reverse() +
#   scale_y_discrete(position = "right") +
#   xlab("Entropy of PET/P") +
#   theme(
#     axis.title.y = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.line   = element_blank(),
#     panel.border = element_blank(),
#     axis.text = element_text(size = 10),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#   )
# 
# plot_grid(p1, p4, ncol = 2, align = "h", rel_widths = c(3, 1))
# 
# p5 <- ggplot() +
#   geom_col(data = budyko_entopy_comb, aes(y = factor(combination), x = evap_entropy_comb)) +
#   scale_x_reverse() +
#   scale_y_discrete(position = "right") +
#   xlab("Entropy of E/P") +
#   theme(
#     axis.title.y = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.line   = element_blank(),
#     panel.border = element_blank(),
#     axis.text = element_text(size = 10),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#   )
# 
# plot_grid(p1, p5, ncol = 2, align = "h", rel_widths = c(3, 1))
# 
# 
# budyko_entopy_comb_melt <- melt(budyko_entopy_comb, id.vars = c('combination', 'number_comb'))
# 
# ggplot(data = budyko_entopy_KG_ment, aes(x = factor(kg_code), value)) +
#   geom_bar(aes(fill = variable), position = "dodge", stat = "identity") 
# # +
# #   scale_fill_manual(values = c("#FF0000", "#C800C8" , "#F5A500", "#37C8FF"))
# 
# 









## Figures
to_plot_sf <- slope_budyko_data[, .(x, y, evap_index)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25), crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% 
  st_as_sf()

entropy_med <- ggplot(to_plot_sf) +
  geom_sf(aes(color = entropy_kg, fill = entropy_kg)) +
  scale_color_gradient2(low =  '#06FF00', mid = '#FFE400', high = '#FF1700') +
  scale_fill_gradient2(low =  '#06FF00', mid = '#FFE400', high = '#FF1700') +
  coord_sf(expand = FALSE, crs = "+proj=robin") +
  scale_x_continuous(breaks = seq(-20, 30, 10)) +
  scale_y_continuous(breaks = seq(30, 70, 10)) +
  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major = element_line(colour = "gray60"),
        axis.title = element_text(size = 16), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 8))

entropy_med



test <- slope_budyko_data[kg_code == 15 ,]
min_evap_ent_comb <- min(test$evap_entropy_comb)
test2 <- test[evap_entropy_comb == min_evap_ent_comb, ]
head(test2)

unique(test2$combination)







