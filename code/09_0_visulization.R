library("entropy")

# load global variables and packages
source("./code/source/global_variables.R")

# load functions
source('./code/source/functions.R')


# load data
evap_slope_data <- 
  readRDS( "~/shared/data_projects/med_datasets/2000_2019_data/budyko/08_evaporative_entropy_KG_slope.rds")

aridity_data <- 
  readRDS( "~/shared/data_projects/med_datasets/2000_2019_data/budyko/05_0_aridity_entropy_KG.rds")


# kg names
kg_name <- c('BWh', 'BSh', 'BSk', 'Csa', 'Csb', 'Dsb', 'Cfb', 'BWk', 'Dfb',
             'Cfa', 'Dfc', 'ET', 'Dfa', 'Dsa', 'Dsc')

# kg names and class
KG_class_names <- c('BWh'= "#FF0000", 'BWk' = "#FF9696" , 'BSh' = "#F5A500",
                    'BSk' = "#FFDC64", 'Csa' = "#FFFF00" , 'Csb' = "#C8C800" , 
                    'Cfa' = "#C8FF50", 'Cfb' = "#64FF50", 'Dsa' = "#FF00FF", 
                    'Dsb' =  "#C800C8", 'Dsc' =  "#963296", 'Dfa' = "#00FFFF", 'Dfb' = "#37C8FF", 
                    'Dfc' = "#007D7D",'ET' = "#B2B2B2" )


# add KG names to the datasets
kg_code <- unique(aridity_data$kg_code)
kg_data_frame <- data.table(kg_code, kg_name)
aridity_data <- merge(aridity_data, kg_data_frame, by = 'kg_code')
evap_slope_data <- merge(evap_slope_data, kg_data_frame, by = 'kg_code')



# calculate normalized entropy for aridity and evaporative indices
arid_bin <-
  c(0, 1, 2, 5, 20, ceiling(max(aridity_data[, arid_index])))

evap_bin <- c(seq(
  from = 0,
  to = (ceiling(max(evap_slope_data[, evap_index])) +
          ceiling(max(evap_slope_data[, evap_index])) %%
          5),
  by = 0.5
))

bin_number_arid <- length(arid_bin) - 1
bin_number_evap <- length(evap_bin) - 1

dummie_arid <- data.table(
  kg_name = unique(aridity_data$kg_name),
  entropy_kg_arid = unique(aridity_data$entropy_kg)
)

kg_number <- aridity_data[arid_comb == 'pet_terraclimate_p_merra2',
                          length(arid_index), by = kg_name]
dummie_arid[, number_arid :=  kg_number$V1][, bin_size_arid := bin_number_arid]
dummie_arid[, .(number_arid, bin_size_arid)]
dummie_arid[, max_entropy_kg_arid := NA]

for (i in 1:length(dummie_arid$number)) {
  dummie_arid$max_entropy_kg_arid[i] <-
    max_entropy(sample_size = dummie_arid$number_arid[i], bin_size = 5)
}

dummie_arid[, normalized_entropy_arid := entropy_kg_arid/max_entropy_kg_arid]

# evaporative index
dummie_evap <- data.table(kg_name = unique(evap_slope_data$kg_name), 
                          entropy_kg_evap = unique(evap_slope_data$entropy_kg))

kg_number_evap <- evap_slope_data[evap_comb == 'e_terraclimate_p_merra2', length(evap_index), by = kg_name]
dummie_evap[, number_evap :=  kg_number_evap$V1][, bin_size_evap := bin_number_evap]
dummie_evap[, .(number_evap, bin_size_evap) ]
dummie_evap[,max_entropy_kg_evap := NA]

for (i in 1:length(dummie_evap$number_evap)) {
  dummie_evap$max_entropy_kg_evap[i] <- max_entropy(sample_size = dummie_evap$number_evap[i], bin_size = 64)
}

dummie_evap[, normalized_entropy_evap := entropy_kg_evap/max_entropy_kg_evap]

# the value of entropies for each kg
arid_evap_entropy_data <- merge(dummie_arid, dummie_evap, by = 'kg_name')
rm(dummie_arid, dummie_evap)


# plot  
plot_slope_kg <- ggplot(data = evap_slope_data, aes(x = slope_p_minus_e, y = kg_name, fill = kg_name)) +
  geom_boxplot(outlier.shape = NA) + # outlier.shape = 19, outlier.size = 0.5
  stat_summary(fun.y=mean, geom="point", size=2, color="black") +
  scale_fill_manual(values =  KG_class_names) +
  xlab("Slope of P-E [mm/month]") +
  ylab("Koppen Geiger Class") +
  xlim(c(-0.15, 0.2)) + 
  geom_vline(xintercept = 0, color = "red") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 20),
    axis.line   = element_blank(),
    axis.text = element_text(size = 20),
    # panel.border = element_blank(),
    legend.position = 'none',
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()) 

plot_slope_kg


# entropy of data
plot_entrpy_arid <- 
  ggplot() +
  geom_col(
    data = arid_evap_entropy_data,
    aes(y = kg_name, x = normalized_entropy_arid, fill = kg_name, ),
    color = 'black'
  ) +
  scale_fill_manual(values =  KG_class_names) +
  scale_x_reverse() +
  scale_y_discrete(position = "right") +
  xlab("Normalized Entropy of PET/P") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 20),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 20),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = 'none'
    # panel.border = element_blank(),
  ) 

plot_entrpy_arid
plot_grid(plot_slope_kg, plot_entrpy_arid, ncol = 2, align = "h", rel_widths = c(3, 1)) 

# visualize evaporative entropy vs slope kg
plot_entrpy_evap <-
  ggplot() +
  geom_col(
    data = arid_evap_entropy_data,
    aes(y = kg_name, x = normalized_entropy_evap, fill = kg_name),
    color = 'black'
  ) +
  scale_fill_manual(values =  KG_class_names) +
  scale_x_reverse() +
  scale_y_discrete(position = "right") +
  xlab("Normalized Entropy of E/P") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 20),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 20),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = 'none'
  )

plot_entrpy_evap

plot_grid(plot_slope_kg, plot_entrpy_evap, ncol = 2, align = "h", 
          rel_widths = c(3, 1)) 




# ==============================================================================

plot_slope_kg_2 <- ggplot(data = evap_slope_data, aes(x = slope_p_minus_e, y = kg_name, fill = kg_name)) +
  geom_boxplot(outlier.shape = NA) + # outlier.shape = 19, outlier.size = 0.5
  stat_summary(fun.y=mean, geom="point", size=2, color="black") +
  scale_fill_manual(values =  KG_class_names) +
  xlab("Slope of P-E [mm/month]") +
  # ylab("Koppen Geiger Class") +
  # xlim(c(-0.15, 0.2)) + 
  scale_x_continuous(breaks = seq(from = -0.2, to = 0.2, by = 0.1), limits = c(-0.15, 0.2))+
  geom_vline(xintercept = 0, color = "red") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 15),
    axis.line   = element_blank(),
    axis.text = element_text(size = 15),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(), 
    # panel.border = element_blank(),
    legend.position = 'none'
    # panel.grid.major.x = element_blank(),
    # panel.grid.minor.x = element_blank()) 
  )
plot_slope_kg_2


# entropy of data
plot_entrpy_arid_2 <-  
  ggplot() +
  geom_col(
    data = arid_evap_entropy_data,
    aes(y = kg_name, x = normalized_entropy_arid, fill = kg_name, ),
    color = 'black'
  ) +
  scale_fill_manual(values =  KG_class_names) +
  # scale_x_reverse() +
  scale_y_discrete(position = "left") +
  xlab("Normalized Entropy of PET/P") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 15),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 15),
    # axis.text.y = element_blank(),
    # axis.ticks.y = element_blank(),
    axis.line   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = 'none'
    # panel.border = element_blank(),
  ) 

plot_entrpy_arid_2

plot_entrpy_evap_2 <-
  ggplot() +
  geom_col(
    data = arid_evap_entropy_data,
    aes(y = kg_name, x = normalized_entropy_evap, fill = kg_name),
    color = 'black'
  ) +
  scale_fill_manual(values =  KG_class_names) +
  scale_x_reverse() +
  scale_y_discrete(position = "right") +
  xlab("Normalized Entropy of E/P") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 15),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 15),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = 'none'
  )

plot_entrpy_evap_2

plot_grid(plot_entrpy_arid_2, plot_slope_kg_2, plot_entrpy_evap_2, ncol = 3, align = "h",
          rel_widths = c(1, 3, 1)) 




# ==============================================================================
# visualize the categories of precip datasets
precip_category_color <- c(observational = '#4D96FF', 
                           reanalysis = '#36AE7C', 
                           satellite_based = '#F9D923')

plot_precip_category <- 
  ggplot(data = evap_slope_data, aes(x = precip_cat_evap, y = slope_p_minus_e, fill = precip_cat_evap)) +
  geom_violin() + 
  scale_fill_manual(values = precip_category_color) +
  geom_boxplot(width=0.1, fill = 'white', outlier.shape = NA) +
  stat_summary(fun.y=mean, geom="point", size=2, color="black") +
  xlab("Category of Precipitation Datasets") +
  ylab("Slope of P-E [mm/month]") +
  ylim(c(-0.15, 0.20)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_hline(yintercept = -0.15, color = "black") +
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
precip_cat_entropy <- data.table(category = unique(evap_slope_data$precip_cat_evap), 
                      entropy_kg = unique(evap_slope_data$entropy_precip_cat_evap))
cat_number <- evap_slope_data[, length(evap_index), by = precip_cat_evap]
bin_number <- length(evap_bin) - 1
precip_cat_entropy[, number :=  cat_number$V1][, bin_size := bin_number]
precip_cat_entropy[,max_entropy_kg := NA]
for (i in 1:length(precip_cat_entropy$number)) {
  precip_cat_entropy$max_entropy_kg[i] <- max_entropy(sample_size = precip_cat_entropy$number[i], bin_size = 64)
}
precip_cat_entropy[, normalized_entropy := entropy_kg/max_entropy_kg]


# plot_entrpy_precip_category_data <- 
#   data.table(kg_code = unique(evap_slope_data$kg_code), entropy_kg = unique(evap_slope_data$entropy_kg))

plot_entrpy_precip_category <- 
  ggplot() +
  geom_col(data = precip_cat_entropy, 
           aes(x = category, y = normalized_entropy, 
               fill = category), 
           color = 'black', width = 0.4) +
  scale_fill_manual(values = precip_category_color) + 
  scale_y_reverse() +
  scale_x_discrete(position = "top") +
  ylab("Normalized Entropy of E/P") +
  
  geom_hline(aes(yintercept = 0), color = "black") +
  ggtitle('Distributions of P-E using different precipitation categories') + 
  theme_bw() +
  theme(
    axis.title = element_text(size = 15),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 15),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(size=20),
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



# descriptive_evap <- evap_slope_data[, describe(evap_index), by = precip_cat_evap]
# descriptive_slope <- evap_slope_data[, describe(slope_p_minus_e), by = precip_cat_evap]



#=============================================================================== 
# remove Sahara
evap_slope_data_without_sahara <- evap_slope_data[kg_code != 4, ]


# calculation of entropy after removing Sahara
evap_slope_data_without_sahara[, entropy_precip_cat_evap := 
                                 {freq_tbl_evap <- table(cut(evap_index, breaks = evap_bin))
                                 entropy(freq_tbl_evap)
                                 }, by = .(precip_cat_evap)]

# bin_number <- length(evap_bin) - 1
precip_cat_entropy_without_sahara <- data.table(category = unique(evap_slope_data_without_sahara$precip_cat_evap), 
                                                entropy_kg = unique(evap_slope_data_without_sahara$entropy_precip_cat_evap))


plot_precip_category_without_sahara <- 
  ggplot(data = evap_slope_data_without_sahara, aes(x = precip_cat_evap, y = slope_p_minus_e, fill = precip_cat_evap)) +
  geom_violin() + 
  scale_fill_manual(values = precip_category_color) +
  geom_boxplot(width=0.1, fill = 'white', outlier.shape = NA) +
  stat_summary(fun.y=mean, geom="point", size=2, color="black") +
  xlab("Category of Precipitation Datasets") +
  ylab("Slope of P-E [mm/month]") +
  ylim(c(-0.15, 0.2)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_hline(yintercept = -0.15, color = "black") +
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

plot_precip_category_without_sahara




cat_number <- evap_slope_data_without_sahara[, length(evap_index), by = precip_cat_evap]
bin_number <- length(evap_bin) - 1
precip_cat_entropy_without_sahara[, number :=  cat_number$V1][, bin_size := bin_number]
precip_cat_entropy_without_sahara[,max_entropy_kg := NA]
for (i in 1:length(precip_cat_entropy_without_sahara$number)) {
  precip_cat_entropy_without_sahara$max_entropy_kg[i] <- max_entropy(sample_size = precip_cat_entropy_without_sahara$number[i], bin_size = 64)
}
precip_cat_entropy_without_sahara[, normalized_entropy := entropy_kg/max_entropy_kg]


plot_entrpy_precip_category_without_sahara <- 
  ggplot() +
  geom_col(data = precip_cat_entropy_without_sahara, 
           aes(x = category, y = normalized_entropy, 
               fill = category), 
           color = 'black', width = 0.4) +
  scale_fill_manual(values = precip_category_color) +
  # scale_y_continuous(breaks = c(0.2, 0.10, 0)) +
  scale_y_reverse() +
  scale_x_discrete(position = "top") +
  ylab("Normalized Entropy of E/P") +
  # ylim(c(0.3, 0)) +
  geom_hline(aes(yintercept = 0), color = "black") +
  ggtitle('Distributions of P-E using different precipitation categories without Sahara desert') + 
  theme_bw() +
  theme(
    axis.title = element_text(size = 15),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 15),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(size=20),
    # axis.line.y = element_blank(),
    # panel.border = element_blank(),
    # axis.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = 'none'
  ) 

plot_entrpy_precip_category_without_sahara


plot_grid(plot_entrpy_precip_category_without_sahara, plot_precip_category_without_sahara,  nrow = 2, 
          axis = "l", align = "v", rel_heights = c(1, 2.5)) 
















# aridity dataset categories for the whole med
fill_color <- c(temperature_based_observational = '#4D96FF', 
                temperature_based_reanalysis = '#36AE7C', 
                temperature_based_satellite_based = '#F9D923',
                combinational_observational = '#96DAE4', 
                combinational_reanalysis = '#A1DE93', 
                combinational_satellite_based = '#FFFD95')




p4_data <- data.table(entropy_comb = unique(aridity_data$entropy_cat_comb), 
                      cat_comb = unique(aridity_data$cat_comb))


plot_aridity <-  
  ggplot(data = aridity_data, aes(x = cat_comb, y = arid_index, fill = cat_comb)) +
  geom_violin() + 
  scale_fill_manual(values = fill_color) +
  # scale_color_manual(values = color_color) +
  geom_boxplot(width=0.1, outlier.shape = NA,fill = "white") +
  labs(y = 'Aridity index PET/P', x ="Dataset categories") +
  stat_summary(fun.y=mean, geom="point", size=2, color="black") +
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

#


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


# for (i in 1:length(name_kg)) {
i = 15
  plot_aridity_kg <-  
    ggplot(data = aridity_data[kg_name == name_kg[i],]) +
    geom_boxplot(aes(fill = cat_comb, x = arid_index),  outlier.shape = NA, size = 1) +
    # xlim(c(0, 20)) + #xlab('aridity index PET/P') +
    xlab('Aridity Index PET/P') +
    scale_fill_manual(values = fill_color) +
    geom_vline(aes(xintercept = 0), color = "black", size = 2) +
    geom_vline(aes(xintercept = 1), color = "#0047AC", size = 2) +
    geom_vline(aes(xintercept = 2), color = "#1497D4", size = 2) +
    geom_vline(aes(xintercept = 5), color = "#FFD301", size = 2) +
    geom_vline(aes(xintercept = 20), color = "#FFB921", size = 2) +
    scale_x_continuous(breaks = c(0, 1, 2, 3, 5, 20), limits = c(0, 2)) +
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
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = 'none'# c(0.9, 0.1)
    ) + ggtitle(name_kg[i]) 
    
  plot_aridity_kg
  # guides(shape = guide_legend(override.aes = list(size = 0.5)))
  
  font_size <- 5
  x_position <- 0.4
  
  plot_aridity_entropy_cat_kg <-  
    ggplot(data = p5_data[kg_name == name_kg[i],], 
           aes(x = entropy_cat_comb_kg, y = cat_comb, 
           fill = cat_comb)) + 
    geom_bar(stat="identity", color = 'black') + 
    scale_x_reverse() +
    scale_fill_manual(values = fill_color) +
    # xlim(c(2,0)) +
    # scale_x_discrete(position = "top") +
    xlab("Entropy of PET/P") +
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
    annotate("text", x = x_position, y = 6, 
             label = 'atop(bold("P: Satellite-based; PET: Temperature-based"))', 
             parse = TRUE, size = font_size) +
    
    annotate("text", x = x_position, y = 5, 
             label = 'atop(bold("P: Reanalysis; PET: Temperature-based"))', 
             parse = TRUE, size = font_size) +
    
    annotate("text", x = x_position, y = 4, 
             label = 'atop(bold("P: Observational; PET: Temperature-based"))', 
             parse = TRUE, size = font_size) +
    
    annotate("text", x = x_position, y = 3, 
             label = 'atop(bold("P: Satellite-based; PET: Combinational"))', 
             parse = TRUE, size = font_size) +
    
    annotate("text", x = x_position, y = 2, 
             label = 'atop(bold("P: Reanalysis; PET: Combinational"))', 
             parse = TRUE, size = font_size) +
    
    annotate("text", x = x_position, y = 1, 
             label = 'atop(bold("P: Observational; PET: Combinational"))', 
             parse = TRUE, size = font_size) 
    
  
  
  plot_aridity_entropy_cat_kg
  
  # plot_aridity_entropy
  # plot_list[[i]] <- plot_grid(plot_aridity_kg, plot_aridity_entropy_cat_kg,  nrow = 1, 
  #           axis = "l", align = "h", rel_widths = c(2, 1)) 
  assign(name_kg[i], plot_grid(plot_aridity_kg, plot_aridity_entropy_cat_kg,  nrow = 1, 
                               axis = "l", align = "h", rel_widths = c(2, 1)))
  # }
  plot_grid(plot_aridity_kg, plot_aridity_entropy_cat_kg,  nrow = 1, 
            axis = "l", align = "h", rel_widths = c(2, 1)) 
  
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







