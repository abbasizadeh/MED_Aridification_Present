# load global variables and packages
source("./code/source/global_variables.R")

# load functions
source('./code/source/functions.R')

path_budyko <- "~/shared/data_projects/med_datasets/2000_2019_data/budyko/"
budyko_data <- readRDS(paste0(path_budyko, "04_1_budyko_data_joint_entropy.rds"))
entropy_kg <- readRDS(paste0(path_budyko, "04_1_joint_entropy_MI_KG.rds"))
entropy_comb <- readRDS(paste0(path_budyko, "04_1_joint_entropy_MI_comb.rds"))

bins <- readRDS( "~/shared/data_projects/med_datasets/2000_2019_data/budyko/04_0_bins_budyko_fu_equation.rds")
 
# KG colors: 
# 4: "#FF0000"  5: "#FF9696"  6: "#F5A500"  7: "#FFDC64"  8: "#FFFF00"  9: "#C8C800" 
# 14: "#C8FF50" 15: "#64FF50" 17: "#FF00FF" 18: "#C800C8" 
# 19: "#963296" 25: "#00FFFF" 26: "#37C8FF" 27: "#007D7D" 29: "#B2B2B2"

KG_class <- c("#FF0000", "#FF9696" , "#F5A500", "#FFDC64", 
                       "#FFFF00" , "#C8C800" , "#C8FF50", "#64FF50", "#FF00FF", 
                       "#C800C8", "#963296", "#00FFFF", "#37C8FF", "#007D7D","#B2B2B2" )

ggplot(data = budyko_data) +
  geom_point( aes(x = arid_index, y = evap_index, color = factor(kg_code)), size = 0.1) +
  scale_color_manual(values =  KG_class) + 
  geom_line(data = bins, aes(x = aridity, y = evaporative, group = factor(Omega)), color = "grey", alpha = 0.5) + 
  geom_abline(intercept = 0, slope = 1) +
  ylim(c(0, 3)) + xlim(c(0, 20)) + labs(x = "Aridity Index [PET/P]", y = "Evaporative Index [E/P]", color = "KG_class") +
  geom_segment(aes(x = 1, xend = 20, y = 1, yend = 1)) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  facet_wrap(vars(combination)) +
  theme_bw()



# barcharts

ggplot(data = entropy_kg) +
  geom_col(aes(x = factor(kg_code), y = normalized_entropy_kg))


# scale_colour_hue(l = 15, c = 300) +

# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
budyko_plot <- list()
itr <- 1

for(kg in unique(budyko_data$kg_code)){
  budyko_plot[[itr]] <-  
    ggplot(data = budyko_data[kg_code == kg, ]) +
    geom_point(aes(x = arid_index, y = evap_index), size = 0.5, color = "#D55E00") +
    geom_abline(intercept = 0, slope = 1) +
    geom_line(data = bins, aes(x = aridity, y = evaporative, group = factor(Omega)), alpha = 0.8, color = "grey") +
    
    # guides(color = guide_legend(override.aes = list(size = 5))) +
    labs(x = "Aridity Index [PET/P]", y = "Evaporative Index [E/P]", color = "Omega") +
    facet_wrap(vars(combination)) 
 itr = itr + 1 
}

# type = c("#fafa6e", "#c4ec74", "#92dc7e", "#64c987", "#39b48e", "#089f8f", "#00898a", "#08737f", "#215d6e","#2a4858")

budyko_plot[[1]] +
  geom_segment(aes(x = 1, xend = 5, y = 1, yend = 1)) +
  xlim(c(0, 5)) + ylim(c(0, 2)) +
  ggtitle("Climate class:  Cfb  Temperate, no dry season, warm summer") 


budyko_plot[[2]] +
  geom_segment(aes(x = 1, xend = 4, y = 1, yend = 1)) +
  xlim(c(0, 4)) + ylim(c(0, 2)) +
  ggtitle("Climate class:  Dfb  Cold, no dry season, warm summer") 


budyko_plot[[3]] +
  geom_segment(aes(x = 1, xend = 2, y = 1, yend = 1)) +
  xlim(c(0, 2)) + ylim(c(0, 1.5)) +
  ggtitle("Climate class:  ET   Polar, tundra") 


budyko_plot[[4]] +
  geom_segment(aes(x = 1, xend = 4, y = 1, yend = 1)) +
  xlim(c(0, 4)) + ylim(c(0, 2.5)) +
  ggtitle("Climate class:  Cfa  Temperate, no dry season, hot summer") 


budyko_plot[[5]] +
  geom_segment(aes(x = 1, xend = 4, y = 1, yend = 1)) +
  xlim(c(0, 4)) + ylim(c(0, 2.5)) +
  ggtitle("Climate class:  Dfa  Cold, no dry season, hot summer") 


budyko_plot[[6]] +
  geom_segment(aes(x = 1, xend = 30, y = 1, yend = 1)) +
  xlim(c(0, 30)) + ylim(c(0, 10)) +
  ggtitle("Climate class:  BSk  Arid, steppe, cold") 


budyko_plot[[7]] +
  geom_segment(aes(x = 1, xend = 2, y = 1, yend = 1)) +
  xlim(c(0, 2)) + ylim(c(0, 1.5)) +
  ggtitle("Climate class:  Dfc  Cold, no dry season, cold summer") 



budyko_plot[[8]] +
  geom_segment(aes(x = 1, xend = 25, y = 1, yend = 1)) +
  xlim(c(0, 25)) + ylim(c(0, 10)) +
  ggtitle("Climate class:  Csa  Temperate, dry summer, hot summer") 


budyko_plot[[9]] +
  geom_segment(aes(x = 1, xend = 10, y = 1, yend = 1)) +
  xlim(c(0, 10)) + ylim(c(0, 4)) +
  ggtitle("Climate class: Csb Temperate, dry summer, warm summer") 



budyko_plot[[10]] +
  geom_segment(aes(x = 1, xend = 4, y = 1, yend = 1)) +
  xlim(c(0, 4)) + ylim(c(0, 1.5)) +
  ggtitle("Climate class: Dsb  Cold, dry summer, warm summer") 


budyko_plot[[11]] +
  geom_segment(aes(x = 1, xend = 4, y = 1, yend = 1)) +
  xlim(c(0, 4)) + ylim(c(0, 1.5)) +
  ggtitle("Climate class: Dsc  Cold, dry summer, cold summer") 


budyko_plot[[12]] +
  geom_segment(aes(x = 1, xend = 5, y = 1, yend = 1)) +
  xlim(c(0, 5)) + ylim(c(0, 2)) +
  ggtitle("Climate class: Dsa  Cold, dry summer, hot summer") 


budyko_plot[[13]] +
  geom_segment(aes(x = 1, xend = 200, y = 1, yend = 1)) +
  xlim(c(0, 200)) + ylim(c(0, 20)) +
  ggtitle("Climate class: Cwc  Temperate, dry winter, cold summer") 


budyko_plot[[14]] +
  geom_segment(aes(x = 1, xend = 50, y = 1, yend = 1)) +
  xlim(c(0, 50)) + ylim(c(0, 10)) +
  ggtitle("Climate class: BWk  Arid, desert, cold") 


budyko_plot[[15]] +
  geom_segment(aes(x = 1, xend = 50, y = 1, yend = 1)) +
  xlim(c(0, 50)) + ylim(c(0, 20)) +
  ggtitle("Climate class: BSh  Arid, steppe, hot") 


unique(budyko_data$kg_code)[15]

