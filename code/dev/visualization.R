budyko_data <- readRDS( "~/shared/data_projects/med_datasets/2000_2019_data/sim/budyko/evaporative_aridity_indices/budyko_data.rds")
bins <- readRDS( "~/shared/data_projects/med_datasets/2000_2019_data/sim/budyko/evaporative_aridity_indices/bins_budyko.rds")
library(viridis) 

KG_class <- c("#FFBF9B", "#56B4E9", "#009E73", "#AACB73", "#00337C", 
                       "#D55E00", "#CC79A7", "#159895", "#00FFD1", "#E11299", 
                       "#867070", "#16FF00", "#820000","#FF0000" , "#A31ACB", "#FFEA20")

ggplot(data = budyko_data) +
  geom_point( aes(x = arid_index, y = evap_index, color = factor(kg_code)), size = 0.1) +
  scale_color_manual(values =  KG_class) + 
  geom_line(data = bins, aes(x = aridity, y = evaporative, group = factor(Omega)), color = "grey") + 
  geom_abline(intercept = 0, slope = 1) +
  ylim(c(0, 3)) + xlim(c(0, 20)) + labs(x = "Aridity Index [PET/P]", y = "Evaporative Index [E/P]", color = "KG_class") +
  geom_segment(aes(x = 1, xend = 20, y = 1, yend = 1)) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  facet_wrap(vars(combination)) 

# scale_colour_hue(l = 15, c = 300) +

# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6

ggplot(data = budyko_data[kg_code == 15,]) +
  geom_point( aes(x = arid_index, y = evap_index), size = 0.2) +
  geom_line(data = bins, aes(x = aridity, y = evaporative, color = factor(Omega))) + 
  geom_segment(aes(x = 1, xend = 20, y = 1, yend = 1)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Aridity Index [PET/P]", y = "Evaporative Index [E/P]", color = "Omega") +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  ylim(c(0, 1.5)) + xlim(c(0, 6)) +
  facet_wrap(vars(combination)) +
  ggtitle("Climate class:  Cfb  Temperate, no dry season, warm summer") 



ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 26,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 45) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))



# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 29 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 45) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))


# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 29 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 14) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))


# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 14 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 14) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))



# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 0 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 14) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))




# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 25 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 14) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))



# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 7 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 14) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))


# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 27 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 14) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))


# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 8 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 14) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))


# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 9 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 14) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))


# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 18 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 14) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))

# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 19 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 14) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))



# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 17 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 14) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))



# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 4 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 14) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))



# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 5 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 14) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))

# 15 26 29 14  0 25  7 27  8  9 18 19 17  4  5  6
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    group = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) +
  geom_point(data = budyko_KG_data[KG == 6 ,],
             aes(x = arid_index, y = evap_index, color = combination),
             alpha = 1) +
  scale_colour_hue(l = 14) +
  facet_wrap(vars(combination)) +
  ylim(c(0, 1.5)) + xlim(c(0, 6))





