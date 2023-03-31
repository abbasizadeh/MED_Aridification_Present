# load global variables and packages
source("./code/source/global_variables.R")

# load functions
source('./code/source/functions.R')


slope_budyko_data <- 
  readRDS( "~/shared/data_projects/med_datasets/2000_2019_data/sim/budyko/evaporative_aridity_indices/slope_budyko_data_07.rds")


bar_plot_data <- unique(slope_budyko_data[, .(kg_code, entropy, normalized_entropy)])

p1 <- ggplot(data = slope_budyko_data) + 
        geom_boxplot(aes(x = slope, y = factor(kg_code))) + 
        xlab("slope [mm/month]") +
        ylab("KG class code") +
        geom_vline(xintercept = 0, color = "red") +
  theme(
    # axis.title.x = element_blank(),
    # axis.text.x = element_blank(),
    # axis.ticks.x = element_blank(),
    axis.line   = element_blank(),
    panel.border = element_blank(),
    axis.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
  )


p2 <- ggplot() +
        geom_col(data = bar_plot_data, aes(y = factor(kg_code), x = normalized_entropy)) +
        scale_x_reverse() +  
  scale_y_discrete(position = "right") +
  theme(
    axis.title.y = element_blank(),
    # axis.text.x = element_blank(),
    # axis.ticks.x = element_blank(),
    axis.line   = element_blank(),
    panel.border = element_blank(),
    axis.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
  )

plot_grid(p1, p2, ncol = 2, align = "h", rel_widths = c(3, 1)) 

## Figures
to_plot_sf <- slope_budyko_data[, .(x, y, normalized_entropy)] %>% 
  rasterFromXYZ(res = c(0.25, 0.25),
                crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  st_as_stars() %>% st_as_sf()

entropy_med <- ggplot(to_plot_sf) +
  geom_sf(aes(color = normalized_entropy, fill = normalized_entropy)) +
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
