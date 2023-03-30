# load global variables and packages
source("./code/source/global_variables.R")

# load functions
source('./code/source/functions.R')

path_slope_files <- ("~/shared/data_projects/med_datasets/2000_2019_data/slopes/")

budyko_data <- readRDS( "~/shared/data_projects/med_datasets/2000_2019_data/sim/budyko/evaporative_aridity_indices/budyko_data.rds")
head(budyko_data)
unique(budyko_data$combination)
slope_files <- list.files(path_slope_files)

# define the data frame to save the slope data
col_names <- c('x', 'y', 'combination', 'slope', 'arid_index', 'evap_index', 'kg_code', 'estimated_omega')
col_names <- c('x', 'y', 'value', 'combination')
slope_data <- data.table(matrix(nrow = 0, ncol = length(col_names)))
names(slope_data) <- col_names

for (i in 1:length(slope_files)) {
  # open the nc file and convert it into the raster format
  raster_data_frame_dummie <- raster(paste0(path_slope_files, slope_files[i])) %>%
    as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE)
  
  # read the name of each file
  name_dummie <- read.table(text = slope_files[i], sep = "_", as.is = TRUE)
  
  # create combination name using the name of each file
  combination_dummie <- paste0("pet", "_", name_dummie$V2, "_", "p", "_", name_dummie$V1)
  
  # add the combination name to the data frame
  raster_data_frame_dummie$combination <- rep(combination_dummie, length(raster_data_frame_dummie$x))
  # raster_data_frame_dummie$x <- round(raster_data_frame_dummie$x, digits = 3)
  # raster_data_frame_dummie$y <- round(raster_data_frame_dummie$y, digits = 3)
  
  
  # remove extra the column
  raster_data_frame_dummie$layer <- NULL
  # data_frame_dummie <- merge(raster_data_frame_dummie, budyko_data, by = c('x', 'y', 'combination'))
  # names(data_frame_dummie) <- col_names
  # add the results to the slope data frame
  slope_data <- rbind(slope_data, raster_data_frame_dummie)
  
  
}


# merge the slope data with budyko data
slope_data$x <- round(slope_data$x , digits = 3)
slope_data$y <- round(slope_data$y , digits = 3)

slope_entropy_data <- slope_data[budyko_data, on = .(x, y, combination), allow.cartesian = TRUE]


unique(slope_entropy_data$combination)
unique(budyko_data$combination)
unique(slope_data$combination)


bar_plot_data <-  unique(budyko_data[, .(kg_code, entropy, normalized_entropy)])

p1 <- ggplot(data = slope_entropy_data) + 
        geom_boxplot(aes(x = value, y = factor(kg_code))) + 
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
to_plot_sf <- budyko_data[, .(x, y, normalized_entropy)] %>% 
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
