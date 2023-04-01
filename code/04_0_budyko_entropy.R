# Calculation of entropy on the Budyko space

# load global variables and packages
source("./code/source/global_variables.R")

# load functions
source('./code/source/functions.R')
library(RcppDE)


# evaporative = E/P
# aridity = PET/P
# w: omiga (free parameter)

# Fu's equation (the relationship between evaporative and aridity indices) steady-state condition
evaporative <- function(aridity, w) {
  return(1 + (aridity) - (1 + (aridity) ^ w) ^ (1 / w))
}


# Define evaporative index, aridity index and omega vectors
evaporative_vec <- c()
aridity_vec <- c()
w_vec <- c()


# Define bins between [1, 4] (discretizing the Budyko space)
bins <- data.frame()
w_dummie <- 4

while(w_dummie > 1.40) {#1.4142136

  for (A in seq(0, 100, by = 0.25)) {
    evaporative_vec <-
      append(evaporative_vec, evaporative(aridity = A, w = logb(4, base = w_dummie)))
    aridity_vec <- append(aridity_vec, A)
  }
  w_vec <- rep(logb(4, base = w_dummie), length(seq(0, 100, by = 0.25)))

  fu <- data.frame(cbind(aridity_vec, evaporative_vec, w_vec))

  evaporative_vec <- c()
  aridity_vec <- c()
  w_vec <- c()

  bins <- rbind(bins, fu)
  w_dummie <- w_dummie - 0.25857864 # logb(4, base = 4-(10*0.25857864)) = 4;
}


names(bins) <- c("aridity", "evaporative", "Omega")
bins$Omega <- round(bins$Omega, digits = 2)


# define omega values of each bin on Budyko space
Omega_bins <- c(unique(bins$Omega))


# load aridity and evaporative indices from preprocess  
path_load <- "~/shared/data_projects/med_datasets/2000_2019_data/sim/budyko/evaporative_aridity_indices/"
budyko_data <- readRDS(file = paste0(path_load, "budyko_data_03.rds"))
unique(budyko_data$combination)


# check the data
budyko_data[arid_index == max(arid_index, na.rm = T), ]
summary(budyko_data$arid_index)
budyko_data[evap_index == max(evap_index, na.rm = T), ]
summary(budyko_data$evap_index)


# remove na value
# budyko_data <- na.omit(budyko_data)

budyko_data

# add Koppen Geiger (KG) classes to the budyko_data 
# convert the Budyko dataframe and into the shapefile
budyko_shape_file <- st_as_sf(budyko_data,
                              coords = c("x", "y"), 
                              crs = 4326)
# read Koppen Geiger raster
kg_raster <- raster("~/shared/data_projects/med_datasets/2000_2019_data/KG_classes/Beck_KG_present_025.tif")
#~/github_projects/MED_Aridification_Present/data/archive/KG_classes

# extract the KG classes
kg_extract <-
  raster::extract(kg_raster, budyko_shape_file, method = 'simple', buffer = NULL,
    small = FALSE, cellnumbers = FALSE, fun = NULL, na.rm = TRUE, layer, nl,
    df = FALSE, factors = FALSE
  )

# add KG classes to the shape file

budyko_shape_file$KG <- kg_extract

# budyko_KG_data <- data.table(budyko_shape_file)

# add KG classes to the budyko data frame file
budyko_data$kg_code <- kg_extract

budyko_data[kg_code == 0, kg_code := NA]
unique(budyko_data$kg_code)
# budyko_data <- na.omit(budyko_data)

# estimate the w (Omega) values of the the points (budyko data frame)
# define a vector to save omega values
estimated_omega_vec <- c()

for (i in 1:length(budyko_data$arid_index)) {
  
  # objective function
  if(budyko_data$arid_index[i] != 0 & 
     budyko_data$evap_index[i] != 0 & 
     !is.na(budyko_data$arid_index[i]) & 
     !is.na(budyko_data$evap_index[i])){
    
    mae = function(omega) {
      A <- budyko_data$arid_index[i]
      E <- budyko_data$evap_index[i]
      mymae = abs(E - (1 + (A) - (1 + (A) ^ omega) ^ (1 / omega)))
    }
    
    # number of iteration
    itermaxW <- 20
    
    # DEoptim setting
    decntr <- DEoptim.control(
      VTR = 0, strategy = 2, bs = FALSE, NP = 10, itermax = itermaxW, CR = 0.25, 
      F = 0.7, trace = FALSE, initialpop = NULL, storepopfrom = itermaxW + 1, 
      storepopfreq = 1, p = 0.2, c = 0, reltol = sqrt(.Machine$double.eps), steptol = itermaxW)
    
    u <- DEoptim(lower = 1, upper = 4, fn = mae, control = decntr)
    estimated_omega_vec <-
      append(estimated_omega_vec, as.numeric(u$optim$bestmem))
    
  }else{
    
    estimated_omega_vec <-
      append(estimated_omega_vec, NA)
  }
  
}

# add the estimated omega to Budyko data frame budyko_data
budyko_data$estimated_omega <- estimated_omega_vec


# extract the Koppen Geiger (KG) codes in Mediterranean region and save it to med_kg_codes data frame
med_kg_codes <- c(unique(budyko_data$kg_code))

# define a data frame (entropy_data_frame) to save entropy values for each KG class
colume_names = c("entropy", "number", "kg_code")
entropy_data_frame = data.table(matrix(nrow = length(med_kg_codes), ncol = length(colume_names))) 
colnames(entropy_data_frame) = colume_names


# calculate entropy of points on the Budyko space
for(kg_ite in 1:length(med_kg_codes)){
  
  entropy_total <- 0
  
  # calculate the number of each point in the each bin as a table
  freq_tbl <-
    table(cut(budyko_data[kg_code == med_kg_codes[kg_ite], estimated_omega], breaks = Omega_bins))
  
  for(bin_itr in 1:length(freq_tbl)) {
    
    if (freq_tbl[bin_itr] == 0) {
      entrpy_dummie <- 0
    } else{
      entrpy_dummie <-
        (freq_tbl[bin_itr] / length(budyko_data[kg_code == med_kg_codes[kg_ite], estimated_omega])) * 
        log2(freq_tbl[bin_itr] / length(budyko_data[kg_code == med_kg_codes[kg_ite], estimated_omega])) * -1
    }
    entropy_total <- entropy_total + entrpy_dummie
  }
  
  entropy_data_frame$entropy[kg_ite] <- entropy_total
  entropy_data_frame$number[kg_ite] <-
  length(budyko_data[kg_code == med_kg_codes[kg_ite], estimated_omega])
  entropy_data_frame$kg_code[kg_ite] <- med_kg_codes[kg_ite]
  }
  

entropy_data_frame <- na.omit(entropy_data_frame)

# normalize the values of entropy according to the number of points (grids) in each KG class
entropy_data_frame[, normalized_entropy:= entropy*number/sum(number)]


budyko_data <- merge(budyko_data, entropy_data_frame, by = 'kg_code')

budyko_data[kg_code == 0,]

saveRDS(budyko_data, "~/shared/data_projects/med_datasets/2000_2019_data/sim/budyko/evaporative_aridity_indices/budyko_data_04.rds")
saveRDS(entropy_data_frame, "~/shared/data_projects/med_datasets/2000_2019_data/sim/budyko/evaporative_aridity_indices/entropy_data_frame.rds")
saveRDS(bins, "~/shared/data_projects/med_datasets/2000_2019_data/sim/budyko/evaporative_aridity_indices/bins_budyko.rds")




# budyko_data[, entropy_shanon_aridity := (entropy.empirical(arid_index, unit = "log2")), 
#             by = kg_code]
# 
# budyko_data[, entropy_shanon_evaporative := entropy.empirical(evap_index, unit = "log2"), 
#             by = kg_code]
# 
# total_length <- length(budyko_data$kg_code)
# budyko_data[, entropy_shanon_aridity := entropy_shanon_aridity*length(arid_index)*length(arid_index)/total_length,
#             by = kg_code]
# 
# unique(budyko_data$entropy_shanon_aridity)
# unique(budyko_data$entropy_shanon_evaporative)
# 
# plot(unique(budyko_data$entropy_shanon_aridity),unique(budyko_data$entropy_shanon_evaporative))
# 
# budyko_data[, length(arid_index), by = kg_code]
# 
# ggplot(data = b_test, aes(x = (evap_index))) + geom_density(alpha=.2, fill="#FF6666") 
# 
# ggplot(data = budyko_data, aes(x = evap_index)) +  
#   geom_histogram(aes(y=..density..), binwidth=.1, colour="black", fill="white") +  
#   geom_density(alpha=.2, fill="#FF6666") + 
#   facet_wrap(vars(factor(kg_code)))
# 
# b_test <- budyko_data[arid_index < 5,]
# 
# ggplot(data = b_test, aes(x = evap_index)) +  
#   geom_histogram(aes(y=..density..), binwidth=.1, colour="black", fill="white") +  
#   geom_density(alpha=.2, fill="#FF6666") + 
#   facet_wrap(vars(factor(kg_code)))
# 
# 
