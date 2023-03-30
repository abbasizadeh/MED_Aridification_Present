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
  w_vec <- rep(logb(4, base = w_dummie), length(seq(0, 5, by = 0.25)))
  
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
budyko_data <- readRDS(file = paste0(path_load, "budyko_data.rds"))
unique(budyko_data$combination)

# check the data
budyko_data[arid_index == Inf, arid_index := NA]
budyko_data[arid_index == max(arid_index, na.rm = T), ]
summary(budyko_data$arid_index)
# budyko_data[arid_index > 20, arid_index := 20]

budyko_data[evap_index == Inf, evap_index := NA]
budyko_data[evap_index == max(evap_index, na.rm = T), ]
summary(budyko_data$evap_index)
# budyko_data[evap_index > 20, evap_index := 20]

# remove na value
budyko_data <- na.omit(budyko_data)

budyko_data

# add Koppen Geiger (KG) classes to the budyko_data 
# convert the Budyko dataframe and into the shapefile
budyko_shape_file <- st_as_sf(budyko_data,
                              coords = c("x", "y"), 
                              crs = 4326)
# read Koppen Geiger raster
kg_raster <- raster("~/MED_Aridification_Present/data/archive/Beck_KG_present_025.tif")



# extract the KG classes
kg_extract <-
  raster::extract(
    kg_raster,
    budyko_shape_file,
    method = 'simple',
    buffer = NULL,
    small = FALSE,
    cellnumbers = FALSE,
    fun = NULL,
    na.rm = TRUE,
    layer,
    nl,
    df = FALSE,
    factors = FALSE
  )

# add KG classes to the shape file

budyko_shape_file$KG <- kg_extract

# budyko_KG_data <- data.table(budyko_shape_file)

# add KG classes to the budyko data frame file
budyko_data$kg_code <- kg_extract

budyko_data[kg_code == 0, kg_code := NA]
unique(budyko_data$kg_code)
budyko_data <- na.omit(budyko_data)

# estimate the w (Omega) values of the the points (budyko data frame)
# define a vector to save omega values
estimated_omega_vec <- c()

for (i in 1:length(budyko_data$arid_index)) {
  
  # objective function
  if(budyko_data$arid_index[i] != 0 & budyko_data$evap_index[i] != 0){
    mae = function(omega) {
      A <- budyko_data$arid_index[i]
      E <- budyko_data$evap_index[i]
      mymae = abs(E - (1 + (A) - (1 + (A) ^ omega) ^ (1 / omega)))
    }
    
    # number of iteration
    itermaxW <- 20
    
    # DEoptim setting
    decntr <- DEoptim.control(
      VTR = 0,
      strategy = 2,
      bs = FALSE,
      NP = 10,
      itermax = itermaxW,
      CR = 0.25,
      F = 0.7,
      trace = FALSE,
      initialpop = NULL,
      storepopfrom = itermaxW + 1,
      storepopfreq = 1,
      p = 0.2,
      c = 0,
      reltol = sqrt(.Machine$double.eps),
      steptol = itermaxW
    )
    
    u <- DEoptim(
      lower = 1,
      upper = 4,
      fn = mae,
      control = decntr
    )
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
  
entropy_data_frame

# normalize the values of entropy according to the number of points (grids) in each KG class
entropy_data_frame[, normalized_entropy:= entropy*number/sum(number)]


dummie_entropy_data_frame <- merge(budyko_data, entropy_data_frame, by = 'kg_code')
budyko_data$entropy <- dummie_entropy_data_frame$entropy
budyko_data$normalized_entropy <- dummie_entropy_data_frame$normalized_entropy

saveRDS(budyko_data, "~/shared/data_projects/med_datasets/2000_2019_data/sim/budyko/evaporative_aridity_indices/budyko_data.rds")
saveRDS(bins, "~/shared/data_projects/med_datasets/2000_2019_data/sim/budyko/evaporative_aridity_indices/bins_budyko.rds")





