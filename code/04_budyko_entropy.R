# Calculation of entropy on the Budyko space

# load global variables and packages
source("./code/source/global_variables.R")

# load functions
source('./code/source/functions.R')
library(RcppDE)


# evaporative = E/P
# aridity = PET/P
# w: omiga (free parameter)

# Fu's equation (the relationship between evaporative and aridity indices)
evaporative <- function(aridity, w) {
  return(1 + (aridity) - (1 + (aridity) ^ w) ^ (1 / w))
}

# Define evaporative index, aridity index and omega vectors
evaporative_vec <- c()
aridity_vec <- c()
w_vec <- c()

# Define bins between [1, 4]
bins <- data.frame()
w_dummie <- 4

while(w_dummie > 1.40) {#1.4142136
  
  for (A in seq(0, 5, by = 0.25)) {
    evaporative_vec <-
      append(evaporative_vec, evaporative(aridity = A, w = logb(4, base = w_dummie)))
    aridity_vec <- append(aridity_vec, A)
  }
  w_vec <- rep(logb(4, base = w_dummie), length(seq(0, 5, by = 0.25)))
  
  fu <-
    data.frame(cbind(aridity_vec, evaporative_vec, w_vec))
  
  evaporative_vec <- c()
  aridity_vec <- c()
  w_vec <- c()
  
  bins <- rbind(bins, fu)
  w_dummie <- w_dummie - 0.25857864 # logb(4, base = 4-(10*0.25857864)) = 4;
}


names(bins) <- c("aridity", "evaporative", "Omega")
bins$Omega <- round(bins$Omega, digits = 2)
Omega_bins <- unique(bins$Omega)



# # test using random numbers
# # random aridity index
# arid_index_dummie <-
#   runif(100, min = 0.5, max = 5)         # min = 3, max = 5
# 
# # random evaporative index
# evap_index_dummie <-
#   runif(100, min = 0.0, max = 1)   # min = 0.25, max = 0.3
# 
# combined_data <- data.frame(cbind(arid_index_dummie, evap_index_dummie))


path_load <- "../shared/data_projects/med_datasets/2000_2019_data/sim/budyko/evaporative_aridity_indices/"

budyko_data <- readRDS(file = paste0(path_load, "budyko_data.rds"))


unique(budyko_data$combination)

budyko_data[arid_index == Inf, arid_index := NA]
budyko_data[arid_index > 20, arid_index := 20]

budyko_data[evap_index == Inf, evap_index := NA]
budyko_data[evap_index > 20, evap_index := 20]

budyko_data <- na.omit(budyko_data)

summary(budyko_data$arid_index)
summary(budyko_data$evap_index)
budyko_data[2500, ]


budyko_data



#--- or this ---#
# convert the Budyko dataframe and into the shapefile
budyko_shape_file <- st_as_sf(budyko_data,
                              coords = c("x", "y"), 
                              crs = 4326)
# read koppen geiger raster
kg_raster <- raster("~/MED_Aridification_Present/data/archive/KG_classes/Beck_KG_present_mediterranian_025.tif")


kg_extract <- raster::extract(kg_raster, budyko_shape_file,method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                            fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)

budyko_shape_file$KG <-kg_extract
budyko_KG_data <- data.table(budyko_shape_file)
# visualization
# color = factor(Omega)





# estimating w (Omega) of the the points
estimated_omega_vec <- c()

for (i in 1:length(budyko_data$arid_index)) {
  # objective function
  if(budyko_data$arid_index[i] !=0 & budyko_data$evap_index[i] != 0){
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


budyko_data$estimated_omega_vec <- estimated_omega_vec
budyko_data$KG <- budyko_shape_file$KG

med_kg_codes <- c(unique(budyko_data$KG))

colume_names = c("entropy", "number", "kg_code")
entropy_data_frame = data.table(matrix(nrow = length(med_kg_codes), ncol = length(colume_names))) 
colnames(entropy_data_frame) = colume_names

for(kg_ite in 1:length(med_kg_codes)){
  
  # calculate entropy of points on the Budyko space
  entropy_total <- 0
  
  # calculate the number of each point in the each bin as a table
  freq_tbl <-
    table(cut(budyko_data[KG == med_kg_codes[kg_ite], estimated_omega_vec], breaks = Omega_bins))
  
  
  for(bin_itr in 1:length(freq_tbl)) {
    
    if (freq_tbl[bin_itr] == 0) {
      entrpy_dummie <- 0
    } else{
      entrpy_dummie <-
        (freq_tbl[bin_itr] / length(budyko_data[KG == med_kg_codes[kg_ite], estimated_omega_vec])) * log2(freq_tbl[bin_itr] / length(budyko_data[KG == med_kg_codes[kg_ite], estimated_omega_vec])) * -1
    }
    entropy_total <- entropy_total + entrpy_dummie
  }
  
  entropy_data_frame$entropy[kg_ite] <- entropy_total
  entropy_data_frame$number[kg_ite] <-
  length(budyko_data[KG == med_kg_codes[kg_ite], estimated_omega_vec])
  entropy_data_frame$kg_code[kg_ite] <- med_kg_codes[kg_ite]
  }
  
entropy_data_frame
entropy_data_frame[, normalized_entropy:= entropy*number/sum(number)]

for (row_num in 1:length(budyko_shape_file$KG)) {
  
  budyko_shape_file$entropy[row_num] <- entropy_data_frame[kg_code == budyko_shape_file$KG[row_num], normalized_entropy]
}



