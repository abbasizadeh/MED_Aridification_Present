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



budyko_data[2500, ]


# visualization
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    color = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "red"
  ) + 
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "red"
  ) + 
  geom_point(data = budyko_test, 
             aes(x = arid_index, y = evap_index), alpha = 0.2) +
  ylim(c(0,1.5)) + xlim(c(0, 6)) +
  facet_wrap(vars(gridcode))


# estimating w (Omega) of the the points
estimated_omega_vec <- c()

for (i in 1:length(combined_data$arid_index_dummie)) {
  # objective function
  mae = function(omega) {
    A <- combined_data[i,]$arid_index_dummie
    E <- combined_data[i,]$evap_index_dummie
    mymae = abs(E - (1 + (A) - (1 + (A) ^ omega) ^ (1 / omega)))
  }
  
  # number of iteration
  itermaxW <- 50
  
  # DEoptim setting
  decntr <- DEoptim.control(
    VTR = 0,
    strategy = 2,
    bs = FALSE,
    NP = 10,
    itermax = itermaxW,
    CR = 0.25,
    F = 0.7,
    trace = TRUE,
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
}

combined_data$estimated_omega_vec <- estimated_omega_vec

# calculate the number of each point in the each bin as a table
freq_tbl <-
  table(cut(combined_data$estimated_omega_vec, breaks = Omega_bins))

# calculate entropy of points on the Budyko space
entropy_total <- 0

for (bin_itr in 1:length(freq_tbl)) {
  if (freq_tbl[bin_itr] == 0) {
    entrpy_dummie <- 0
  } else{
    entrpy_dummie <- (freq_tbl[bin_itr] / 100) * log2(freq_tbl[bin_itr] / 100) * -1
  }
  entropy_total <- entropy_total + entrpy_dummie
}
entropy_total





