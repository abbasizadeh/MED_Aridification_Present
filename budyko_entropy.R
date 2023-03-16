# Calculation of entropy on the Budyko scape

library(ggplot2)
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
w_tmp <- 4

while(w_tmp > 1.40) {#1.4142136
  
  for (A in seq(0, 5, by = 0.25)) {
    evaporative_vec <-
      append(evaporative_vec, evaporative(aridity = A, w = logb(4, base = w_tmp)))
    aridity_vec <- append(aridity_vec, A)
  }
  w_vec <- rep(logb(4, base = w_tmp), length(seq(0, 5, by = 0.25)))
  
  fu <-
    data.frame(cbind(aridity_vec, evaporative_vec, w_vec))
  
  evaporative_vec <- c()
  aridity_vec <- c()
  w_vec <- c()
  
  bins <- rbind(bins, fu)
  w_tmp <- w_tmp - 0.25857864 # logb(4, base = 4-(10*0.25857864)) = 4;
}


names(bins) <- c("aridity", "evaporative", "Omega")
bins$Omega <- round(bins$Omega, digits = 2)
Omega_bins <- unique(bins$Omega)


# test using random numbers

# random aridity index
arid_index_tmp <-
  runif(100, min = 0.5, max = 5)         # min = 3, max = 5

# random evaporative index
evap_index_tmp <-
  runif(100, min = 0.0, max = 1)   # min = 0.25, max = 0.3

combined_data <- data.frame(cbind(arid_index_tmp, evap_index_tmp))


# visualization
ggplot(data = bins) +
  geom_line(aes(
    x = aridity,
    y = evaporative,
    color = factor(Omega)
  )) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "green") +
  geom_abline(
    intercept = 0,
    linetype = "dashed",
    slope = 1,
    color = "green"
  ) +
  geom_point(data = combined_data , aes(x = arid_index_tmp, y = evap_index_tmp))


# estimating w (Omega) of the the points
estimated_omega_vec <- c()

for (i in 1:length(combined_data$arid_index_tmp)) {
  # objective function
  mae = function(omega) {
    A <- combined_data[i,]$arid_index_tmp
    E <- combined_data[i,]$evap_index_tmp
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
entropy_data <- 0

for (i in 1:length(freq_tbl)) {
  if (freq_tbl[i] == 0) {
    entrpy_tmp <- 0
  } else{
    entrpy_tmp <- (freq_tbl[i] / 100) * log2(freq_tbl[i] / 100) * -1
  }
  entropy_data <- entropy_data + entrpy_tmp
}
entropy_data





