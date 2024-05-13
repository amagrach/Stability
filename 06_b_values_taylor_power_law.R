taylor.law <- function(metacomm_tsdata) {
  # Function to calculate the exponent b in Taylor's power law for each level within a metacommunity
  
  # Initialize an empty vector to store b values
  b_values <- numeric()
  
  # Calculate abundance sums across different dimensions
  ts_metacom <- apply(metacomm_tsdata, 2, sum)
  ts_patch <- apply(metacomm_tsdata, c(2, 3), sum)
  ts_species <- apply(metacomm_tsdata, c(1, 2), sum)
  
  # Log-transform the abundance data
  log_ts_metacom <- log(ts_metacom)
  log_ts_patch <- log(ts_patch)
  log_ts_species <- log(ts_species)
  
  # Remove any NA, NaN, or Inf values from the log-transformed abundance data
  log_ts_metacom <- log_ts_metacom[is.finite(log_ts_metacom)]
  log_ts_patch <- log_ts_patch[is.finite(log_ts_patch)]
  log_ts_species <- log_ts_species[is.finite(log_ts_species)]
  
  # Fit linear regression models to calculate b values
  if (length(log_ts_metacom) > 1) {
    b_values <- c(b_values, coef(lm(log_ts_metacom ~ log(seq_along(log_ts_metacom))))[2])
  }
  if (length(log_ts_patch) > 1) {
    b_values <- c(b_values, coef(lm(log_ts_patch ~ log(seq_along(log_ts_patch))))[2])
  }
  if (length(log_ts_species) > 1) {
    b_values <- c(b_values, coef(lm(log_ts_species ~ log(seq_along(log_ts_species))))[2])
  }
  
  return(b_values)
}



#obtain data from 03_Stab_metrics script

b.exp <- lapply(all.array, function (x) taylor.law(x))
b.exp


b.exp2<-as.data.frame(b.exp)
rownames(b.exp2)<-c("Region", "Community", "Population")

b.exp2<-b.exp2[,c(1,4,3,2)]
colnames(b.exp2)<-c("Flower availability", "Pollinator visitation rates",
                    "Pollinator interaction frequencies", "Fruit set")


save(b.exp2, file = "RData/b_exp_taylor.RData")

