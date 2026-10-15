# 06 - Calibrate PD-T relationship.R
rm(list=ls())

library(tidyverse)
library(terra)

# Predicting the beta0i intercept of the OConnor et al 2007 relationship
# OConnor et al 2007 Eq. S11:
b0i.func <- function(x) log(x[2]) - (-1.3439341)*(log(x[1])-log(15)) - (-0.2775613)*(log(x[1])-log(15))^2
# The argument x is a vector with two elements
# x[1]: temperature
# x[2]: PLD
## Variances of the mixed model of O'Connor et al (from their Table S2)
## Needed to calculate the weight wi below
0.75299956 -> tau2
0.02296691 -> s2
# ## Testing the function with the example from O'Connor et al SI ll. 160
# b0i.func(c(10,3.83))
# # It works

# Diplodus sargus
# Using values from Di Franco et al (2011) and Di Franco & Guidetti (2011), as reported in Di Stefano et al 2022
sampling_locations <- vect("sites/sampling_locations.shp")
# Create a 50 km buffer around each sampling point
sampling_locations %>% buffer(50000) -> sampling_locations_buffer
# Extract mean temperature for each sampling point
T_mean <- rep(NA,nrow(sampling_locations_buffer))
for (i.point in 1: nrow(sampling_locations_buffer)) {
  # Open raster of temperatures for that sampling point
  rast_temp <- rast(paste0("sites/",sampling_locations_buffer$temp_raste[i.point]))
  # Keep only dates between start_date and end_date
  idx_time <- which(as.Date(time(rast_temp)) >= as.Date(sampling_locations_buffer$start_date[i.point]) &
                      as.Date(time(rast_temp)) <= as.Date(sampling_locations_buffer$end_date[i.point]))
  rast_sel <- rast_temp[[idx_time]]
  # Extract the values in the buffer and average them, for each date and depth
  mean_layer <- extract(rast_sel, sampling_locations_buffer[i.point], fun = mean, na.rm = TRUE, exact = TRUE)
  # Average over dates and depths
  T_mean[i.point] <- mean(unlist(mean_layer[, -1]), na.rm = TRUE)
}
sampling_locations$T_mean <- T_mean

# We calculate b0 with OLS for each sampling point
# We add 3 days to PLD (time from spawning to hatching)
b0.OLS_vec <- vector()
for (i.point in 1 : nrow(sampling_locations)) {
  b0.OLS_vec[i.point] <- b0i.func(c(sampling_locations$T_mean[i.point], sampling_locations$PLD[i.point]+3))
}
# We average  over sampling points
b0.OLS <- mean(b0.OLS_vec)
# gives 3.008423

#Now we combine this OLS estimator of beta0i with the average intercept of the multilevel model
n_i <- nrow(sampling_locations)
wi<-n_i*tau2/(n_i*tau2+s2) # 11 observations available
wi*b0.OLS+(1-wi)*3.1671863
# gives 3.009

