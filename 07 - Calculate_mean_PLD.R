# 07 - Calculate mean PLD

rm(list=ls())

library(tidyverse)
library(sf)

i.species <- 1
species <- c("Diplodus_sargus","Paracentrotus_lividus","Diplodus_vulgaris")[i.species]

# Info on the initial positions
init_pos <- st_read("initial_positions/initial_positions_100_0-50m.shp")

# Calculate PLD as a function of temperature for each drifter and release day
# Using the equation for PLD by O'Connor
# beta0i is the species-specific intercept predicted in Calibrate PLD-T relationship.R

if (species == "Diplodus_sargus") {
  beta0i <-  3.310
  v.year <- c(2020:2029,2090:2099)

}

for (i.year in 1 : length(v.year)) {
  cat(v.year[i.year],"\n")
  load(paste0("D:/Mean_temperature_on_larval_dispersal/Mean_temp_",species,"_",v.year[i.year],".RData"))
  data_PLD <- list(length=length(mean_temp))
  for (r_day in 1 : length(mean_temp)) {
    data.frame(ID.polygon = init_pos$i_pol,
               year = v.year[i.year],
               r_day = r_day,
               mean_temp = mean_temp[[r_day]]) %>% 
      mutate(log_PLD = beta0i + (-1.3439341)*(log(mean_temp)-log(15)) + (-0.2775613)*(log(mean_temp)-log(15))^2) %>%
      mutate(PLD = round(exp(log_PLD))) %>%
      select(ID.polygon,year,r_day,PLD) ->
      data_PLD[[r_day]]
  }
  
  save(data_PLD, file=paste0("Mean_PLD/Mean_PLD_",species,"_",v.year[i.year],".RData"))
}

