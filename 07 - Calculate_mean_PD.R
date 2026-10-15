# 07 - Calculate mean PD

rm(list=ls())

library(tidyverse)
library(sf)

i.species <- 1
species <- c("Diplodus_sargus","Paracentrotus_lividus","Diplodus_vulgaris")[i.species]

scen <- 85

# Info on the initial positions
init_pos <- st_read("initial_positions/initial_positions_100_0-50m.shp")

# Calculate PD as a function of temperature for each drifter and release day
# Using the equation for PD by O'Connor
# beta0i is the species-specific intercept predicted in Calibrate PD-T relationship.R

if (species == "Diplodus_sargus") {
  beta0i <-  3.009
  v.year <- c(2020:2029,2090:2099)

}
i.year <- 1
for (i.year in 1 : length(v.year)) {
  cat(v.year[i.year],"\n")
  load(paste0("D:/Mean_temperature_on_larval_dispersal/RCP",scen,"/Mean_temp_",species,"_",v.year[i.year],".RData"))
  data_PD <- list(length=length(mean_temp))
  for (r_day in 1 : length(mean_temp)) {
    data.frame(ID.polygon = init_pos$i_pol,
               year = v.year[i.year],
               r_day = r_day,
               mean_temp = mean_temp[[r_day]]) %>% 
      mutate(log_PD = beta0i + (-1.3439341)*(log(mean_temp)-log(15)) + (-0.2775613)*(log(mean_temp)-log(15))^2) %>%
      mutate(PD = round(exp(log_PD))) %>%
      select(ID.polygon,year,r_day,PD) ->
      data_PD[[r_day]]
  }
  
  save(data_PD, file=paste0("output/Mean_PD/RCP",scen,"/Mean_PD_",species,"_",v.year[i.year],".RData"))
}

