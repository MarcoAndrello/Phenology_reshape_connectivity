rm(list=ls())

library(tidyverse)
library(sf)

# Functions
source("Functions - Map.R")
check.NA.temp <- function(x) {
  x %>% filter(decade == levels(x$decade)[1]) %>% pull(temp) %>% is.nan %>% all -> check.1
  x %>% filter(decade == levels(x$decade)[1]) %>% pull(temp) %>% is.na %>% all -> check.2
  x %>% filter(decade == levels(x$decade)[2]) %>% pull(temp) %>% is.nan %>% all -> check.3
  x %>% filter(decade == levels(x$decade)[2]) %>% pull(temp) %>% is.na %>% all -> check.4
  return(any(check.1, check.2, check.3, check.4))
}

# Load id of zero productive sites
load("zero_prod_to_exclude.RData")

# Need this for temperature
init_pos <- st_read("../initial_positions_100_0-50m.shp")
init_pos$i_pol -> init_pol

v.species <- c("Diplodus_sargus","Diplodus_vulgaris","Paracentrotus_lividus")
i.species <- 1

# for (i.species in 1 : 3) {
species <- v.species[i.species]

load(paste0("spawning_dataset_",v.species[i.species],".RData"))

################################################################################
# TEMPERATURE IN ADULT SITES
################################################################################
# Present temperature
load("temperature_dataset_Diplodus_sargus.RData")
mean_present_adult_temp <- mean_future_adult_temp <- list()
i.year <- 1
for(i.year in 1 : 10) {
  mean_present_adult_temp[[i.year]] <- data.frame(
    ID.polygon = factor(c(1:1325), levels=c(1:1325)),
    temp = apply(temperature[,,i.year,1],1,mean),
    year = i.year,
    decade = "present"
  )
}
mean_present_adult_temp %>% bind_rows -> mean_present_adult_temp_data
for(i.year in 1 : 10) {
  mean_future_adult_temp[[i.year]] <- data.frame(
    ID.polygon = factor(c(1:1325), levels=c(1:1325)),
    temp = apply(temperature[,,i.year,2],1,mean),
    year = i.year,
    decade = "future"
  )
}
mean_future_adult_temp %>% bind_rows -> mean_future_adult_temp_data

rbind(mean_present_adult_temp_data, mean_future_adult_temp_data) %>% 
  filter(!(ID.polygon %in% zero_prod_to_exclude)) %>% 
  mutate(decade=factor(decade, levels =c("present","future"))) -> data_yearly_long

# calculate p-value of the comparison
## Define pvalue
pvalue <- rep(NA,length(levels(data_yearly_long$ID.polygon))) # 1325 original polygons (will skip the missing ones)
# Loop on polygons
i.pol <- 1
for (i.pol in 1 : length(pvalue)) {
  if (i.pol %% 100 == 0) cat(i.pol,"\n"); flush.console()
  dat2 <- filter(data_yearly_long, ID.polygon==i.pol)
  if (dim(dat2)[1] == 0) next
  if (check.NA.temp(dat2)) next
  mod <- lm(temp ~ decade,dat2, na.action = na.omit)
  pvalue[i.pol] <- summary(mod)$coefficients[2,4]
}
pvalue <- as.data.frame(pvalue)
names(pvalue) <- c("pvalue")
pvalue$ID.polygon <- factor(levels(data_yearly_long$ID.polygon))


# Create data decadal, for plotting Figure
data_yearly_long %>% group_by(ID.polygon) %>% filter(decade == "present") %>% summarise(present = mean(temp, na.rm=T)) -> mean_adult_temp_present
data_yearly_long %>% group_by(ID.polygon) %>% filter(decade == "future") %>% summarise(future = mean(temp, na.rm=T)) -> mean_adult_temp_future
mean_adult_temp_present %>% left_join(mean_adult_temp_future) %>% #differences
  mutate(diff = future - present) %>%
  # pvalues
  left_join(pvalue) -> data_decadal_adult_temperature
save(data_decadal_adult_temperature,file="Data_figure_adult_temperature.RData")

# Stats
data_decadal_adult_temperature$present %>% quantile(c(0.025,0.975))
data_decadal_adult_temperature$future %>% quantile(c(0.025,0.975))
data_decadal_adult_temperature$diff %>% quantile(c(0.025,0.975))


################################################################################
# TEMPERATURE ON LARVAL TRAJECTORIES
################################################################################
# Present temperature
mean_present_larval_temp <- list()
v.year <- c(2020:2029)
i.year <- 1
for(i.year in 1 : 10) {
  load(paste0("D:/Mean_temperature_on_larval_dispersal/Mean_temp_",species,"_",v.year[i.year],".RData"))
  mean_temp_rday <- list()
  r_day <- 1
  for (r_day in 1 : length(mean_temp)){
    if(r_day %% 50 == 0) cat(r_day,"\n"); flush.console()
    # for this r_day: make data.frame saying if the site has spawned
    data.frame(ID.polygon = as.numeric(rownames(spawn)),
               has.spawned = spawn[, r_day, i.year, 1]) -> x # 1: releases of the 2020 decade
    # for each drifter, determine if its release site has spawned (joining by ID.polygon)
    data.frame(ID.polygon=init_pos$i_pol,
               temp = mean_temp[[r_day]]) %>%
      left_join(x, by = join_by(ID.polygon)) %>%
      # retain only drifters that were spawned, group them by site and calculate their mean PLD 
      filter(has.spawned) %>% 
      group_by(ID.polygon) %>% 
      summarise(mean_temp = mean(temp,na.rm=T))  ->
      mean_temp_rday[[r_day]]
  }
  mean_temp_rday %>% bind_rows %>%
    group_by(ID.polygon) %>%
    summarise(mean_temp = mean(mean_temp, na.rm=T)) %>%
    mutate(year = i.year) ->
    mean_present_larval_temp[[i.year]]
}
mean_present_larval_temp %>% bind_rows %>% mutate(ID.polygon = factor(ID.polygon, levels=c(1:1325))) %>% rename(mean_present_temp = mean_temp) -> mean_present_larval_temp_data

# Future temperature
mean_future_larval_temp <- list()
v.year <- c(2090:2099)
i.year <- 1
for(i.year in 1 : 10) {
  load(paste0("D:/Mean_temperature_on_larval_dispersal/Mean_temp_",species,"_",v.year[i.year],".RData"))
  mean_temp_rday <- list()
  r_day <- 1
  for (r_day in 1 : length(mean_temp)){
    if(r_day %% 50 == 0) cat(r_day,"\n"); flush.console()
    # for this r_day: make data.frame saying if the site has spawned
    data.frame(ID.polygon = as.numeric(rownames(spawn)),
               has.spawned = spawn[, r_day, i.year, 1]) -> x # 1: releases of the 2020 decade (we keep spawning days fixed)
    # for each drifter, determine if its release site has spawned (joining by ID.polygon)
    data.frame(ID.polygon=init_pos$i_pol,
               temp = mean_temp[[r_day]]) %>%
      left_join(x, by = join_by(ID.polygon)) %>%
      # retain only drifters that were spawned, group them by site and calculate their mean PLD 
      filter(has.spawned) %>% 
      group_by(ID.polygon) %>% 
      summarise(mean_temp = mean(temp,na.rm=T))  ->
      mean_temp_rday[[r_day]]
  }
  mean_temp_rday %>% bind_rows %>%
    group_by(ID.polygon) %>%
    summarise(mean_temp = mean(mean_temp, na.rm=T)) %>%
    mutate(year = i.year) ->
    mean_future_larval_temp[[i.year]]
}
mean_future_larval_temp %>% bind_rows %>% mutate(ID.polygon = factor(ID.polygon, levels=c(1:1325))) %>% rename(mean_future_temp = mean_temp) -> mean_future_larval_temp_data

# Join the two variables
mean_present_larval_temp_data %>% left_join(mean_future_larval_temp_data) %>%
  filter(!(ID.polygon %in% zero_prod_to_exclude)) -> data_yearly

# calculate p-value of the comparison
## Make data in long format
data_yearly %>% rename(present =mean_present_temp,
                       future = mean_future_temp) %>%
  pivot_longer(cols=c("present","future"),names_to = "decade", values_to = "temp") %>%
  mutate(decade = factor(decade, levels=c("present","future")))-> data_yearly_long
## Define pvalue
pvalue <- rep(NA,length(levels(data_yearly$ID.polygon))) # 1325 original polygons (will skip the missing ones)
# Loop on polygons
i.pol <- 271
for (i.pol in 1 : length(pvalue)) {
  if (i.pol %% 100 == 0) cat(i.pol,"\n"); flush.console()
  dat2 <- filter(data_yearly_long, ID.polygon==i.pol)
  if (dim(dat2)[1] == 0) next
  if (check.NA.temp(dat2)) next
  mod <- lm(temp ~ decade,dat2, na.action = na.omit)
  pvalue[i.pol] <- summary(mod)$coefficients[2,4]
}


pvalue <- as.data.frame(pvalue)
names(pvalue) <- c("pvalue_fixed")
pvalue$ID.polygon <- factor(levels(data_yearly$ID.polygon))

# Create data decadal, for plotting Figure 2
data_yearly %>% 
  group_by(ID.polygon) %>%
  # mean value per polygon  (mean decadal)
  summarise(present = mean(mean_present_temp, na.rm=T),
            future = mean(mean_future_temp, na.rm=T)) %>%
  # differences
  mutate(diff_fixed = future - present) %>%
  # pvalues
  left_join(pvalue) -> data_decadal_larval_temperature
save(data_decadal_larval_temperature,file="Data_figure_larval_temp.RData")

data_decadal_larval_temperature$present %>% quantile(c(0.025,0.975))
data_decadal_larval_temperature$future %>% quantile(c(0.025,0.975))
data_decadal_larval_temperature$diff_fixed %>% quantile(c(0.025,0.975))
data_decadal_larval_temperature$diff_fixed %>% range

################################################################################
# COMPOSE FIGURE
################################################################################
load("Data_figure_larval_temp.RData")
load("Data_figure_adult_temperature.RData")

map_adult_present <- draw_map(data_decadal_adult_temperature, "present", NA, "Present temperature in spawning sites", fill_range=c(15, 26), map_type="value", legend_title="Temperature (°C)", panel_label="a")
map_adult_future <- draw_map(data_decadal_adult_temperature, "future", NA, "Future temperature in spawning sites", c(15, 26), "value","Temperature (°C)", "b")
map_adult_difference <- draw_map(data_decadal_adult_temperature, "diff", "pvalue", "Temperature difference in spawning sites", c(1, 3.6), "difference", "Δ temperature (°C)", "c")

map_larval_present <- draw_map(data_decadal_larval_temperature, "present", NA, "Present temperature over larval trajectories", fill_range=c(15, 26), "value","Temperature (°C)", "d")
map_larval_future <- draw_map(data_decadal_larval_temperature, "future", NA, "Future temperature over larval trajectories", fill_range=c(15, 26), "value","Temperature (°C)", "e")
map_larval_difference <- draw_map(data_decadal_larval_temperature, "diff_fixed", "pvalue_fixed", "Temperature difference over larval trajectories", fill_range=c(-6, 6), "difference","Δ temperature (°C)", "f")


# ggsave("Figure PROVA2.png",
ggsave("Figure 2.pdf",
       plot= marrangeGrob(c(map_adult_present, map_adult_future, map_adult_difference,
                      map_larval_present, map_larval_future, map_larval_difference),
                    nrow=3,ncol=2,
                    top=""),
       # device="png", bg="white", width=20, height=15, units="cm", dpi=600)
       device=cairo_pdf, width=20, height=15, units="cm", dpi=600)



# Density plots
density_adult_present <- draw_density(data_decadal_adult_temperature, "present", "Present temperature in spawning sites", c(15, 26), "Temperature (°C)", "a")
density_adult_future <- draw_density(data_decadal_adult_temperature, "future", "Future temperature in spawning sites", c(15, 26), "Temperature (°C)", "b")
density_adult_difference <- draw_density(data_decadal_adult_temperature, "diff", "Temperature difference in spawning sites", c(1, 3.6), "Δ temperature (°C)", "c")

density_larval_present <- draw_density(data_decadal_larval_temperature, "present", "Present temperature over larval trajectories", c(15, 26), "Temperature (°C)", "d")
density_larval_future <- draw_density(data_decadal_larval_temperature, "future", "Future temperature over larval trajectories", c(15, 26), "Temperature (°C)", "e")
density_larval_difference <- draw_density(data_decadal_larval_temperature, "diff_fixed", "Temperature difference over larval trajectories", c(-6, 6), "Δ temperature (°C)", "f")

ggsave("Figure S2.png",
       marrangeGrob(c(density_adult_present, density_adult_future, density_adult_difference,
                      density_larval_present, density_larval_future, density_larval_difference),
                    nrow=3,ncol=2,
                    top=""),
       device=cairo_pdf, bg="white", width=20, height=15, units="cm", dpi=600)
