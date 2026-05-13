# Statistical analysis

rm(list=ls())

library(tidyverse)
library(sf)

# Functions
source("00 - Functions.R")

# Load id of zero productive sites
load("zero_prod_to_exclude.RData")

# Need this for temperature
init_pos <- st_read("initial_positions/initial_positions_100_0-50m.shp")
init_pos$i_pol -> init_pol

v.species <- c("Diplodus_sargus","Diplodus_vulgaris","Paracentrotus_lividus")
i.species <- 1

# for (i.species in 1 : 3) {
species <- v.species[i.species]

load(paste0("spawning_dataset_",v.species[i.species],".RData"))
rm(number_release_days, first_day)

v.stat <- c("distance","coastal_retention")

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
rm(temperature, data_yearly_long, mod, pvalue)
rm(mean_present_adult_temp, mean_present_adult_temp_data, mean_future_adult_temp, mean_future_adult_temp_data,
   mean_adult_temp_present, mean_adult_temp_future)

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
rm(mean_temp, mean_temp_rday, dat2, mod, pvalue, r_day)
rm(mean_present_larval_temp, mean_present_larval_temp_data, mean_future_larval_temp, mean_future_larval_temp_data)

data_decadal_larval_temperature$present %>% quantile(c(0.025,0.975))
data_decadal_larval_temperature$future %>% quantile(c(0.025,0.975))
data_decadal_larval_temperature$diff_fixed %>% quantile(c(0.025,0.975))
data_decadal_larval_temperature$diff_fixed %>% range

###############################
# SPAWNING ONSET
###############################
# Prepare data spawning_onset
# Build dataframe with ID.polygon (factor), year, spawning_onset (cardinal day), decade
#if(nrow(spawn)==1325) spawn[-zero_prod_to_exclude,,,] -> spawn
datalist <- vector(mode="list",length=1325*2*10) # change 10 to 9 for vulgaris
k <- 1
for (i.pol in 1 : 1325) {
  for (i.decade in 1 : 2) {
    for (i.year in 1 : 10) {
      datalist[[k]] <- data.frame(ID.polygon = i.pol,
                                  decade = c("decade2020","decade2090")[i.decade],
                                  year = i.year,
                                  spawning_onset = match(T,spawn[i.pol,,i.year,i.decade])+c(59,273,59)[i.species])
      k <- k + 1
    }
  }
}
bind_rows(datalist) %>%
  mutate(ID.polygon = factor(ID.polygon), decade = factor(decade)) -> data_yearly_so
rm(datalist)

# calculate p-value of the comparison
## Make data in long format
## Define pvalue
pvalue <- rep(NA,length(levels(data_yearly_so$ID.polygon)))
# Loop on polygons
i.pol <- 1
for (i.pol in 1 : length(pvalue)) {
  if (i.pol %% 100 == 0) cat(i.pol,"\n"); flush.console()
  dat2 <- filter(data_yearly_so, ID.polygon==i.pol)
  if (dim(dat2)[1] == 0) next
  if (check.NA.so(dat2)) next
  mod <- lm(spawning_onset ~ decade,dat2, na.action = na.omit)
  pvalue[i.pol] <- summary(mod)$coefficients[2,4]
}

pvalue <- as.data.frame(pvalue)
names(pvalue) <- c("pvalue")
pvalue$ID.polygon <- factor(levels(data_yearly_so$ID.polygon))

# Create data decadal, for plotting Figure
data_yearly_so %>% group_by(ID.polygon) %>% filter(decade == "decade2020") %>% summarise(present = mean(spawning_onset, na.rm=T)) -> mean_so_present
data_yearly_so %>% group_by(ID.polygon) %>% filter(decade == "decade2090") %>% summarise(future = mean(spawning_onset, na.rm=T)) -> mean_so_future
mean_so_present %>% left_join(mean_so_future) %>%# differences
  mutate(diff = future - present) %>%
  # pvalues
  left_join(pvalue) -> data_decadal_so
save(data_decadal_so,file="Data_figure_spawning_onset.RData")
rm(data_yearly_so, dat2, mod, pvalue)
rm(mean_so_present, mean_so_future)


# Stats
data_decadal_so$present %>% quantile(c(0.025, 0.5, 0.975))
data_decadal_so$future %>% quantile(c(0.025, 0.5, 0.975))
data_decadal_so$diff %>% quantile(c(0.025, 0.5, 0.975))


###############################
# PLD
###############################

# Present PLD
mean_present_PLD <- list()
v.year <- c(2020:2029)
i.year <- 1
for(i.year in 1 : 10) {
  load(paste0("Mean_PLD/Mean_PLD_",species,"_",v.year[i.year],".RData"))
  mean_PLD_rday <- list()
  r_day <- 1
  for (r_day in 1 : length(data_PLD)){
    if(r_day %% 50 == 0) cat("Year",i.year,"Release day",r_day,"\n"); flush.console()
    # for this r_day: make data.frame saying if the site has spawned
    data.frame(ID.polygon = as.numeric(rownames(spawn)),
               has.spawned = spawn[, r_day, i.year, 1]) -> x # 1: releases of the 2020 decade
    # for each drifter, determine if its release site has spawned (joining by ID.polygon)
    left_join(data_PLD[[r_day]], x, by = join_by(ID.polygon)) %>%
      # retain only drifters that were spawned, group them by site and calculate their mean PLD 
      filter(has.spawned) %>% 
      group_by(ID.polygon) %>% 
      summarise(mean_PLD = mean(PLD,na.rm=T))  ->
      mean_PLD_rday[[r_day]]
  }
  mean_PLD_rday %>% bind_rows %>%
    group_by(ID.polygon) %>%
    summarise(mean_PLD = mean(mean_PLD, na.rm=T)) %>%
    mutate(year = i.year) ->
    mean_present_PLD[[i.year]]
}


# Future PLD fixed release
mean_future_PLD_fixed_release <- list()
v.year <- c(2090:2099)
i.year <- 1
for(i.year in 1 : 10) {
  load(paste0("Mean_PLD/Mean_PLD_",species,"_",v.year[i.year],".RData"))
  mean_PLD_rday <- list()
  r_day <- 1
  for (r_day in 1 : length(data_PLD)){
    if(r_day %% 50 == 0) cat("Year",i.year,"Release day",r_day,"\n"); flush.console()
    # for this r_day: make data.frame saying if the site has spawned
    data.frame(ID.polygon = as.numeric(rownames(spawn)),
               has.spawned = spawn[, r_day, i.year, 1]) -> x # 1: releases of the 2020 decade
    # for each drifter, determine if its release site has spawned (joining by ID.polygon)
    left_join(data_PLD[[r_day]], x, by = join_by(ID.polygon)) %>%
      # retain only drifters that were spawned, group them by site and calculate their mean PLD 
      filter(has.spawned) %>% 
      group_by(ID.polygon) %>% 
      summarise(mean_PLD = mean(PLD,na.rm=T))  ->
      mean_PLD_rday[[r_day]]
  }
  mean_PLD_rday %>% bind_rows %>%
    group_by(ID.polygon) %>%
    summarise(mean_PLD = mean(mean_PLD, na.rm=T)) %>%
    mutate(year = i.year) ->
    mean_future_PLD_fixed_release[[i.year]]
}


# Future PLD variable release
mean_future_PLD_variable_release <- list()
v.year <- c(2090:2099)
i.year <- 1
for(i.year in 1 : 10) {
  load(paste0("Mean_PLD/Mean_PLD_",species,"_",v.year[i.year],".RData"))
  mean_PLD_rday <- list()
  r_day <- 1
  for (r_day in 1 : length(data_PLD)){
    if(r_day %% 50 == 0) cat("Year",i.year,"Release day",r_day,"\n"); flush.console()
    # for this r_day: make data.frame saying if the site has spawned
    data.frame(ID.polygon = as.numeric(rownames(spawn)),
               has.spawned = spawn[, r_day, i.year, 2]) -> x # 2: releases of the 2090 decade
    # for each drifter, determine if its release site has spawned (joining by ID.polygon)
    left_join(data_PLD[[r_day]], x, by = join_by(ID.polygon)) %>%
      # retain only drifters that were spawned, group them by site and calculate their mean PLD 
      filter(has.spawned) %>% 
      group_by(ID.polygon) %>% 
      summarise(mean_PLD = mean(PLD,na.rm=T))  ->
      mean_PLD_rday[[r_day]]
  }
  mean_PLD_rday %>% bind_rows %>%
    group_by(ID.polygon) %>%
    summarise(mean_PLD = mean(mean_PLD, na.rm=T)) %>%
    mutate(year = i.year) ->
    mean_future_PLD_variable_release[[i.year]]
}


# Make ID.polygon a factor and rename stuff
mean_present_PLD %>% bind_rows %>% mutate(ID.polygon = factor(ID.polygon, levels=c(1:1325))) %>% rename(mean_present_PLD = mean_PLD) -> mean_present_PLD_data
mean_future_PLD_fixed_release %>% bind_rows %>% mutate(ID.polygon = factor(ID.polygon, levels=c(1:1325))) %>% rename(mean_future_PLD_fixed_release = mean_PLD) -> mean_future_PLD_fixed_release_data
mean_future_PLD_variable_release %>% bind_rows %>% mutate(ID.polygon = factor(ID.polygon, levels=c(1:1325))) %>% rename(mean_future_PLD_variable_release = mean_PLD) -> mean_future_PLD_variable_release_data

# Join the three variables
mean_present_PLD_data %>% left_join(mean_future_PLD_fixed_release_data) %>% left_join(mean_future_PLD_variable_release_data) %>%
  filter(!(ID.polygon %in% zero_prod_to_exclude)) -> data_yearly

# calculate p-value of the comparison

## Make data in long format
data_yearly %>% rename(present=mean_present_PLD,
                       future_fixed=mean_future_PLD_fixed_release,
                       future_variable=mean_future_PLD_variable_release) %>%
  pivot_longer(cols=c("present","future_fixed","future_variable"),names_to = "decade", values_to = "PLD") -> data_yearly_long

## Define pvalue
pvalue <- array(NA,dim=c(length(levels(data_yearly$ID.polygon)), # 1325 original polygons (will skip the missing ones)
                         2))                                     # 2 scenarios: future fixed and future variable; 
## Loop on the two scenarios and polygons
i.scen <- 1
for (i.scen in 1 : 2) {
  if(i.scen == 1) data_yearly_long %>%
    filter(decade %in% c("present","future_fixed")) %>%
    mutate(decade = factor(decade, levels=c("present","future_fixed"))) -> dat1
  if(i.scen == 2) data_yearly_long %>%
    filter(decade %in% c("present","future_variable")) %>%
    mutate(decade = factor(decade, levels=c("present","future_variable"))) -> dat1
  i.pol <- 1
  for (i.pol in 1 : dim(pvalue)[1]) {
    if (i.pol %% 100 == 0) cat(i.scen, i.pol,"\n"); flush.console()
    dat2 <- filter(dat1, ID.polygon==i.pol)
    if (dim(dat2)[1] == 0) next
    if (check.NA.PLD(dat2)) next
    mod <- lm(PLD ~ decade,dat2, na.action = na.omit)
    pvalue[i.pol,i.scen] <- summary(mod)$coefficients[2,4]
  }
  
}
pvalue <- as.data.frame(pvalue)
names(pvalue) <- c("future_fixed","future_variable")
pvalue$ID.polygon <- factor(levels(data_yearly$ID.polygon))

# Create data decadal PLD, for plotting Figure
data_yearly %>% 
  group_by(ID.polygon) %>%
  # mean value per polygon  (mean decadal)
  summarise(present = mean(mean_present_PLD, na.rm=T),
            future_fixed = mean(mean_future_PLD_fixed_release, na.rm=T),
            future_variable = mean(mean_future_PLD_variable_release, na.rm=T)) %>%
  # differences
  mutate(diff_fixed = future_fixed - present,
         diff_variable = future_variable - present) %>%
  # pvalues
  left_join(pvalue %>% rename(pvalue_fixed = future_fixed,
                              pvalue_variable = future_variable)) -> data_decadal_PLD
save(data_decadal_PLD, file="Data_figure_PLD.RData")
rm(data_PLD, x, mean_PLD_rday, data_yearly, data_yearly_long, dat1, dat2, mod, pvalue)
rm(mean_present_PLD, mean_present_PLD_data,
   mean_future_PLD_fixed_release, mean_future_PLD_fixed_release_data, mean_future_PLD_variable_release, mean_future_PLD_variable_release_data)

# Stats
data_decadal_PLD$diff_fixed %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_PLD$diff_variable %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
# m <- lm(diff_variable ~ diff_fixed, data_decadal_PLD)
# summary(m)
# data_decadal$buffering <- data_decadal$diff_variable - data_decadal$diff_fixed
# summary(data_decadal)


################################################################################
# CONNECTIVITY (DISPERSAL DISTANCE AND TOTAL SETTLEMENT SUCCESS)
################################################################################

load(paste0("data_connectivity_",species,"_mu01.RData"))

data %>% select(ID.polygon, distance, coastal_retention, year, i.decade, scen_spawning) %>%
  mutate(decade = case_when(i.decade == 1 & scen_spawning == 1 ~ "present",
                            i.decade == 2 & scen_spawning == 1 ~ "future_fixed",
                            i.decade == 2 & scen_spawning == 2 ~ "future_variable")) -> data_yearly_long

# calculate p-value of the comparison
pvalue <- list() # on metrics/stat
## Define pvalue
pvalue[[1]] <- pvalue[[2]] <- array(NA,dim=c(length(levels(data_yearly_long$ID.polygon)), # 1313 polygons
                                             2))                                     # 2 scenarios: future fixed and future variable; 
# Loop on the two scenarios and polygons
i.scen <- 1
for (i.scen in 1 : 2) {
  if(i.scen == 1) data_yearly_long %>%
    filter(decade %in% c("present","future_fixed")) %>%
    mutate(decade = factor(decade, levels=c("present","future_fixed"))) -> dat1
  if(i.scen == 2) data_yearly_long %>%
    filter(decade %in% c("present","future_variable")) %>%
    mutate(decade = factor(decade, levels=c("present","future_variable"))) -> dat1
  i.pol <- 268
  for (i.pol in 1 : 1313) {
    if (i.pol %% 100 == 0) cat(i.scen, i.pol,"\n"); flush.console()
    dat2 <- filter(dat1, ID.polygon==i.pol)
    if (dim(dat2)[1] == 0) next
    if (!check.NA(dat2,"distance")) {
      mod <- lm(distance ~ decade,dat2, na.action = na.omit)
      pvalue[[1]][i.pol,i.scen] <- summary(mod)$coefficients[2,4]
    }
    if (!check.NA(dat2,"coastal_retention")) {
      mod <- lm(coastal_retention ~ decade,dat2, na.action = na.omit)
      pvalue[[2]][i.pol,i.scen] <- summary(mod)$coefficients[2,4]
    }
    
  }
}

for (i in 1 : length(pvalue)){
  pvalue[[i]] <- as.data.frame(pvalue[[i]])
  names(pvalue[[i]]) <- c("future_fixed","future_variable")
  pvalue[[i]]$ID.polygon <- factor(levels(data_yearly_long$ID.polygon))
}
names(pvalue) <- v.stat

# Create data decadal, for plotting Figure 4
# Distance
data_yearly_long %>%
  select(-coastal_retention) %>%
  group_by(ID.polygon, decade) %>%
  # mean value per polygon  (mean decadal)
  summarise(distance = mean(distance, na.rm=T)) %>%
  pivot_wider(values_from=distance, names_from=decade) %>%
  # differences
  mutate(diff_fixed = future_fixed - present,
         diff_variable = future_variable - present) %>%
  # pvalues
  left_join(pvalue[[1]] %>% rename(pvalue_fixed = future_fixed,
                                   pvalue_variable = future_variable)) -> data_decadal_distance
# Coastal retention
data_yearly_long %>%
  select(-distance) %>%
  group_by(ID.polygon, decade) %>%
  summarise(coastal_retention = mean(coastal_retention, na.rm=T)) %>%
  pivot_wider(values_from=coastal_retention, names_from=decade) %>%
  mutate(diff_fixed = future_fixed - present,
         diff_variable = future_variable - present) %>%
  left_join(pvalue[[2]] %>% rename(pvalue_fixed = future_fixed,
                                   pvalue_variable = future_variable)) -> data_decadal_coastal_retention

data_decadal_distance %>% mutate(effect = diff_variable - diff_fixed) -> data_decadal_distance
data_decadal_coastal_retention %>% mutate(effect = diff_variable - diff_fixed) -> data_decadal_coastal_retention
save(data_decadal_distance, data_decadal_coastal_retention, file="Data_figure_connectivity.RData")
rm(data, dat1, dat2, mod, pvalue, data_yearly_long)


# Stats
data_decadal_distance$diff_fixed %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
# data_decadal_distance$diff_fixed %>% range(na.rm=T)#quantile(c(0.01, 0.5, 0.99),na.rm=T)
data_decadal_distance$diff_variable %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_distance$effect %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_coastal_retention$diff_fixed %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
# data_decadal_coastal_retention$diff_fixed %>% quantile(c(0.01, 0.5, 0.99),na.rm=T)
data_decadal_coastal_retention$diff_variable %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_coastal_retention$diff_variable %>% quantile(c(0.01, 0.5, 0.99),na.rm=T)
data_decadal_coastal_retention$effect %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_coastal_retention$effect %>% quantile(c(0.001, 0.5, 0.99),na.rm=T)

# Present values
data_decadal_distance$present %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_distance$future_fixed %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
# data_decadal_distance$future_fixed %>% quantile(c(0.01, 0.5, 0.99),na.rm=T)
data_decadal_distance$future_variable %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_coastal_retention$present %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_coastal_retention$future_fixed %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_coastal_retention$future_variable %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
