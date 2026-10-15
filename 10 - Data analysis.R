# Data analysis

rm(list=ls())

library(tidyverse)
library(sf)

# Functions
source("Functions - Map.R")

# RCP scenario
scen <- 45

# Load id of zero productive sites
load("zero_prod_to_exclude.RData")

v.stat <- c("distance","tss")

# Need this for temperature
init_pos <- st_read("initial_positions/initial_positions_100_0-50m.shp")
init_pos$i_pol -> init_pol

v.species <- c("Diplodus_sargus","Diplodus_vulgaris","Paracentrotus_lividus")
i.species <- 1

species <- v.species[i.species]


################################################################################
# TEMPERATURE IN SPAWNING SITES
################################################################################
# contemporary temperature
load(paste0("output/temperature_dataset_Diplodus_sargus_RCP",scen,".RData"))
mean_contemporary_spawning_temp <- mean_future_spawning_temp <- list()
i.year <- 1
for(i.year in 1 : 10) {
  mean_contemporary_spawning_temp[[i.year]] <- data.frame(
    ID.polygon = factor(c(1:1325), levels=c(1:1325)),
    temp = apply(temperature[,,i.year,1],1,mean),
    year = i.year,
    decade = "contemporary"
  )
}
mean_contemporary_spawning_temp %>% bind_rows -> mean_contemporary_spawning_temp_data
for(i.year in 1 : 10) {
  mean_future_spawning_temp[[i.year]] <- data.frame(
    ID.polygon = factor(c(1:1325), levels=c(1:1325)),
    temp = apply(temperature[,,i.year,2],1,mean),
    year = i.year,
    decade = "future"
  )
}
mean_future_spawning_temp %>% bind_rows -> mean_future_spawning_temp_data

rbind(mean_contemporary_spawning_temp_data, mean_future_spawning_temp_data) %>% 
  filter(!(ID.polygon %in% zero_prod_to_exclude)) %>% 
  mutate(decade=factor(decade, levels =c("contemporary","future"))) -> data_yearly_long

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
data_yearly_long %>% group_by(ID.polygon) %>% filter(decade == "contemporary") %>% summarise(contemporary = mean(temp, na.rm=T)) -> mean_spawning_temp_contemporary
data_yearly_long %>% group_by(ID.polygon) %>% filter(decade == "future") %>% summarise(future = mean(temp, na.rm=T)) -> mean_spawning_temp_future
mean_spawning_temp_contemporary %>% left_join(mean_spawning_temp_future) %>% #differences
  mutate(diff = future - contemporary) %>%
  # pvalues
  left_join(pvalue) -> data_decadal_spawning_temperature
save(data_decadal_spawning_temperature,file=paste0("output/Data_figure_spawning_temperature_RCP",scen,".RData"))


################################################################################
# TEMPERATURE DURING TRANSPORT
################################################################################
load(paste0("output/spawning_dataset_",v.species[i.species],"_RCP",scen,".RData"))

# contemporary temperature
mean_contemporary_pelagic_temp <- list()
v.year <- c(2020:2029)
i.year <- 1
for(i.year in 1 : 10) {
  load(paste0("D:/Mean_temperature_on_larval_dispersal/RCP",scen,"/Mean_temp_",species,"_",v.year[i.year],".RData"))
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
      # retain only drifters that were spawned, group them by site and calculate their mean PD 
      filter(has.spawned) %>% 
      group_by(ID.polygon) %>% 
      summarise(mean_temp = mean(temp,na.rm=T))  ->
      mean_temp_rday[[r_day]]
  }
  mean_temp_rday %>% bind_rows %>%
    group_by(ID.polygon) %>%
    summarise(mean_temp = mean(mean_temp, na.rm=T)) %>%
    mutate(year = i.year) ->
    mean_contemporary_pelagic_temp[[i.year]]
}
mean_contemporary_pelagic_temp %>% bind_rows %>% mutate(ID.polygon = factor(ID.polygon, levels=c(1:1325))) %>% rename(mean_contemporary_temp = mean_temp) -> mean_contemporary_pelagic_temp_data

# Future temperature
mean_future_pelagic_temp <- list()
v.year <- c(2090:2099)
i.year <- 1
for(i.year in 1 : 10) {
  load(paste0("D:/Mean_temperature_on_larval_dispersal/RCP",scen,"/Mean_temp_",species,"_",v.year[i.year],".RData"))
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
      # retain only drifters that were spawned, group them by site and calculate their mean PD 
      filter(has.spawned) %>% 
      group_by(ID.polygon) %>% 
      summarise(mean_temp = mean(temp,na.rm=T))  ->
      mean_temp_rday[[r_day]]
  }
  mean_temp_rday %>% bind_rows %>%
    group_by(ID.polygon) %>%
    summarise(mean_temp = mean(mean_temp, na.rm=T)) %>%
    mutate(year = i.year) ->
    mean_future_pelagic_temp[[i.year]]
}
mean_future_pelagic_temp %>% bind_rows %>% mutate(ID.polygon = factor(ID.polygon, levels=c(1:1325))) %>% rename(mean_future_temp = mean_temp) -> mean_future_pelagic_temp_data

# Join the two variables
mean_contemporary_pelagic_temp_data %>% left_join(mean_future_pelagic_temp_data) %>%
  filter(!(ID.polygon %in% zero_prod_to_exclude)) -> data_yearly

# calculate p-value of the comparison
## Make data in long format
data_yearly %>% rename(contemporary =mean_contemporary_temp,
                       future = mean_future_temp) %>%
  pivot_longer(cols=c("contemporary","future"),names_to = "decade", values_to = "temp") %>%
  mutate(decade = factor(decade, levels=c("contemporary","future")))-> data_yearly_long
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
names(pvalue) <- c("pvalue_baseline")
pvalue$ID.polygon <- factor(levels(data_yearly$ID.polygon))

# Create data decadal, for plotting
data_yearly %>% 
  group_by(ID.polygon) %>%
  # mean value per polygon  (mean decadal)
  summarise(contemporary = mean(mean_contemporary_temp, na.rm=T),
            future = mean(mean_future_temp, na.rm=T)) %>%
  # differences
  mutate(diff_baseline = future - contemporary) %>%
  # pvalues
  left_join(pvalue) -> data_decadal_pelagic_temperature
save(data_decadal_pelagic_temperature,file=paste0("output/Data_figure_pelagic_temp_RCP",scen,".RData"))


###############################
# SPAWNING ONSET
###############################
# Prepare data spawning_onset
load(paste0("output/spawning_dataset_",v.species[i.species],"_RCP",scen,".RData"))
# Build dataframe with ID.polygon (factor), year, spawning_onset (cardinal day), decade
datalist <- vector(mode="list",length=1325*2*10) # 10 years only for lividus e sargus
k <- 1
for (i.pol in 1 : 1325) {
  for (i.decade in 1 : 2) {
    for (i.year in 1 : 10) {
      datalist[[k]] <- data.frame(ID.polygon = i.pol,
                                  decade = c("decade2020","decade2090")[i.decade],
                                  year = i.year,
                                  spawning_onset = match(T,spawn[i.pol,,i.year,i.decade]))
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
data_yearly_so %>% group_by(ID.polygon) %>% filter(decade == "decade2020") %>% summarise(contemporary = mean(spawning_onset, na.rm=T)) -> mean_so_contemporary
data_yearly_so %>% group_by(ID.polygon) %>% filter(decade == "decade2090") %>% summarise(future = mean(spawning_onset, na.rm=T)) -> mean_so_future
mean_so_contemporary %>% left_join(mean_so_future) %>%# differences
  mutate(diff = future - contemporary) %>%
  # pvalues
  left_join(pvalue) -> data_decadal_so
save(data_decadal_so,file=paste0("output/Data_figure_spawning_onset_RCP",scen,".RData"))



###############################
# PELAGIC DURATION (PD)
###############################
# Prepare data spawning_onset
load(paste0("output/spawning_dataset_",v.species[i.species],"_RCP",scen,".RData"))
# Contemporary PD
mean_contemporary_PD <- list()
v.year <- c(2020:2029)
i.year <- 1
for(i.year in 1 : 10) {
  load(paste0("output/Mean_PD/RCP",scen,"/Mean_PD_",species,"_",v.year[i.year],".RData"))
  mean_PD_rday <- list()
  r_day <- 1
  for (r_day in 1 : length(data_PD)){
    if(r_day %% 50 == 0) cat("Year",i.year,"Release day",r_day,"\n"); flush.console()
    # for this r_day: make data.frame saying if the site has spawned
    data.frame(ID.polygon = as.numeric(rownames(spawn)),
               has.spawned = spawn[, r_day, i.year, 1]) -> x # 1: releases of the 2020 decade
    # for each drifter, determine if its release site has spawned (joining by ID.polygon)
    left_join(data_PD[[r_day]], x, by = join_by(ID.polygon)) %>%
      # retain only drifters that were spawned, group them by site and calculate their mean PD 
      filter(has.spawned) %>% 
      group_by(ID.polygon) %>% 
      summarise(mean_PD = mean(PD,na.rm=T))  ->
      mean_PD_rday[[r_day]]
  }
  mean_PD_rday %>% bind_rows %>%
    group_by(ID.polygon) %>%
    summarise(mean_PD = mean(mean_PD, na.rm=T)) %>%
    mutate(year = i.year) ->
    mean_contemporary_PD[[i.year]]
}

# Future PD baseline phenology
mean_future_PD_baseline_release <- list()
v.year <- c(2090:2099)
i.year <- 1
for(i.year in 1 : 10) {
  load(paste0("output/Mean_PD/RCP",scen,"/Mean_PD_",species,"_",v.year[i.year],".RData"))
  mean_PD_rday <- list()
  r_day <- 1
  for (r_day in 1 : length(data_PD)){
    if(r_day %% 50 == 0) cat("Year",i.year,"Release day",r_day,"\n"); flush.console()
    # for this r_day: make data.frame saying if the site has spawned
    data.frame(ID.polygon = as.numeric(rownames(spawn)),
               has.spawned = spawn[, r_day, i.year, 1]) -> x # 1: releases of the 2020 decade
    # for each drifter, determine if its release site has spawned (joining by ID.polygon)
    left_join(data_PD[[r_day]], x, by = join_by(ID.polygon)) %>%
      # retain only drifters that were spawned, group them by site and calculate their mean PD 
      filter(has.spawned) %>% 
      group_by(ID.polygon) %>% 
      summarise(mean_PD = mean(PD,na.rm=T))  ->
      mean_PD_rday[[r_day]]
  }
  mean_PD_rday %>% bind_rows %>%
    group_by(ID.polygon) %>%
    summarise(mean_PD = mean(mean_PD, na.rm=T)) %>%
    mutate(year = i.year) ->
    mean_future_PD_baseline_release[[i.year]]
}


# Future PD climate-responsive phenology
mean_future_PD_climate_responsive_release <- list()
v.year <- c(2090:2099)
i.year <- 1
for(i.year in 1 : 10) {
  load(paste0("output/Mean_PD/RCP",scen,"/Mean_PD_",species,"_",v.year[i.year],".RData"))
  mean_PD_rday <- list()
  r_day <- 1
  for (r_day in 1 : length(data_PD)){
    if(r_day %% 50 == 0) cat("Year",i.year,"Release day",r_day,"\n"); flush.console()
    # for this r_day: make data.frame saying if the site has spawned
    data.frame(ID.polygon = as.numeric(rownames(spawn)),
               has.spawned = spawn[, r_day, i.year, 2]) -> x # 2: releases of the 2090 decade
    # for each drifter, determine if its release site has spawned (joining by ID.polygon)
    left_join(data_PD[[r_day]], x, by = join_by(ID.polygon)) %>%
      # retain only drifters that were spawned, group them by site and calculate their mean PD 
      filter(has.spawned) %>% 
      group_by(ID.polygon) %>% 
      summarise(mean_PD = mean(PD,na.rm=T))  ->
      mean_PD_rday[[r_day]]
  }
  mean_PD_rday %>% bind_rows %>%
    group_by(ID.polygon) %>%
    summarise(mean_PD = mean(mean_PD, na.rm=T)) %>%
    mutate(year = i.year) ->
    mean_future_PD_climate_responsive_release[[i.year]]
}

# Make ID.polygon a factor and rename stuff
mean_contemporary_PD %>% bind_rows %>% mutate(ID.polygon = factor(ID.polygon, levels=c(1:1325))) %>% rename(mean_contemporary_PD = mean_PD) -> mean_contemporary_PD_data
mean_future_PD_baseline_release %>% bind_rows %>% mutate(ID.polygon = factor(ID.polygon, levels=c(1:1325))) %>% rename(mean_future_PD_baseline_release = mean_PD) -> mean_future_PD_baseline_release_data
mean_future_PD_climate_responsive_release %>% bind_rows %>% mutate(ID.polygon = factor(ID.polygon, levels=c(1:1325))) %>% rename(mean_future_PD_climate_responsive_release = mean_PD) -> mean_future_PD_climate_responsive_release_data

# Join the three variables
mean_contemporary_PD_data %>% left_join(mean_future_PD_baseline_release_data) %>% left_join(mean_future_PD_climate_responsive_release_data) %>%
  filter(!(ID.polygon %in% zero_prod_to_exclude)) -> data_yearly

# calculate p-value of the comparison

## Make data in long format
data_yearly %>% rename(contemporary=mean_contemporary_PD,
                       future_baseline=mean_future_PD_baseline_release,
                       future_climate_responsive=mean_future_PD_climate_responsive_release) %>%
  pivot_longer(cols=c("contemporary","future_baseline","future_climate_responsive"),names_to = "decade", values_to = "PD") -> data_yearly_long

## Define pvalue
pvalue <- array(NA,dim=c(length(levels(data_yearly$ID.polygon)), # 1325 original polygons (will skip the missing ones)
                         2))                                     # 2 scenarios: future baseline and future climate_responsive; 
## Loop on the two scenarios and polygons
i.scen <- 1
for (i.scen in 1 : 2) {
  if(i.scen == 1) data_yearly_long %>%
    filter(decade %in% c("contemporary","future_baseline")) %>%
    mutate(decade = factor(decade, levels=c("contemporary","future_baseline"))) -> dat1
  if(i.scen == 2) data_yearly_long %>%
    filter(decade %in% c("contemporary","future_climate_responsive")) %>%
    mutate(decade = factor(decade, levels=c("contemporary","future_climate_responsive"))) -> dat1
  i.pol <- 1
  for (i.pol in 1 : dim(pvalue)[1]) {
    if (i.pol %% 100 == 0) cat(i.scen, i.pol,"\n"); flush.console()
    dat2 <- filter(dat1, ID.polygon==i.pol)
    if (dim(dat2)[1] == 0) next
    if (check.NA.PD(dat2)) next
    mod <- lm(PD ~ decade,dat2, na.action = na.omit)
    pvalue[i.pol,i.scen] <- summary(mod)$coefficients[2,4]
  }
  
}
pvalue <- as.data.frame(pvalue)
names(pvalue) <- c("future_baseline","future_climate_responsive")
pvalue$ID.polygon <- factor(levels(data_yearly$ID.polygon))

# Create data decadal PD, for plotting Figure
data_yearly %>% 
  group_by(ID.polygon) %>%
  # mean value per polygon  (mean decadal)
  summarise(contemporary = mean(mean_contemporary_PD, na.rm=T),
            future_baseline = mean(mean_future_PD_baseline_release, na.rm=T),
            future_climate_responsive = mean(mean_future_PD_climate_responsive_release, na.rm=T)) %>%
  # differences
  mutate(diff_baseline = future_baseline - contemporary,
         diff_climate_responsive = future_climate_responsive - contemporary) %>%
  # pvalues
  left_join(pvalue %>% rename(pvalue_baseline = future_baseline,
                              pvalue_climate_responsive = future_climate_responsive)) -> data_decadal_PD
save(data_decadal_PD, file=paste0("output/Data_figure_PD_RCP",scen,".RData"))


################################################################################
# CONNECTIVITY (DISPERSAL DISTANCE AND TOTAL SETTLEMENT SUCCESS)
################################################################################
mortality_rate <- 0.1

load(paste0("output/Data_connectivity_",species,"_RCP",scen,"_mu",mortality_rate*100,".RData"))

# data %>% select(ID.polygon, distance, tss, year, i.decade, scen_spawning) %>%
data %>% select(ID.polygon, distance, tss, year, i.decade, scen_spawning) %>%
  mutate(decade = case_when(i.decade == 1 & scen_spawning == 1 ~ "contemporary",
                            i.decade == 2 & scen_spawning == 1 ~ "future_baseline",
                            i.decade == 2 & scen_spawning == 2 ~ "future_climate_responsive")) -> data_yearly_long

# calculate p-value of the comparison
pvalue <- list() # on metrics/stat
## Define pvalue: two variables
pvalue[[1]] <- pvalue[[2]] <- array(NA,dim=c(length(levels(data_yearly_long$ID.polygon)), # 1313 polygons
                                                            2))                                     # 2 scenarios: future baseline and future climate_responsive; 
# Loop on the two scenarios and polygons
i.scen <- 1
for (i.scen in 1 : 2) {
  if(i.scen == 1) data_yearly_long %>%
    filter(decade %in% c("contemporary","future_baseline")) %>%
    mutate(decade = factor(decade, levels=c("contemporary","future_baseline"))) -> dat1
  if(i.scen == 2) data_yearly_long %>%
    filter(decade %in% c("contemporary","future_climate_responsive")) %>%
    mutate(decade = factor(decade, levels=c("contemporary","future_climate_responsive"))) -> dat1
  i.pol <- 200
  for (i.pol in 1 : 1313) {
    if (i.pol %% 100 == 0) cat(i.scen, i.pol,"\n"); flush.console()
    dat2 <- filter(dat1, ID.polygon==i.pol)
    if (dim(dat2)[1] == 0) next
    if (!check.NA(dat2,"distance")) {
      mod <- lm(distance ~ decade,dat2, na.action = na.omit)
      pvalue[[1]][i.pol,i.scen] <- summary(mod)$coefficients[2,4]
    }
    if (!check.NA(dat2,"tss")) {
      mod <- lm(tss ~ decade,dat2, na.action = na.omit)
      pvalue[[2]][i.pol,i.scen] <- summary(mod)$coefficients[2,4]
    }
  }
}

for (i in 1 : length(pvalue)){
  pvalue[[i]] <- as.data.frame(pvalue[[i]])
  names(pvalue[[i]]) <- c("future_baseline","future_climate_responsive")
  pvalue[[i]]$ID.polygon <- factor(levels(data_yearly_long$ID.polygon))
}
names(pvalue) <- v.stat

# Create data decadal, for plotting Figure 4
# Distance
data_yearly_long %>%
  select(-tss) %>%
  group_by(ID.polygon, decade) %>%
  # mean value per polygon  (mean decadal)
  summarise(distance = mean(distance, na.rm=T)) %>%
  pivot_wider(values_from=distance, names_from=decade) %>%
  # differences
  mutate(diff_baseline = future_baseline - contemporary,
         diff_climate_responsive = future_climate_responsive - contemporary) %>%
  # pvalues
  left_join(pvalue[[1]] %>% rename(pvalue_baseline = future_baseline,
                                   pvalue_climate_responsive = future_climate_responsive)) -> data_decadal_distance
# Total settlement success
data_yearly_long %>%
  select(-distance) %>%
  group_by(ID.polygon, decade) %>%
  summarise(tss = mean(tss, na.rm=T)) %>%
  pivot_wider(values_from=tss, names_from=decade) %>%
  mutate(diff_baseline = future_baseline - contemporary,
         diff_climate_responsive = future_climate_responsive - contemporary) %>%
  left_join(pvalue[[2]] %>% rename(pvalue_baseline = future_baseline,
                                   pvalue_climate_responsive = future_climate_responsive)) -> data_decadal_tss

data_decadal_distance %>% mutate(effect = diff_climate_responsive - diff_baseline) -> data_decadal_distance
data_decadal_tss %>% mutate(effect = diff_climate_responsive - diff_baseline) -> data_decadal_tss
save(data_decadal_distance, data_decadal_tss,
     file=paste0("output/Data_figure_connectivity_RCP",scen,"_mu",mortality_rate*100,".RData"))

