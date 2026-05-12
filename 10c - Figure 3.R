rm(list=ls())

library(tidyverse)
library(sf)

# Functions
source("Functions - Map.R")
check.NA.PLD <- function(x) {
  x %>% filter(decade == levels(x$decade)[1]) %>% pull(PLD) %>% is.nan %>% all -> check.1
  x %>% filter(decade == levels(x$decade)[1]) %>% pull(PLD) %>% is.na %>% all -> check.2
  x %>% filter(decade == levels(x$decade)[2]) %>% pull(PLD) %>% is.nan %>% all -> check.3
  x %>% filter(decade == levels(x$decade)[2]) %>% pull(PLD) %>% is.na %>% all -> check.4
  return(any(check.1, check.2, check.3, check.4))
}
check.NA.so <- function(x) {
  x %>% filter(decade == levels(x$decade)[1]) %>% pull(spawning_onset) %>% is.nan %>% all -> check.1
  x %>% filter(decade == levels(x$decade)[1]) %>% pull(spawning_onset) %>% is.na %>% all -> check.2
  x %>% filter(decade == levels(x$decade)[2]) %>% pull(spawning_onset) %>% is.nan %>% all -> check.3
  x %>% filter(decade == levels(x$decade)[2]) %>% pull(spawning_onset) %>% is.na %>% all -> check.4
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


###############################
# SPAWNING PHENOLOGY
###############################
# Prepare data spawning_onset
load(paste0("spawning_dataset_",v.species[i.species],".RData"))
# Build dataframe with ID.polygon (factor), year, spawning_onset (cardinal day), decade
#if(nrow(spawn)==1325) spawn[-zero_prod_to_exclude,,,] -> spawn
datalist <- vector(mode="list",length=1325*2*10) # dieci anni solo per lividus e sargus
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

# Stats
data_decadal_so$present %>% quantile(c(0.025, 0.5, 0.975))
data_decadal_so$future %>% quantile(c(0.025, 0.5, 0.975))
data_decadal_so$diff %>% quantile(c(0.025, 0.5, 0.975))


###############################
# CHANGE IN PLD
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

# Stats
data_decadal_PLD$diff_fixed %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_PLD$diff_variable %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
# m <- lm(diff_variable ~ diff_fixed, data_decadal_PLD)
# summary(m)
# data_decadal$buffering <- data_decadal$diff_variable - data_decadal$diff_fixed
# summary(data_decadal)


################################################################################
# COMPOSE FIGURE
################################################################################

load("Data_figure_spawning_onset.RData")
load("Data_figure_PLD.RData")

map_so_present <- draw_map(data_decadal_so, "present", NA, "Present spawning onset", fill_range=c(60, 166), map_type="value", legend_title="Spawning onset\n(day of year)", panel_label="a")
map_so_future <- draw_map(data_decadal_so, "future", NA, "Future spawning onset", c(60, 166), "value","Spawning onset\n(day of year)", "b")
map_so_difference <- draw_map(data_decadal_so, "diff", "pvalue", "Difference in spawning onset", c(-75, 75), "difference", "Δ spawning onset\n(days)", "c")

map_PLD_fixed <- draw_map(data_decadal_PLD, "diff_fixed", "pvalue_fixed", "Fixed spawning", fill_range=c(-5, 5), "difference", legend_title="Δ PLD\n(days)", panel_label="d")
map_PLD_variable <- draw_map(data_decadal_PLD, "diff_variable", "pvalue_variable", "Variable spawning", fill_range=c(-5, 5), "difference","Δ PLD\n(days)", "e")
biplot <- ggplot(data_decadal_PLD) +
  geom_point(aes(x=diff_fixed, y=diff_variable, col=diff_variable), size=0.1, alpha = 0.6) +
  scale_color_continuous_c4a_div("brewer.pu_or",
                                 limits=c(-5, 5)) +
  xlab("Δ PLD (fixed spawning)") + ylab("Δ PLD (variable spawning)") +
  geom_hline(yintercept=0,linewidth=0.1) +
  geom_vline(xintercept=0,linewidth=0.1) +
  geom_abline(intercept=0, slope=1, linetype="dashed",linewidth=0.1) +
  guides(color="none", fill="none") +
  theme_biplot() +
  annotate("text", x = -9.5, y = Inf, label = "f", hjust = 0, vjust = 1.5, size = 4) +  
  coord_cartesian(xlim = c(-7, 2.6), clip = "off")


# ggsave("Figure PROVA3.png",
ggsave("Figure 3.pdf",
       marrangeGrob(c(map_so_present, map_so_future, map_so_difference, map_PLD_fixed, map_PLD_variable, biplot),
                    nrow=3,ncol=2,
                    top=""),
       device=cairo_pdf, width=20, height=15, units="cm", dpi=600)



# Density plots
density_so_present <- draw_density(data_decadal_so, "present", "Present spawning onset", c(60, 166), "Spawning onset (day of year)", "a")
density_so_future <- draw_density(data_decadal_so, "future", "Future spawning onset", c(60, 166), "Spawning onset (day of year)", "b")
density_so_difference <- draw_density(data_decadal_so, "diff", "Difference in spawning onset", c(-75, 75), "Δ spawning onset (days)", "c")

density_PLD_fixed <- draw_density(data_decadal_PLD, "diff_fixed", "Fixed spawning", c(-6, 4), "Δ PLD (days)", "d")
density_PLD_variable <- draw_density(data_decadal_PLD, "diff_variable", "Variable spawning", c(-6, 4), "Δ PLD (days)", "e")


ggsave("Figure S3.png",
       marrangeGrob(c(density_so_present, density_so_future, density_so_difference,
                      density_PLD_fixed, density_PLD_variable),
                    nrow=3,ncol=2,
                    top=""),
       device="png", bg="white", width=20, height=15, units="cm", dpi=600)
