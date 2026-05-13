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



