# 05 - Calculate spawning dates

rm(list=ls())

library(tidyverse)
library(sf)


################################################################################
# Read files
################################################################################

# Read polygon layer
pol <- st_read("sites/sites.shp")


################################################################################
# CALCULATE SPAWNING DATES
################################################################################

# Read temperature
load("temperature_dataset_Diplodus_sargus.RData")

# Onset of spawning: when mean temperature is >17 degrees over 6 days after March 1st / February 1st
# Duration of spawning: 3 months

spawn <- array(F,dim=c(1325,365,10,2))
dimnames(spawn) <- dimnames(temperature)

first_day <- last_day <- number_release_days <- array(NA,c(nrow(pol),10,2))
i.decade <- 2
for (i.decade in 1 : 2) {
    i.year <- 1
    for (i.year in 1 : 10) {
        cat(i.decade, i.year,"\n"); flush.console()
        i.pol <- 1
        for (i.pol in 1 : 1325) {
            t_med <- vector()
            for (i in 1 : 123) t_med[i] <- sum(temperature[i.pol,i:(i+5),i.year,i.decade])
            first_day[i.pol,i.year,i.decade] <- which(t_med > (6*17))[1]
            last_day[i.pol,i.year,i.decade] <- first_day[i.pol,i.year,i.decade] + 89
            if(!is.na(first_day[i.pol,i.year,i.decade])) {
                spawn[i.pol,
                      first_day[i.pol,i.year,i.decade]:min(last_day[i.pol,i.year,i.decade],182), # max last day is 182 (Aug 29th)
                      i.year,
                      i.decade] <- T
            }
            number_release_days[i.pol,i.year,i.decade] <- sum(spawn[i.pol,,i.year,i.decade])
        }
    }
}
save(spawn, first_day, number_release_days, file=paste0("spawning_dataset_Diplodus_sargus.RData"))

