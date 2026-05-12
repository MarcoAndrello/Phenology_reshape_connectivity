# 03 - Read temperature in spawning sites
rm(list=ls())

library(tidyverse)
library(terra)
library(lubridate)

pol <- vect("sites/sites.shp")
pol %>%  centroids %>% geom %>% .[,c(3,4)] -> release_coords

v_year <- c(2090:2099)

i.year <- 10
for (i.year in 1 : 10) {
    year <- v_year[i.year]
    cat(year,"\n")
    num_days <- c(31,28,31,30,31,30,31,31,30,31,30,31)
    temp_list <- list()
    for (i.month in 1 : 12) {
        temp <- rast(paste0("F:/Temp/C3S-MCF_CERES_daily_TSUVchl_Med.rcp85.",year,".",sprintf("%02d",(i.month)),".nc"),"thetao") # PUT PATH TO POLCOMS FILES
        # Retain only surface temperature
        temp <- temp[[seq(1,c(43*(num_days[i.month]-1)+1),43)]]
        # Reclassify big values into NA and good values to 0...
        classify(temp,matrix(c(-10,40,0,40,Inf,NA),ncol=3,byrow = T)) -> t2
        # ... so as to remove big values in t3:
        temp + t2 -> t3
        # Extract the values
        terra::extract(t3,release_coords,cells=T) -> tt
        # For the NA, look into the adjacent values
        id_land_pol <- which(is.na(tt[,2]))
        id_land_cells <- tt[id_land_pol,1]
        for (i in 1 : length(id_land_cells) ) {
            adjacent(t3, id_land_cells[i],directions="queen") %>% as.vector -> id_adjacent_cells
            tt[id_land_pol[i],2:(num_days[i.month]+1)] <- colMeans(t3[id_adjacent_cells],na.rm=T)
        }
        # Store the values into temp_list
        names(tt) <- 
            temp_list[[i.month]] <- tt[,2:(num_days[i.month]+1)]
    }
    # Bind the months
    cbind(temp_list[[1]],
          temp_list[[2]],
          temp_list[[3]],
          temp_list[[4]],
          temp_list[[5]],
          temp_list[[6]],
          temp_list[[7]],
          temp_list[[8]],
          temp_list[[9]],
          temp_list[[10]],
          temp_list[[11]],
          temp_list[[12]]) %>% as.matrix -> temp_year
    colnames(temp_year) <- 1:sum(num_days)
    
    save(temp_year,file=paste0("F:/Temp/output/Temp_",year,".RData"))
}


# Reformat temperature data into array with start March 1st
temperature <- array(NA,dim=c(nrow(pol),365,10,2))
dimnames(temperature) <- list(pol=1:nrow(pol),
                              day=paste(month(as_date(date("2021-01-01"):date("2021-12-31")),label=T),day(as_date(date("2021-01-01"):date("2021-12-31"))))[c(60:365,1:59)],
                              year=1:10,
                              decade=1:2)
i.decade <- 1
for (i.decade in 1 : 2) {
    v.year <- c(2020:2029) + (i.decade-1)*70
    i.year <- 1
    for (i.year in 1 : 10) {
        # Read temperature of the year
        load(paste0("[Path to temperatures]/Temp_",v.year[i.year],".RData"))
        ## If loop year, remove February 29th for simplicity
        if (ncol(temp_year) == 366) temp_year <- temp_year[,-60]
        ## Put months 3 to 12 at the beginning
        temperature[,1:306,i.year,i.decade] <- temp_year[,60:365]
        ## Put months 1 to 2 at the end
        temperature[,307:365,i.year,i.decade] <- temp_year[,1:59]

    }
}
save(temperature, file="temperature_dataset_Diplodus_sargus.RData")
