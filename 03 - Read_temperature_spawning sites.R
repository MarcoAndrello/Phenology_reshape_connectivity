# 03 - Read temperature in spawning sites
rm(list=ls())

library(tidyverse)
library(terra)
library(lubridate)

pol <- vect("sites/sites.shp")
pol %>%  centroids %>% geom %>% .[,c(3,4)] -> release_coords

v_year <- c(2090:2099)
scen <- "85"

num_days <- list()
for (i in 1 : 10) num_days[[i]] <- c(31,28,31,30,31,30,31,31,30,31,30,31)
names(num_days) <- v_year
## 2020s:
# num_days[[1]] <- num_days[[5]] <- num_days[[9]] <- c(31,29,31,30,31,30,31,31,30,31,30,31)
# ## 2090s:
num_days[[3]] <- num_days[[7]] <- c(31,29,31,30,31,30,31,31,30,31,30,31)

i.year <- 1
for (i.year in 1 : 10) {
  year <- v_year[i.year]
  temp_list <- list()
  i.month <- 1
  for (i.month in 1 : 12) {
    cat(year,i.month,"\n"); flush.console()
    if (scen == "45") {
      if (year == 2020) {
        temp <- rast(paste0("C:/Users/marco/Desktop/Modify POLCOMS/RCP45/POLCOMS_ERSEM_biogeochemical-daily-all-rcp",scen,"-thetao-",year,"-",sprintf("%02d",(i.month)),"-v1.1.nc"),"thetao")
      } else {
        temp <- rast(paste0("C:/Users/marco/Desktop/Modify POLCOMS/RCP45/C3S-MCF_CERES_daily_TSUVchl_Med.rcp",scen,".",year,".",sprintf("%02d",(i.month)),".nc"),"thetao")
      }
    } else {
      temp <- rast(paste0("D:/Temperature POLCOMS/POLCOMS_ERSEM_biogeochemical-daily-all-rcp",scen,"-thetao-",year,"-",sprintf("%02d",(i.month)),"-v1.1.nc"),"thetao")
    }
    
    # Retain down to 50 metres (level 13)
    #temp <- temp[[seq(1,c(43*(num_days[[i.year]][i.month]-1)+1),43)]] # surface
    temp <- temp[[1:(13 * num_days[[i.year]][i.month])]] # 50 metres (level 13)
    # Reclassify big values into NA and good values to 0...
    classify(temp,matrix(c(-10,40,0,40,Inf,NA),ncol=3,byrow = T)) -> t2
    # ... so as to remove big values in t3:
    temp + t2 -> t3
    # Extract the values
    terra::extract(t3,release_coords,cells=T) -> tt
    rm(temp, t2)
    # For the NA, look into the adjacent values
    id_land_pol <- which(is.na(tt[,2]))
    id_land_cells <- tt[id_land_pol,1]
    i <- 1
    for (i in 1 : length(id_land_cells) ) {
      adjacent(t3, id_land_cells[i],directions="queen") %>% as.vector -> id_adjacent_cells
      tt[id_land_pol[i],2:ncol(tt)] <- colMeans(t3[id_adjacent_cells],na.rm=T)
    }
    rm(t3)
    ## Averaging over depth levels
    temp_list[[i.month]] <- data.frame(cell = tt$cell)
    i.day <- 1
    for (i.day in 1:num_days[[i.year]][i.month]) {
      cols <- 2 + (0:(13 - 1)) * num_days[[i.year]][i.month] + (i.day - 1)
      temp_list[[i.month]][[paste0("day_", i.day)]] <- rowMeans(tt[, cols], na.rm = TRUE)
    }
    temp_list[[i.month]] <- temp_list[[i.month]][,-1] # remove "cell" column
    
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
  colnames(temp_year) <- 1:sum(num_days[[i.year]])
  
  save(temp_year,file=paste0("D:/Temperature POLCOMS/Temperature spawning sites/RCP",scen,"/Temp_",year,".RData"))
}


# Reformat temperature data into array 
temperature <- array(NA,dim=c(nrow(pol),365,10,2))
dimnames(temperature) <- list(pol=1:nrow(pol),
                              day=paste(month(as_date(date("2021-01-01"):date("2021-12-31")),label=T),day(as_date(date("2021-01-01"):date("2021-12-31")))),
                              year=1:10,
                              decade=1:2)
i.decade <- 1
for (i.decade in 1 : 2) {
    v.year <- c(2020:2029) + (i.decade-1)*70
    i.year <- 1
    for (i.year in 1 : 10) {
        # Read temperature of the year
        load(paste0("D:/Temperature POLCOMS/Temperature spawning sites/RCP",scen,"/Temp_",v.year[i.year],".RData"))
        ## If loop year, remove February 29th for simplicity
        if (ncol(temp_year) == 366) temp_year <- temp_year[,-60]
        temperature[,,i.year,i.decade] <- temp_year
    }
}
save(temperature, file=paste0("output/temperature_dataset_Diplodus_sargus_RCP",scen,".RData"))


