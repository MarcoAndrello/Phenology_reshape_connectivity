# 04 - Calculate mean temperature over larval dispersal pathway
# Attention: rcp45 and year 2020 should use the theta_ref of rcp85
rm(list=ls())

options(warn=1)

library(tidyverse)
library(xlsx)
library(terra)
library(RNetCDF)
library(lubridate)

scen <- 85

# Build nc_map
## Path NetCDF
if (scen == 85) nc_path <- "D:/Temperature POLCOMS" # scen 85 
if (scen == 45) nc_path <- "C:/Users/marco/Desktop/Modify POLCOMS/RCP45" # scen 45
## list files thetao
nc_files <- list.files(nc_path, pattern = "thetao-\\d{4}-\\d{2}-v1\\.1\\.nc$", full.names = TRUE)
## Extract YYYY-MM from filename
file_months <- sub(".*thetao-(\\d{4}-\\d{2})-v1\\.1\\.nc$", "\\1", nc_files)
# If rcp45, most files have different names:
if (scen == 45) {
  nc_files_2 <- list.files(nc_path,pattern = "rcp45\\.\\d{4}\\.\\d{2}\\.nc$", full.names = TRUE)
  file_months_2 <- sub(".*rcp45\\.(\\d{4})\\.(\\d{2})\\.nc$","\\1-\\2",nc_files_2)
  nc_files <- c(nc_files, nc_files_2)
  file_months <- c(file_months, file_months_2)
  rm(nc_files_2, file_months_2)
}


# --- Named vector: "YYYY-MM" -> filepath
nc_map <- setNames(nc_files, file_months)

################################################################################
# Define parameters and read files
################################################################################

# Read polygon layer
pol <- vect("sites/sites.shp")

# Info on the initial positions
init_pos <- vect("initial_positions/initial_positions_100_0-50m.shp")
# num_released_larvae <- as.vector(table(init_pos$i_pol)) # Number of released larvae per polygon

# # Read grid, time unit, fillvalue and NA points
# if (scen == 85) nc_ref <- open.nc(paste0(nc_path,"/POLCOMS_ERSEM_biogeochemical-daily-all-rcp",scen,"-thetao-2020-01-v1.1.nc")) # rcp85 or 45 with year 2020
# if (scen == 45) nc_ref <- open.nc(paste0(nc_path,"/C3S-MCF_CERES_daily_TSUVchl_Med.rcp45.2021.01.nc")) # rcp45 (C3S)
# lon_nc   <- var.get.nc(nc_ref, "lon")
# lat_nc   <- var.get.nc(nc_ref, "lat")
# depth_nc <- var.get.nc(nc_ref, "depth")
# tunits <- att.get.nc(nc_ref, "time", "units")
# fillvalue <- att.get.nc(nc_ref, "thetao", "_FillValue")
# theta_ref <- var.get.nc(nc_ref, "thetao")
# # Set fillvalue to NA
# theta_ref[abs(theta_ref - fillvalue) < 1e-10 * abs(fillvalue)] <- NA
# # dims <- dim(theta_ref)
# close.nc(nc_ref)
# # Origin of time
# origin_datetime <- sub("seconds since ", "", tunits)
# #  Regular grid parameters (robust to floating noise)
# dlon <- mean(diff(lon_nc))
# dlat <- mean(diff(lat_nc))
# lon0 <- lon_nc[1]
# lat0 <- lat_nc[1]
# # save(depth_nc, origin_datetime, fillvalue, dlon, dlat, lon0, lat0, file="theta_ref_C3S.RData")
# save(depth_nc, origin_datetime, fillvalue, dlon, dlat, lon0, lat0, file="theta_ref_v11.RData")
# rm(lon_nc, lat_nc, tunits,
#    depth_nc, origin_datetime, fillvalue, dlon, dlat, lon0, lat0)

# # # Find all NA points and their indices
# arr1 <- theta_ref[,,,1]
# na_idx <- which(is.na(arr1), arr.ind = TRUE)
# # store into dataframe
# na_idx <- as.matrix(data.frame(ix = na_idx[,1], iy = na_idx[,2], iz = na_idx[,3]))
# # find non-NA points
# valid_idx <- which(!is.na(arr1), arr.ind = TRUE)
# # nearest neighbor
# nn <- RANN::nn2(data = valid_idx, query = na_idx, k = 1)
# nearest_valid <- valid_idx[nn$nn.idx[,1], ]
# # output
# points_subst <- data.frame(
#   ix = na_idx[,1],
#   iy = na_idx[,2],
#   iz = na_idx[,3],
#   ix_nn = nearest_valid[,1],
#   iy_nn = nearest_valid[,2],
#   iz_nn = nearest_valid[,3]
# )
# points_subst -> points_substC3S
# save(points_subst,file="points_subst_C3S.RData")
# rm(theta_ref, arr1, na_idx, valid_idx, nn, nearest_valid)

load("points_subst.RData")


i.species <- 1
for (i.species in 1 : 1) {
  species <- c("Diplodus_sargus","Paracentrotus_lividus","Diplodus_vulgaris")[i.species]
  number_years_per_decade <- c(10,10,9)[i.species]
  # Read table of release dates
  release_dates <- read.xlsx("Release_dates.xlsx",sheetName = species)
  release_dates <- release_dates[1:59,] #### QUI PER FARE SOLO gennaio-febbraio
  ndays <- c(15,25,40)[i.species]
  i.decade <- 1
  for (i.decade in 1 : 2) {
    cat("Doing",species,"decade",i.decade,"\n"); flush.console()
    if (i.decade == 1) {
      v.year <- list(c(2020:2029),
                     c(2020:2029),
                     c(2020:2028))[[i.species]]
    } else {
      v.year <- list(c(2090:2099),
                     c(2090:2099),
                     c(2090:2098))[[i.species]]
    }
    # loop on years
    i.year <- 1
    for (i.year in 1 : 10) {
      year <- v.year[i.year]
      if(year < 2030) decade = "2020s/" else decade = "2090s/"
      
      # Load ref
      if (scen == 85 | (scen == 45 & year == 2020) ) {
        load("theta_ref_v11.RData")
      } else {
        load("theta_ref_C3S.RData")
      }
      
      # Extract all temperature cubes of that year
      all_months <- 
        unique(format(
          as.Date( c(release_dates[,(((i.decade-1)*number_years_per_decade)+i.year)], release_dates[,(((i.decade-1)*number_years_per_decade)+i.year)] + ndays) ),
          "%Y-%m"
        ))
      
      theta_list <- dates_nc <- list()
      m <- "2020-05"
      for (m in all_months) {
        message("Loading month ", m)
        nc <- open.nc(nc_map[m])
        # --- temperature
        theta_list[[m]] <- var.get.nc(nc, "thetao")
        # --- time
        time_nc <- var.get.nc(nc, "time")
        dates_nc[[m]] <- as.POSIXct(time_nc,
                                    origin = origin_datetime,
                                    tz = "UTC")
        dates_nc[[m]] <- as.Date(dates_nc[[m]])
        close.nc(nc)
        # Set fillvalue to NA
        theta_list[[m]][abs(theta_list[[m]] - fillvalue) < 1e-10 * abs(fillvalue)] <- NA
      }
      
      mean_temp <- list() # on release days
      # Loop on release days: r_day is the index of the ichthyop output files 
      r_day <- 1
      for (r_day in 1 : nrow(release_dates)) {
        # File name and path to ichthyop output file
        rdate <- release_dates[[paste0("Y",year)]][r_day]
        ryear <- substr(rdate,1,4)
        if (scen == 85) file_name <- paste0("D:/output_ichthyop_RCP85/", decade, ryear,"/",rdate,".nc")
        if (scen == 45) file_name <- paste0("D:/output_ichthyop/",rdate,".nc")
        
        # Open Ichthyop output file
        cat(species, as.character(rdate), "\n")
        res <- open.nc(file_name)
        # Read longitude and latitude of all larvae at all days
        lon <- var.get.nc(res, "lon")
        lat <- var.get.nc(res, "lat")
        depth <- abs(var.get.nc(res, "depth"))
        max_transport_time <- ncol(lon)-1
        colnames(lon) <- paste0("lon_",c(0:max_transport_time))
        colnames(lat) <- paste0("lat_",c(0:max_transport_time))
        colnames(depth) <- paste0("depth_",c(0:max_transport_time))
        #larvae <- data.frame(ID=1:nrow(lon),cbind(lon,lat),i_pol=init_pos$i_pol)
        lon <- data.frame(lon,i_pol=init_pos$i_pol)
        lat <- data.frame(lat,i_pol=init_pos$i_pol)
        depth <- data.frame(depth,i_pol=init_pos$i_pol)
        close.nc(res)
        
        ndrifts <- nrow(lon)
        # Dates to read temperatures
        dates <- as.Date(rdate) + 0:(ndays - 1)
        # Set matrix of temperatures
        temp <- matrix(NA_real_, ndrifts, ndays)
        # Months required from this release date 
        months_needed <- unique(format(dates, "%Y-%m"))
        m <- months_needed[1]
        # Loop on months
        for (m in months_needed) {
          # days of larval trajectory in this month
          day_idx <- which(format(dates, "%Y-%m") == m)
          t_idx   <- match(dates[day_idx], dates_nc[[m]])
          # dim of this month
          dims <- dim(theta_list[[m]])  # [nx, ny, nz, nt]
          k <- 1
          for (k in seq_along(day_idx)) {
            j <- day_idx[k]
            # --- spatial indices (nearest neighbour) of the coordinates that need to be extracted 
            ix <- round((lon[,j] - lon0) / dlon) + 1
            iy <- round((lat[,j] - lat0) / dlat) + 1
            ix[ix < 1] <- 1
            iy[iy < 1] <- 1
            ix[ix > dims[1]] <- dims[1]
            iy[iy > dims[2]] <- dims[2]
            # --- nearest depth index
            iz <- max.col(-abs(outer(depth[,j], depth_nc, "-")))
            # correction for NA
            punti <- data.frame(ix,iy,iz)
            if (scen == 85 | (scen == 45 & year == 2020) ) {
              punti_new <- punti %>% left_join(points_subst_v11, by = c("ix", "iy", "iz"))
            } else {
              punti_new <- punti %>% left_join(points_subst_C3S, by = c("ix", "iy", "iz"))
            }
            punti_new <- punti_new %>%
              mutate(
                ix = if_else(!is.na(ix_nn), ix_nn, ix),
                iy = if_else(!is.na(iy_nn), iy_nn, iy),
                iz = if_else(!is.na(iz_nn), iz_nn, iz)
              ) %>%
              select(ix, iy, iz)
              
            # --- linear index 4D
            # linear_index <- ix +
            #   (iy - 1) * dims[1] +
            #   (iz - 1) * dims[1] * dims[2] +
            #   (t_idx[k] - 1) * dims[1] * dims[2] * dims[3]
            
            linear_index_new <- punti_new$ix +
              (punti_new$iy - 1) * dims[1] +
              (punti_new$iz - 1) * dims[1] * dims[2] +
              (t_idx[k] - 1) * dims[1] * dims[2] * dims[3]
            # 
            # s <- data.frame(v1=theta_list[[m]][linear_index],
            #                 v2=theta_list[[m]][linear_index_new])
            
            temp[,j] <- theta_list[[m]][linear_index_new]
          } # end k (days of transport)
        }
        mean_temp[[r_day]] <- rowMeans(temp,na.rm=T)
      } # End loop on release day r_day (one ichthyop file)
      save(mean_temp,file=paste0("D:/Mean_temperature_on_larval_dispersal/RCP",scen,"/Mean_temp_",species,"_",year,".RData"))
    rm(depth_nc, origin_datetime, fillvalue, dlon, dlat, lon0, lat0)  
    } # end loop years
    
  } # end loop decade
  
} # end loop species

# # Combine files January-February with files March-August
# species <- "Diplodus_sargus"
# scen <- 85
# v.year <- c(2020:2029,2090:2099)
# i.year <- 1
# for (i.year in 2 : 20) {
#   year <- v.year[i.year]
#   cat(year,"\n")
#   file_JF <- paste0("D:/Mean_temperature_on_larval_dispersal/RCP",scen,"/Mean_temp_",species,"_",year,"_JF.RData")
#   load(file_JF)
#   mean_temp_combined <- mean_temp
#   file_MA <- paste0("D:/Mean_temperature_on_larval_dispersal/RCP",scen,"/Mean_temp_",species,"_",year,".RData")
#   load(file_MA)
#   mean_temp_combined <- c(mean_temp_combined, mean_temp[1:122])
#   mean_temp <- mean_temp_combined
#   rm(mean_temp_combined)
#   save(mean_temp,file=paste0("D:/Mean_temperature_on_larval_dispersal/RCP",scen,"/combined/Mean_temp_",species,"_",year,".RData"))
# }


