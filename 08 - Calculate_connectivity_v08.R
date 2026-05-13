# 08 - Calculate connectivity

# larval mortality
#https://esajournals.onlinelibrary.wiley.com/doi/pdf/10.1890/13-2248.1
# exp(-d*PLD) is the cumulative survival 
# exp(-0.2*30)
# exp(-0.1*13)

rm(list=ls())

options(warn=1)

library(tidyverse)
library(xlsx)
library(terra)
library(RNetCDF)

i.species <- 1
species <- c("Diplodus_sargus","Paracentrotus_lividus","Diplodus_vulgaris")[i.species]


################################################################################
# Define parameters and read files
################################################################################

# Read polygon layer
pol <- vect("sites/sites.shp")

# Info on the initial positions
init_pos <- vect("initial_positions/initial_positions_100_0-50m.shp")
num_released_larvae <- as.vector(table(init_pos$i_pol)) # Number of released larvae per polygon

# Read table of release dates
release_dates <- read.xlsx("Release_dates.xlsx",sheetName = species)

# Read spawning dataset and competency dataset
load(paste0("spawning_dataset_",species,".RData"))
# load(paste0("PLD_dataset_",species,".RData")) 
# sd_PLD <- c(2,5,8)[i.species] # not used anymore

larval_mortality_rate <- 0.1
# https://esajournals.onlinelibrary.wiley.com/doi/10.1890/13-2248.1
# Planktonic larval mortality rates are lower than widely expected
# J. Wilson White, Steven G. Morgan, Jennifer L. Fisher


################################################################################
# Calculate DISPERSAL DISTANCE and DISPERSAL MATRIX over release days
################################################################################


scen_spawning <- 1
for (scen_spawning in 2 : 2) { # Only for second decade: 1: fixed spawning, 2: variable spawning
  i.decade <- 1
  for (i.decade in 2 : 2) {
    if(scen_spawning == 2 & i.decade == 1) next
    cat("Doing spawning",scen_spawning,"decade",i.decade,"\n"); flush.console()
    
    if (i.decade == 1) {
      v.year <- list(c(2025:2029), # c(2020:2029),
                     c(2020:2029),
                     c(2020:2028))[[i.species]]
    } else {
      v.year <- list(c(2095:2099), # c(2090:2099),
                     c(2090:2099),
                     c(2090:2098))[[i.species]]
    }
    
    # loop on  years
    i.year <- 1
    for (i.year in 1 : length(v.year)) {
      
      if(i.decade == 1) decade = "2020s/" else decade = "2090s/"
      
      # Cmatrix_year is the dispersal matrix for this year
      Cmatrix_year <- matrix(0, nrow=nrow(pol), ncol=nrow(pol))
      # Distance_year is the dispersal distance vector for this year
      Distance_year <- vector(length=nrow(pol))

      # distance_mean_r_day is the mean dispersal distance for each polygon and day of release
      distance_mean_r_day <- array(NA,c(nrow(pol),nrow(release_dates)))
      dimnames(distance_mean_r_day) <- list(pol=1:nrow(pol), r_day=1:nrow(release_dates))

      # Load PLD per larva
      # load(paste0("D:/Mean_PLD/Mean_PLD_",species,"_",v.year_PLD[i.year],".RData"))
      load(paste0("Mean_PLD/Mean_PLD_",species,"_",v.year[i.year],".RData"))

      # Loop on release days: r_day is the index of the ichthyop output files (there are 182 files per year)
      r_day <- 1
      for (r_day in 1 : nrow(release_dates)) {

        # Cmatrix_r_day is the dispersal matrix for each day of release
        Cmatrix_r_day <- array(0,c(nrow(pol),nrow(pol)))
        dimnames(Cmatrix_r_day) <- list(destination=1:nrow(pol), origin=1:nrow(pol))

        # File name and path to ichthyop output file
        rdate <- release_dates[[paste0("Y",v.year[i.year])]][r_day]
        ryear <- substr(rdate,1,4)
        file_name <- paste0("D:/output_ichthyop/",
                            decade,
                            ryear,
                            "/",
                            rdate,
                            ".nc")

        # Open Ichthyop output file
        cat(species, "Spawning",scen_spawning, as.character(rdate), "\n")
        res <- open.nc(file_name)
        # Read longitude and latitude of all larvae at all days
        lon <- var.get.nc(res, "lon")
        lat <- var.get.nc(res, "lat")
        max_transport_time <- ncol(lon)-1
        colnames(lon) <- paste0("lon_",c(0:max_transport_time))
        colnames(lat) <- paste0("lat_",c(0:max_transport_time))
        larvae <- data.frame(ID=1:nrow(lon),cbind(lon,lat),i_pol=init_pos$i_pol)
        rm(lon, lat)
        close.nc(res)

        # Add PLD (as a function of T on larval track)
        larvae$PLD <- data_PLD[[r_day]]$PLD

        # --> Retain only larvae whose release site is in the spawning window: take the decade according to scen_spawning
        larvae %>% filter(i_pol %in% which(spawn[,r_day,i.year,scen_spawning])) -> larvae

        # Replace NA-PLD with site mean
        larvae %>% group_by(i_pol) %>%
          mutate(site_mean_PLD = mean(PLD,na.rm=T)) %>%
          mutate(PLD = coalesce(round(site_mean_PLD), PLD)) %>%
          select(-site_mean_PLD) %>%
          ungroup() -> larvae
        # Omit larvae that might still be PLD
        larvae %>% na.omit -> larvae
        # If no larvae are released, go to next release day
        if (nrow(larvae) == 0) next

        # Create temporary objects (Cmatrix and distance) for each settlement day:
        ## Cmatrix_s_day is the dispersal matrix for each day of settlement
        Cmatrix_s_day <- array(0, c(nrow(pol), nrow(pol), max_transport_time))
        dimnames(Cmatrix_s_day) <- list(destination=1:nrow(pol), origin=1:nrow(pol), s_day=1:max_transport_time)
        ## Distance_s_day is the dispersal distance for each settled larva, in each day of settlement. In a list
        Distance_s_day <- vector(mode="list",length=max_transport_time)
        names(Distance_s_day) <- 1:max_transport_time

        # Loop on settlement days
        if (min(larvae$PLD)>max_transport_time) {
          cat("for release day",
              as.character(rdate),
              "settlement of larvae begins after",min(larvae$PLD),
              "days of pelagic time. Skipping this release day\n")
          next
        }
        for (s_day in max(min(larvae$PLD),1):max_transport_time) {

          if (s_day %% 10 == 0) {
            cat("Settlement day:",s_day,"\n")
            flush.console()
          }


          # Retain only larvae that have reached competency at s_day
          larvae %>% filter(PLD <= s_day) %>% as.data.frame -> competent_larvae

          # (1) Create spatial vector of final positions at this settlement day
          final_positions <- vect(data.frame(lon=competent_larvae[,paste0("lon_",s_day)],
                                             lat=competent_larvae[,paste0("lat_",s_day)],
                                             ID=competent_larvae$ID,
                                             i_pol=competent_larvae$i_pol),
                                  geom=c("lon", "lat"))
          # (2) Find settled larvae in this settlement day
          # Find overlap between larvae in this settlement day and settlement polygons
          a <- relate(final_positions, pol, pairs=T, relation = "intersects")
          a <- as_tibble(a)
          if(nrow(a) == 0) next # If no larvae are settled, go to next settlement day
          names(a) <- c("drifter","final_pol")
          # Append the ID of the larvae and the initial polygon to the "a" tibble
          a$ID <- final_positions$ID[a$drifter]
          a$initial_pol <- final_positions$i_pol[a$drifter]


          # (3) Calculate dispersal matrix (only for settled larvae)
          # Loop on final polygons (only those that are present in a):
          # for each, subdivide (hist) the larvae into initial polygons
          for (i.final.pol in unique(a$final_pol)) {
            a %>%
              filter(final_pol == i.final.pol) %>%         # Keep only larvae of the focal final polygon
              pull(initial_pol) %>%                        # Pull ID of the initial polygons
              hist(breaks=c(0:nrow(pol)),plot=F) %>%       # Bin the values
              .$counts ->                                  # Extract the counts
              # These are the elements of the row of the focal polygon in the Cmatrix
              Cmatrix_s_day[i.final.pol,,s_day]
          }
          # Divide each column by the number of released larvae in that polygon:
          Cmatrix_s_day[,,s_day] <- t(t(Cmatrix_s_day[,,s_day])/num_released_larvae)
          # Add it to the dispersal matrix of the release day after applying larval mortality
          Cmatrix_r_day <- Cmatrix_r_day + Cmatrix_s_day[,,s_day]*exp(-larval_mortality_rate*s_day)

          # (4) Calculate dispersal distance between initial positions and settlement positions (only for settled larvae)
          Distance_s_day[[s_day]] <- data.frame(r_pol = competent_larvae$i_pol[a$drifter],
                                                distance = round(distance(x = cbind(competent_larvae[a$drifter,"lon_0"],
                                                                                    competent_larvae[a$drifter,"lat_0"]),
                                                                          y = crds(final_positions[a$drifter]),
                                                                          pairwise=T,
                                                                          lonlat=T
                                                ) / 1000, # convert m to km
                                                1), # round to 1 decimal digit
                                                settlement_day = s_day
          )


          # (5) Remove settled larvae from the pool of drifting larvae
          larvae %>% filter(!ID %in% a$ID) -> larvae

          if(nrow(larvae) == 0) break # if no drifting larvae are left, break the loop
        } # End loop on settlement day (s_day)

        # Dispersal matrix
        Cmatrix_year <- Cmatrix_year + Cmatrix_r_day

        # Dispersal distance
        # Bind all settlement days to get distance for this release day
        bind_rows(Distance_s_day) -> Distance_r_day
        ## TEST sort(unique(Distance_r_day$r_pol)) %>% sort # should be equal to which(colSums(Cmatrix_r_day)>0)
        if (nrow(Distance_r_day) == 0) next
        for (i.pol in unique(Distance_r_day$r_pol)) {
          Distance_r_day %>% filter(r_pol == i.pol) -> Distance_r_day_pol
          Distance_r_day_pol %>%
            mutate(weights_dispersal_distance = exp(-larval_mortality_rate*settlement_day)) %>%
            summarise(weighted_distance = weighted.mean(distance, weights_dispersal_distance)) %>%
            pull(weighted_distance) ->
            distance_mean_r_day[i.pol, r_day]
        }
      } # End loop on release day r_day (one ichthyop file)

      # Divide the yearly dispersal probability matrix by the number of release days
      Cmatrix_year <- t(t(Cmatrix_year)/number_release_days[,i.year,scen_spawning])
      # Sum the dispersal distances over release days then divide them by the number of release days per polygon
      Distance_year <- rowSums(distance_mean_r_day,na.rm=T)
      Distance_year <- Distance_year / number_release_days[,i.year,scen_spawning]

      # Save variables for this year and scenario
      save(Cmatrix_year, Distance_year,
           file=paste0("connectivity/conn_",species,"_SPA",scen_spawning,"_",v.year[i.year],"_mu01.RData"))

      print(paste0("connectivity/conn_",species,"_SPA",scen_spawning,"_",v.year[i.year],"_mu01.RData"))
      
    } # end loop years
    
  } # end loop decade
} # end loop spawning


