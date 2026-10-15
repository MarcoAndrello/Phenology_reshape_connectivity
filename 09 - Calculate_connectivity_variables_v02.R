# 09 - Calculate connectivity variables

rm(list=ls())

library(tidyverse)
library(igraph)
library(sf)

scen <- 45
mortality_rate <- 0.02

i.species <- 1
species <- c("Diplodus_sargus","Diplodus_vulgaris","Paracentrotus_lividus")[i.species]

pol <- st_read("sites/sites.shp")
# load("habitat_area_per_polygon.RData")
# habitat_area_per_polygon %>%
#   rename(M151 = "MC151: Coralligenous biocenosis",
#          MB15 = "MB15: Mediterranean infralittoral rock",                                         
#          MB2523 = "MB2523: Facies of dead \"mattes\" of [Posidonia oceanica] without much epiflora",
#          MC251 = "MC251: Coralligenous platforms",                                                
#          MB252 = "MB252: Biocenosis of [Posidonia oceanica]"
#          ) -> habitat_area_per_polygon
# 
# pol %>%
#   st_drop_geometry %>%
#   select(ID) %>%
#   left_join(habitat_area_per_polygon, by=join_by("ID" == "ID_1")) %>%
#   replace_na(list(ID_1=0, M151=0, MB15=0, MB2523=0, MC251=0, MB252=0)) ->
#   productivity

# productivity %>% as.matrix %>% .[,2:6] %>% rowSums %>% sort
# which((productivity %>% as.matrix %>% .[,2:6] %>% rowSums) == 0) -> zero_prod_to_exclude ##### Check: it gives 241 sites, not 12
# save(zero_prod_to_exclude,file="zero_prod_to_exclude.RData")

load("zero_prod_to_exclude.RData")

# Remove pol to exlcude
# productivity <- productivity[-zero_prod_to_exclude,]

# Calculate productivity as weighted average of habitat-specific productivity
# productivity %>%
#   # Posidonia habitat has 8% productivity
#   mutate(prod = (M151 + MB15 + 0.08*MB2523 + MC251 + 0.08*MB252)) ->
#   productivity
#save(productivity,file="productivity.RData")

# Create productivity matrix
# matrix(rep(productivity$prod,nrow(productivity)),
#        ncol=nrow(productivity),
#        byrow=T) ->
#     productivity_matrix



# Load spawning days
load(paste0("output/spawning_dataset_",species,"_RCP",scen,".RData"))
spawn <- spawn[-zero_prod_to_exclude,,,]

# Calculate the connectivity variables
dd <- vector(mode = "list", length = 10*3) # 10 years per decade/hydro (9 for vulgaris) * 3 (present, future fixed, future variable)

i.k <- 1
scen_spawning <- 1
i.decade <- 1
i.year <- 4
for (scen_spawning in 1 : 2){ 
  for (i.decade in 1 : 2){
    
    if(scen_spawning == 2 & i.decade == 1) next
    cat("Doing spawning",scen_spawning,"decade",i.decade,"\n"); flush.console()
    
    if (i.decade == 1) {
      v.year <- c(2020:2029)
    } else {
      v.year <- c(2090:2099)
    }
    i.year <- 9
    for (i.year in 1 : 10){
      if (i.year == 10 & species == "Diplodus_vulgaris") next
      year <- v.year[i.year]
      cat("Spawning",scen_spawning,"Decade",i.decade,"Year",i.year,"\n"); flush.console()
      
      # Find sites where spawning did not occur in this year
      rowSums(spawn[,,i.year,scen_spawning]) -> num_spawning_days
      id.no.spawning <- which(num_spawning_days == 0)
      
      # Load raw data
      load(
        paste0("output/connectivity/RCP",scen,"/conn_",species,"_SPA",scen_spawning,"_",v.year[i.year],"_mu",mortality_rate*100,".RData")
      )
      # Check if there are NaN / NA
      cat("Spawning",scen_spawning,"Decade",i.decade,"Year",i.year,
          "NaN:",length(which(is.nan(Cmatrix_year))),
          "NA:",length(which(is.na(Cmatrix_year))),"\n")
      
      # Define a new probability matrix by Filtering out sites with 0 productivity
      Cmatrix_prob <- Cmatrix_year[-zero_prod_to_exclude,-zero_prod_to_exclude]
      
      # # Calculate flow matrix: dispersal matrix multiplied by the productivity of each site
      # Cmatrix_flow <- Cmatrix_prob * productivity_matrix
      # diag(Cmatrix_flow) <- 0
      
      # Calculate indegree and outdegree
      # Cmatrix_unit <- ceiling(Cmatrix_prob) # adjacency matrix
      # diag(Cmatrix_unit) <- 0 # exclude self-loops
      # rowSums(Cmatrix_unit,na.rm=T) -> indegree
      # colSums(Cmatrix_unit,na.rm=T) -> outdegree
      # outdegree[id.no.spawning] <- NA # set outdegree to NA in sites where spawning does not occurr
      
      # Make tibble from Distance_year
      tibble(ID.polygon = factor(c(1:1313)),
             distance = Distance_year[-zero_prod_to_exclude]) %>%
        # Calculate connectivity metrics and add them to the dataframe
        mutate(tss = colSums(Cmatrix_prob)) ->                   # Total settlement success
               # local_retention = diag(Cmatrix_prob)) ->                      # Local retention
               # indegree = indegree,
               # outdegree = outdegree) %>%
        # mutate(outflow = colSums(Cmatrix_flow, na.rm=T), 
        #        inflow = rowSums(Cmatrix_flow, na.rm=T)) %>% 
        # mutate(netflow = (outflow-inflow) / (outflow+inflow)) ->
        dd[[i.k]]
      # # Set to 0 sites where both inflow and outflow are 0:
      # dd[[i.k]]$netflow[ which(dd[[i.k]]$inflow + dd[[i.k]]$outflow == 0) ] <- 0
      # # Set to NA sites where spawning does not occur
      # dd[[i.k]]$outflow[id.no.spawning] <- NA
      # dd[[i.k]]$netflow[id.no.spawning] <- NA
      
      
      # Add scenario factors
      dd[[i.k]] %>%
        mutate(scen_spawning = factor(scen_spawning, levels=c(1,2)),
               i.decade = factor(i.decade, levels=c(1,2)),
               year = i.year) ->            # Add explanatory variables
        dd[[i.k]]
      
      i.k <- i.k + 1
    }
  }
}

# Combine values in the same data.frame
data <- bind_rows(dd)
data
summary(data)

# Save
save(data,file=paste0("output/Data_connectivity_",species,"_RCP",scen,"_mu",mortality_rate*100,".RData"))

################################################################################
# Calculate mean Cmatrices over years
################################################################################
# Mean Cmatrix for a given scen_spawning, and i.decade: maybe useful for representation or as explanatory variables in other studies
scen_spawning <- 1
i.decade <- 1
i.year <- 1
for (scen_spawning in 1 : 2){ 
  for (i.decade in 1 : 2){
    
    if(scen_spawning == 2 & i.decade == 1) next
    cat("Doing spawning",scen_spawning,"decade",i.decade,"\n"); flush.console()
    
    if (i.decade == 1) {
      v.year <- list(c(2020:2029),
                     c(2020:2029),
                     c(2020:2028))[[i.species]]
    } else {
      v.year <- list(c(2090:2099),
                     c(2090:2099),
                     c(2090:2098))[[i.species]]
    }
    
    # Define the decadal matrix
    Cmatrix_decade <- matrix(0,nrow=1313,ncol=1313)
    
    for (i.year in 1 : length(v.year)){
      year <- v.year[i.year]
      cat("Spawning",scen_spawning,"Decade",i.decade,"Year",i.year,"\n"); flush.console()
      # Load Cmatrix
      load(
        paste0("output/connectivity/RCP",scen,"/conn_",species,"_SPA",scen_spawning,"_",v.year[i.year],"_mu",mortality_rate*100,".RData")
      )
      # Set NaN to 0
      Cmatrix_year[which(is.nan(Cmatrix_year))] <- 0
      # Remove sites with 0 productivity and add the year's matrix to the decadal matrix
      Cmatrix_decade <- Cmatrix_decade + Cmatrix_year[-zero_prod_to_exclude,-zero_prod_to_exclude]
      
    }
    # Divide each element of the decadal matrix by the number of matrices (10; except for Diplodus_vulgaris which has only 9)
    Cmatrix_decade <- Cmatrix_decade / length(v.year)
    save(Cmatrix_decade,file=paste0("output/connectivity/RCP",scen,"/Mean_Cmatrix_",species,"_SPA",scen_spawning,"_",c("2020decade","2090decade")[i.decade],"_mu",mortality_rate*100,".RData"))
  }
}



