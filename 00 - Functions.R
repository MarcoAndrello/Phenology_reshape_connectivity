# Functions needed for statistical analysis and plotting figures
## Functions to check NA before t tests
check.NA.temp <- function(x) {
  x %>% filter(decade == levels(x$decade)[1]) %>% pull(temp) %>% is.nan %>% all -> check.1
  x %>% filter(decade == levels(x$decade)[1]) %>% pull(temp) %>% is.na %>% all -> check.2
  x %>% filter(decade == levels(x$decade)[2]) %>% pull(temp) %>% is.nan %>% all -> check.3
  x %>% filter(decade == levels(x$decade)[2]) %>% pull(temp) %>% is.na %>% all -> check.4
  return(any(check.1, check.2, check.3, check.4))
}
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
check.NA <- function(x,variable) {
  x %>% filter(decade == levels(x$decade)[1]) %>% pull(variable) %>% is.nan %>% all -> check.1
  x %>% filter(decade == levels(x$decade)[1]) %>% pull(variable) %>% is.na %>% all -> check.2
  x %>% filter(decade == levels(x$decade)[2]) %>% pull(variable) %>% is.nan %>% all -> check.3
  x %>% filter(decade == levels(x$decade)[2]) %>% pull(variable) %>% is.na %>% all -> check.4
  return(any(check.1, check.2, check.3, check.4))
}

theme_map <- function() {
  theme_minimal() + 
    theme(
      legend.position="right",
      plot.title = element_text(hjust=0.5, size=8),
      legend.title=element_text(size=8, hjust=0.5),
      legend.text=element_text(size=8),
      plot.margin=unit(c(0,0,0,0),"cm"),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      legend.key.width  = unit(0.5, "lines"),
      legend.key.height = unit(5, "lines"),
      legend.margin=margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
      panel.grid.major = element_line(colour = "transparent"),
      panel.grid.minor = element_line(colour = "transparent")
    )
}

theme_biplot <- function() {
  theme_classic() + 
    theme(point=element_point(size=0.1),
          line=element_line(linewidth=0.1),
          legend.position="right",
          plot.title = element_text(hjust=0.5, size=8),
          legend.title=element_blank(),
          legend.text=element_text(size=8),
          plot.margin=unit(c(0,2,0,1),"cm"),
          axis.title=element_text(size=8),
          axis.text=element_text(size=5),
          legend.key.width  = unit(0.5, "lines"),
          legend.key.height = unit(1, "lines"),
          legend.margin=margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
    )
}




draw_map <- function(data, variable, pvalue=NA, map_title, fill_range, map_type, legend_title, panel_label) {
  # # Arguments
  # data <- data_decadal_distance
  # variable <- "diff_fixed"
  # pvalue <- "pvalue_fixed"
  # map_title <- "Warming effect (fixed spawning)"
  # fill_range <- c(-50, 65)
  # map_type <- "difference"
  # legend_title="Δ distance\n(km)"
  # panel_label="a"
  
  library(tidyverse)
  library(gridExtra)
  library(sf)
  library(rnaturalearth)
  library(cols4all)
  library(cowplot)
  library(scales)
  
  load("zero_prod_to_exclude.RData")
  
  pol <- st_read("reticolo_habidoneo_Mediterraneo/reticolo_utile.shp", quiet=T)
  pol %>%
    select(-c(left, top, right, bottom, row_index, col_index, id, ID_1, areaGradi2, Area_km2)) %>%
    mutate(ID.polygon = factor(1:1325)) -> pol
  if(nrow(pol) == 1325) pol[-zero_prod_to_exclude,] -> pol
  ne_countries(scale = 50, type = "countries") %>%
    st_crop(c(xmin=-6, ymin=29, xmax=36, ymax=45)) ->
    countries
  
  if(map_type == "value") {
    # Values per site of the decadal means
    data %>% left_join(x=pol,y=.,by="ID.polygon") -> pol_data
    ## Main map
    main_map <- ggplot() +
      geom_sf(data=countries, lwd=0.05) +
      geom_sf(data=pol_data, aes(fill = !!sym(variable)), lwd=0.05, show.legend=T) +
      guides(fill = guide_colourbar(theme = theme(legend.key.width  = unit(0.5, "lines"),
                                                  legend.key.height = unit(5, "lines"),
                                                  legend.margin=margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")))) +
      scale_fill_continuous_c4a_seq("brewer.yl_or_rd", limits=fill_range) 
  }
  
  if(map_type == "difference") {
    # Differences: per site of the decadal means
    ## Significance of the differences: Benjamini-Hochberg correction
    data$padjust <- p.adjust(data[[pvalue]],"BH")
    data %>% left_join(x=pol,y=.,by="ID.polygon") -> pol_data
    pol_data %>% mutate(pborder = if_else(padjust<0.05,T,F)) -> pol_data
    n_compar <- length(which(!is.na(pol_data$padjust)))
    n_signific <- length(which(pol_data$padjust<0.05))
    ## Split sf datasets to plot the significant ones after the non-significant ones
    pol_data %>% filter(!pborder) -> pol_data_ns
    pol_data %>% filter(pborder) -> pol_data_s
    ## Main map
    main_map <- ggplot() +
      geom_sf(data=countries, lwd=0.05) +
      geom_sf(data=pol_data_ns, aes(fill = !!sym(variable)), colour = NA, show.legend=T) +
      geom_sf(data=pol_data_s, aes(fill = !!sym(variable)), show.legend=T, lwd=0.05) +
      guides(fill = guide_colourbar(theme = theme(legend.key.width  = unit(0.5, "lines"),
                                                  legend.key.height = unit(5, "lines"),
                                                  legend.margin=margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")))) +
      scale_fill_continuous_c4a_div("brewer.pu_or", limits=fill_range) +
      # scale_fill_continuous_c4a_div("bu_wh_rd", limits=fill_range) +
      # scale_color_discrete(palette=c("gray90","black"))
      annotate("rect", xmin = 18, xmax = 36, ymin = 45, ymax = 47, fill= "white", col="black", lwd=0.1) +
      annotate("text", x = 27, y = 46, label = paste0(n_signific," / ",n_compar," = ",percent(n_signific/n_compar)), size = 3)
    
  }
  
  if(map_type == "effect") {
    # Values per site of the effect of spawning
    data %>% left_join(x=pol,y=.,by="ID.polygon") -> pol_data
    ## Main map
    main_map <- ggplot() +
      geom_sf(data=countries, lwd=0.05) +
      geom_sf(data=pol_data, aes(fill = !!sym(variable)), lwd=0.05, show.legend=T) +
      guides(fill = guide_colourbar(theme = theme(legend.key.width  = unit(0.5, "lines"),
                                                  legend.key.height = unit(5, "lines"),
                                                  legend.margin=margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")))) +
      scale_fill_continuous_c4a_div("gmt.polar", limits=fill_range) 
  }
  
  main_map <- 
    main_map +
    labs(title = map_title, fill=legend_title) +
    annotate("text", x = -Inf, y = Inf, label = panel_label, hjust = -1.5, vjust = 1.5, size = 4) +
    theme_map()
    
  return(main_map)
}


theme_density <- function() {
  theme_classic() + 
    theme(point=element_point(size=0.1),
          line=element_line(linewidth=0.1),
          plot.title = element_text(hjust=0.5, size=9),
          plot.margin=unit(c(0,0,0.5,0),"cm"),
          axis.title=element_text(size=7),
          axis.text.x=element_text(size=7),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()
    )
}

draw_density <- function(data, variable, panel_title, fill_range, x_title, panel_label) {
  # data <- data_decadal_larval_temperature
  # variable <- "present"
  # panel_title <- "Present temperature in spawning sites"
  # fill_range <- c(15, 26)
  # x_title="Temperature (°C)"
  # panel_label="a"
  
  density_plot <- ggplot(data) +
    geom_density(aes(!!sym(variable)), fill="gray", outline.type="full", lwd= 0.05) + xlab(x_title) + ylab("") + ggtitle(panel_title) +
    coord_cartesian(xlim=fill_range) +
    annotate("text", x = -Inf, y = Inf, label = panel_label, hjust = -1.5, vjust = 1.5, size = 4) +
    theme_density() 
  # density_plot
  return(density_plot)
}





