# 12 - Draw figures of results

rm(list=ls())

library(tidyverse)
library(sf)

scen <- 45

# Functions
source("Functions - Map.R")
load("zero_prod_to_exclude.RData")

v.species <- c("Diplodus_sargus","Diplodus_vulgaris","Paracentrotus_lividus")
i.species <- 1
species <- v.species[i.species]




################################################################################
# Temperature in the spawning sites 
################################################################################

load(paste0("output/Data_figure_spawning_temperature_RCP",scen,".RData"))

# Stats
data_decadal_spawning_temperature$contemporary %>% quantile(c(0.025, 0.5, 0.975))
data_decadal_spawning_temperature$contemporary %>% range
data_decadal_spawning_temperature$future %>% quantile(c(0.025,0.5,0.975))
data_decadal_spawning_temperature$future %>% range
data_decadal_spawning_temperature$diff %>% quantile(c(0.025,0.5,0.975))
data_decadal_spawning_temperature$diff %>% range

# Maps
map_spawning_temperature_contemporary <- draw_map(data_decadal_spawning_temperature, "contemporary", NA, "Contemporary temperature in spawning sites", fill_range=c(15.44, 22.24), map_type="value", legend_title="Temperature\n(°C)", panel_label="a")
map_spawning_temperature_future <- draw_map(data_decadal_spawning_temperature, "future", NA, "Future temperature in spawning sites", c(15.44, 22.24), "value","Temperature\n(°C)", "b")
map_spawning_temperature_difference <- draw_map(data_decadal_spawning_temperature, "diff", "pvalue", "Temperature difference in spawning sites", c(0.62, 1.44), "difference", "Δ temperature\n(°C)", "c")

# Density plots
density_spawning_temperature_contemporary <- draw_density(data_decadal_spawning_temperature, "contemporary", "Contemporary temperature in spawning sites", c(15.44, 22.24), "Temperature (°C)", "a")
density_spawning_temperature_future <- draw_density(data_decadal_spawning_temperature, "future", "Future temperature in spawning sites", c(15.44, 22.24), "Temperature (°C)", "b")
density_spawning_temperature_difference <- draw_density(data_decadal_spawning_temperature, "diff", "Temperature difference in spawning sites", c(0.62, 1.44), "Δ temperature (°C)", "c")



################################################################################
# Temperature during transport 
################################################################################

load(paste0("output/Data_figure_pelagic_temp_RCP",scen,".RData"))

# Stats
data_decadal_pelagic_temperature$contemporary %>% quantile(c(0.025, 0.5, 0.975))
data_decadal_pelagic_temperature$contemporary %>% range
data_decadal_pelagic_temperature$future %>% quantile(c(0.025, 0.5, 0.975))
data_decadal_pelagic_temperature$future %>% range
data_decadal_pelagic_temperature$diff_baseline %>% quantile(c(0.025, 0.5, 0.975))
data_decadal_pelagic_temperature$diff_baseline %>% range

# Maps
map_pelagic_temperature_contemporary <- draw_map(data_decadal_pelagic_temperature, "contemporary", NA, "Contemporary temperature during transport", fill_range=c(15.3, 22.83), "value","Temperature\n(°C)", "d")
map_pelagic_temperature_future <- draw_map(data_decadal_pelagic_temperature, "future", NA, "Future temperature during transport", fill_range=c(15.3, 22.83), "value","Temperature\n(°C)", "e")
map_pelagic_temperature_difference <- draw_map(data_decadal_pelagic_temperature, "diff_baseline", "pvalue_baseline", "Temperature difference during transport", fill_range=c(-1.07, 2.6), "difference","Δ temperature\n(°C)", "f")

# Density plots
density_pelagic_temperature_contemporary <- draw_density(data_decadal_pelagic_temperature, "contemporary", "Contemporary temperature during transport", c(15.3, 22.83), "Temperature (°C)", "d")
density_pelagic_temperature_future <- draw_density(data_decadal_pelagic_temperature, "future", "Future temperature during transport", c(15.3, 22.83), "Temperature (°C)", "e")
density_pelagic_temperature_difference <- draw_density(data_decadal_pelagic_temperature, "diff_baseline", "Temperature difference temperature during transport", c(-1.07, 2.6), "Δ temperature (°C)", "f")



################################################################################
# Spawning onset
################################################################################

load(paste0("output/Data_figure_spawning_onset_RCP",scen,".RData"))

# Stats
data_decadal_so$contemporary %>% quantile(c(0.025, 0.5, 0.975))
data_decadal_so$future %>% quantile(c(0.025, 0.5, 0.975))
data_decadal_so$diff %>% quantile(c(0.025, 0.5, 0.975))
format(as.Date(107,origin = "2020-12-31"), "%d/%m")
data_decadal_so$contemporary %>% range
data_decadal_so$future %>% range
data_decadal_so$diff %>% range

# Maps
map_so_contemporary <- draw_map(data_decadal_so, "contemporary", NA, "Contemporary spawning onset", fill_range=c(48, 161), map_type="value", legend_title="Spawning\nonset\n(day of year)", panel_label="a")
map_so_future <- draw_map(data_decadal_so, "future", NA, "Future spawning onset", c(48, 161), "value","Spawning\nonset\n(day of year)", "b")
map_so_difference <- draw_map(data_decadal_so, "diff", "pvalue", "Difference in spawning onset", c(-60, -3), "difference", "Δ spawning\nonset\n(days)", "c")

# Density plots
density_so_contemporary <- draw_density(data_decadal_so, "contemporary", "contemporary spawning onset", c(48, 162), "Spawning onset (day of year)", "d")
density_so_future <- draw_density(data_decadal_so, "future", "Future spawning onset", c(48, 162), "Spawning onset (day of year)", "e")
density_so_difference <- draw_density(data_decadal_so, "diff", "Difference in spawning onset", c(-60, -4), "Δ spawning onset (days)", "f")



################################################################################
# PD
################################################################################

load(paste0("output/Data_figure_PD_RCP",scen,".RData"))

# Stats
data_decadal_PD$contemporary %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_PD$future_baseline %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_PD$diff_baseline %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_PD$future_climate_responsive %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_PD$diff_climate_responsive %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_PD$contemporary %>% range
data_decadal_PD$future_baseline %>% range
data_decadal_PD$future_climate_responsive %>% range
data_decadal_PD$diff_baseline %>% range
data_decadal_PD$diff_climate_responsive %>% range

# Maps
map_PD_contemporary <- draw_map(data_decadal_PD, "contemporary", NA, "Contemporary PD", fill_range=c(11, 20), "value", legend_title="PD\n(days)", panel_label="a")
map_PD_future_baseline <- draw_map(data_decadal_PD, "future_baseline", NA, "Future PD (baseline phenology)", fill_range=c(11, 20), "value", legend_title="PD\n(days)", panel_label="b")
map_diff_baseline_PD <- draw_map(data_decadal_PD, "diff_baseline", "pvalue_baseline", "Difference in PD (baseline phenology)", fill_range=c(-3.1, 1.7), "difference", legend_title="Δ PD\n(days)", panel_label="c")
map_PD_future_climate_responsive <- draw_map(data_decadal_PD, "future_climate_responsive", NA, "Future PD (climate-responsive phenology)", fill_range=c(11, 20), "value", legend_title="PD\n(days)", panel_label="d")
map_diff_climate_responsive_PD <- draw_map(data_decadal_PD, "diff_climate_responsive", "pvalue_climate_responsive", "Difference in PD (climate-responsive phenology)", fill_range=c(-3.1, 1.7), "difference","Δ PD\n(days)", "e")

# Biplot
biplot_PD <- draw_biplot(data_decadal_PD,"diff_baseline","diff_climate_responsive","diff_climate_responsive",col_limits=c(-3.1, 1.7),
                                        "Δ PD\n(baseline)","Δ PD\n(climate-responsive)","f",-4.25,c(-3.1, 1.7))

# Density plots
density_PD_contemporary <- draw_density(data_decadal_PD, "contemporary", "Contemporary PD", c(11, 20), "PD (days)", "d")
density_PD_future_baseline <- draw_density(data_decadal_PD, "future_baseline", "Future PD (baseline phenology)", c(11, 20), "PD (days)", "e")
density_PD_future_climate_responsive <- draw_density(data_decadal_PD, "future_climate_responsive", "Future PD (climate-responsive phenology)", c(11, 20), "PD (days)", "f")

density_PD_diff_baseline <- draw_density(data_decadal_PD, "diff_baseline", "baseline spawning", c(-6, 4), "Δ PD (days)", "d")
density_PD_diff_climate_responsive <- draw_density(data_decadal_PD, "diff_climate_responsive", "climate_responsive spawning", c(-6, 4), "Δ PD (days)", "e")



################################################################################
# Connectivity (distance) 
################################################################################
mortality_rate <- 0.1
load(paste0("output/Data_figure_connectivity_RCP",scen,"_mu",mortality_rate*100,".RData"))

# Recreates the initial ID.polygon (from 1 to 1325 polygons)
c(1:1325) %>% .[-zero_prod_to_exclude] %>% factor -> data_decadal_distance$ID.polygon

# 95th percentile
data_decadal_distance$contemporary %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_distance$future_baseline %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_distance$diff_baseline %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_distance$future_climate_responsive %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_distance$diff_climate_responsive %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)

# Full range
data_decadal_distance$contemporary %>% range
data_decadal_distance$future_baseline %>% range
data_decadal_distance$diff_baseline %>% range
data_decadal_distance$future_climate_responsive %>% range
data_decadal_distance$diff_climate_responsive %>% range

# Maps
map_contemporary_distance <- draw_map(data_decadal_distance, "contemporary", NULL, "Contemporary dispersal distance", fill_range=c(0, 315), map_type="value", legend_title="Distance\n(km)", panel_label="a")
map_future_baseline_distance <- draw_map(data_decadal_distance, "future_baseline", NULL, "Future dispersal distance (baseline phenology)", fill_range=c(0, 315),"value","Distance\n(km)", "b")
map_future_climate_responsive_distance <- draw_map(data_decadal_distance, "future_climate_responsive", NULL, "Future dispersal distance (climate-responsive phenology)", fill_range=c(0, 315),"value","Distance\n(km)", "c")
map_diff_climate_responsive_distance <- draw_map(data_decadal_distance, "diff_climate_responsive", "pvalue_climate_responsive", "Difference in dispersal distance (climate-responsive phenology)", fill_range=c(-85, 123),"difference","Δ distance\n(km)", "e")

# Biplot
biplot_distance <- draw_biplot(data_decadal_distance,"diff_baseline","diff_climate_responsive","diff_climate_responsive",col_limits=c(-85, 123),
                                        "Δ settlement success\n(baseline)","Δ settlement success\n(climate-responsive)","l",-110,c(-85, 123))

# Density
density_baseline_distance <- draw_density(data_decadal_distance, "diff_baseline", "Warming effect (baseline spawning)", c(-100, 130), "Δ distance (km)", "a")
density_climate_responsive_distance <- draw_density(data_decadal_distance, "diff_climate_responsive", "Warming effect (climate_responsive spawning)", c(-100, 130), "Δ distance (km)", "b")
density_effect_distance <- draw_density(data_decadal_distance, "effect", "Phenology effect", c(-25, 40), "Phenology effect (km)", "c")

################################################################################
# Connectivity (total settlement success)
################################################################################
mortality_rate <- 0.1
load(paste0("output/Data_figure_connectivity_RCP",scen,"_mu",mortality_rate*100,".RData"))

# Recreates the initial ID.polygon (from 1 to 1325 polygons)
c(1:1325) %>% .[-zero_prod_to_exclude] %>% factor -> data_decadal_tss$ID.polygon

# Stats
data_decadal_tss$contemporary %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_tss$future_baseline %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_tss$diff_baseline %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_tss$future_climate_responsive %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_tss$diff_climate_responsive %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_tss$effect %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)

data_decadal_tss$contemporary %>% range
data_decadal_tss$future_baseline %>% range
data_decadal_tss$future_climate_responsive %>% range
data_decadal_tss$diff_baseline %>% range
data_decadal_tss$diff_climate_responsive %>% range

# Maps
map_contemporary_tss <- draw_map(data_decadal_tss, "contemporary", NULL, "Contemporary settlement success", fill_range=c(0, 0.35), map_type="value", legend_title="Total\nsettlement\nsuccess", panel_label="g")
map_future_baseline_tss <- draw_map(data_decadal_tss, "future_baseline", NULL, "Future settlement success (baseline phenology)", fill_range=c(0, 0.35),"value","Total\nsettlement\nsuccess", "h")
map_diff_baseline_tss <- draw_map(data_decadal_tss, "diff_baseline", "pvalue_baseline", "Difference in settlement success (baseline phenology)", fill_range=c(-0.06, 0.1), map_type="difference", legend_title="Δ total\nsettlement\nsuccess", panel_label="i")
map_future_climate_responsive_tss <- draw_map(data_decadal_tss, "future_climate_responsive", NULL, "Future settlement success (climate-responsive phenology)", fill_range=c(0, 0.35),"value","Total\nsettlement\nsuccess", "j")
map_diff_climate_responsive_tss <- draw_map(data_decadal_tss, "diff_climate_responsive", "pvalue_climate_responsive", "Difference in settlement success (climate-responsive phenology)", fill_range=c(-0.06, 0.1),"difference","Δ total\nsettlement\nsuccess", "k")

# Biplot
biplot_tss <- draw_biplot(data_decadal_tss,"diff_baseline","diff_climate_responsive","diff_climate_responsive",col_limits=c(-0.06, 0.1),
                                        "Δ settlement success\n(baseline)","Δ settlement success\n(climate-responsive)","l",-0.125,c(-0.06, 0.1))

# Density
density_baseline_tss <- draw_density(data_decadal_tss, "diff_baseline", "Warming effect (baseline spawning)", c(-0.07, 0.1), "Δ total settlement success", "d")
density_climate_responsive_tss <- draw_density(data_decadal_tss, "diff_climate_responsive", "Warming effect (climate_responsive spawning)", c(-0.07, 0.1), "Δ total settlement success", "e")
density_effect_tss <- draw_density(data_decadal_tss, "effect", "Phenology effect", c(-0.15, 0.05), "Phenology effect", "f")


################################################################################
# COMPOSE FIGURES
################################################################################

# Figure 2 
ggsave("Figure 2.pdf",
       plot= marrangeGrob(c(map_spawning_temperature_contemporary, map_spawning_temperature_future, map_spawning_temperature_difference,
                            map_pelagic_temperature_contemporary, map_pelagic_temperature_future, map_pelagic_temperature_difference),
                          nrow=3,ncol=2,
                          top=""),
       device=cairo_pdf, width=20, height=15, units="cm")

# Figure 3 for paper
ggsave("Figure 3.pdf",
       marrangeGrob(c(map_so_contemporary, map_so_future, map_so_difference),
                    nrow=3,ncol=1,
                    top=""),
       device=cairo_pdf, width=10, height=15, units="cm")

# Figure 4 for paper
ggsave("Figure 4.pdf",
       marrangeGrob(c(map_PD_contemporary, map_PD_future_baseline, map_diff_baseline_PD,
                      map_PD_future_climate_responsive, map_diff_climate_responsive_PD, biplot_PD,
                      map_contemporary_tss, map_future_baseline_tss, map_diff_baseline_tss,
                      map_future_climate_responsive_tss, map_diff_climate_responsive_tss, biplot_tss),
                    nrow=6,ncol=2,
                    top=""),
       device=cairo_pdf, width=20, height=30, units="cm")

# Figure S2 :
# Figure supplementary distance :
ggsave("Figure S2.png",
       marrangeGrob(c(map_contemporary_distance, map_future_baseline_distance, map_future_climate_responsive_distance),
                    nrow=3,ncol=1,
                    top=""),
       device=png, width=10, height=15, units="cm", res=600)


# Figure 5
op<-par(mfrow=c(1,2))
x <- (exp(-mortality_rate*data_decadal_PD$future_baseline) - exp(-mortality_rate*data_decadal_PD$contemporary))
y <- (data_decadal_tss$future_baseline - data_decadal_tss$contemporary)
plot(x, y, xlab="Δ Cumulative survival", ylab="Δ Total settlement success", main="Difference\n(baseline phenology)",pch=16,col=alpha("gray60",0.5)) 
abline(a=0,b=1,lty=2)
mod <- lm(y~x)
abline(coef(mod), lty=1,col="lightblue")
mtext("(a)", side = 3, line = 2, adj = -0.25, cex=1.5)
summary(mod)
x <- (exp(-mortality_rate*data_decadal_PD$future_climate_responsive) - exp(-mortality_rate*data_decadal_PD$contemporary))
y <- (data_decadal_tss$future_climate_responsive - data_decadal_tss$contemporary)
plot(x, y, xlab="Δ Cumulative survival", ylab="Δ Total settlement success", main="Difference\n(climate-responsive phenology)",pch=16,col=alpha("gray60",0.5)) 
abline(a=0,b=1,lty=2)
mod <- lm(y~x)
abline(coef(mod), lty=1,col="lightblue")
mtext("(b)", side = 3, line = 2, adj = -0.3, cex=1.5)
summary(mod)
