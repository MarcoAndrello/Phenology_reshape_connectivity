# Plot figures

rm(list=ls())

library(tidyverse)
library(sf)

# Functions
source("00 - Functions.R")


################################################################################
# Figure 2 - MAP - Adult temperature and larval temperatures
################################################################################
load("Data_figure_larval_temp.RData")
load("Data_figure_adult_temperature.RData")

map_adult_present <- draw_map(data_decadal_adult_temperature, "present", NA, "Present temperature in spawning sites", fill_range=c(15, 26), map_type="value", legend_title="Temperature (°C)", panel_label="a")
map_adult_future <- draw_map(data_decadal_adult_temperature, "future", NA, "Future temperature in spawning sites", c(15, 26), "value","Temperature (°C)", "b")
map_adult_difference <- draw_map(data_decadal_adult_temperature, "diff", "pvalue", "Temperature difference in spawning sites", c(1, 3.6), "difference", "Δ temperature (°C)", "c")

map_larval_present <- draw_map(data_decadal_larval_temperature, "present", NA, "Present temperature over larval trajectories", fill_range=c(15, 26), "value","Temperature (°C)", "d")
map_larval_future <- draw_map(data_decadal_larval_temperature, "future", NA, "Future temperature over larval trajectories", fill_range=c(15, 26), "value","Temperature (°C)", "e")
map_larval_difference <- draw_map(data_decadal_larval_temperature, "diff_fixed", "pvalue_fixed", "Temperature difference over larval trajectories", fill_range=c(-6, 6), "difference","Δ temperature (°C)", "f")

ggsave("Figure 2.pdf",
       plot= marrangeGrob(c(map_adult_present, map_adult_future, map_adult_difference,
                            map_larval_present, map_larval_future, map_larval_difference),
                          nrow=3,ncol=2,
                          top=""),
       device=cairo_pdf, width=20, height=15, units="cm", dpi=600)


################################################################################
# Figure 3 - MAP and BIPLOT - Spawning onset and PLD
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

ggsave("Figure 3.pdf",
       marrangeGrob(c(map_so_present, map_so_future, map_so_difference, map_PLD_fixed, map_PLD_variable, biplot),
                    nrow=3,ncol=2,
                    top=""),
       device=cairo_pdf, width=20, height=15, units="cm", dpi=600)


################################################################################
# FIGURE 4 - MAP and BIPLOT - Dispersal distance and total settlement success (future - present differences)
################################################################################
load("Data_figure_connectivity.RData")

# Ricrea l'ID.polygon iniziale (da 1 a 1325 poligoni)
c(1:1325) %>% .[-zero_prod_to_exclude] %>% factor -> data_decadal_distance$ID.polygon
c(1:1325) %>% .[-zero_prod_to_exclude] %>% factor -> data_decadal_coastal_retention$ID.polygon

# Distance
map_fixed_distance <- draw_map(data_decadal_distance, "diff_fixed", "pvalue_fixed", "Warming effect (fixed spawning)", fill_range=c(-100, 130), map_type="difference", legend_title="Δ distance\n(km)", panel_label="a")
map_variable_distance <- draw_map(data_decadal_distance, "diff_variable", "pvalue_variable", "Warming effect (variable spawning)", fill_range=c(-100, 130),"difference","Δ distance\n(km)", "b")
map_effect_distance  <- draw_map(data_decadal_distance, "effect", "pvalue_variable", "Phenology effect", fill_range=c(-25, 40),"effect","Phenology\neffect (km)", "c")
# Total settlement success (aka Coastal retention)
map_fixed_coastal_retention <- draw_map(data_decadal_coastal_retention, "diff_fixed", "pvalue_fixed", "Warming effect (fixed spawning)", fill_range=c(-0.07, 0.1), map_type="difference", legend_title="Δ total\nsettlement\nsuccess", panel_label="d")
map_variable_coastal_retention <- draw_map(data_decadal_coastal_retention, "diff_variable", "pvalue_variable", "Warming effect (variable spawning)", fill_range=c(-0.07, 0.1),"difference","Δ total\nsettlement\nsuccess", "e")
map_effect_coastal_retention  <- draw_map(data_decadal_coastal_retention, "effect", "pvalue_variable", "Phenology effect", fill_range=c(-0.15, 0.05),"effect","Phenology\neffect", "f")

ggsave("Figure 4.pdf",
       marrangeGrob(c(map_fixed_distance, map_variable_distance, map_effect_distance,
                      map_fixed_coastal_retention, map_variable_coastal_retention, map_effect_coastal_retention),
                    nrow=3,ncol=2,
                    top=""),
       device=cairo_pdf, width=20, height=15, units="cm", dpi=600)



################################################################################
# FIGURE S1 - MAP - Dispersal distance and total settlement success (present and future values)
################################################################################
load("Data_figure_connectivity.RData")

map_present_distance <- draw_map(data_decadal_distance, "present", NULL, "Dispersal distance (present)", fill_range=c(0, 180), map_type="value", legend_title="Distance (km)", panel_label="a")
map_future_fixed_distance <- draw_map(data_decadal_distance, "future_fixed", NULL, "Dispersal distance (future, fixed spawning)", fill_range=c(0, 180),"value","Distance (km)", "b")
map_future_variable_distance <- draw_map(data_decadal_distance, "future_variable", NULL, "Dispersal distance (future, variable spawning)", fill_range=c(0, 180),"value","Distance (km)", "c")

map_present_coastal_retention <- draw_map(data_decadal_coastal_retention, "present", NULL, "Total settlement success (present)", fill_range=c(0, 0.25), map_type="value", legend_title="Total\nsettlement\nsuccess", panel_label="d")
map_future_fixed_coastal_retention <- draw_map(data_decadal_coastal_retention, "future_fixed", NULL, "Total settlement success (future, fixed spawning)", fill_range=c(0, 0.25),"value","Total\nsettlement\nsuccess", "e")
map_future_variable_coastal_retention <- draw_map(data_decadal_coastal_retention, "future_variable", NULL, "Total settlement success (future, variable spawning)", fill_range=c(0, 0.25),"value","Total\nsettlement\nsuccess", "f")

ggsave("Figure S1.png",
       marrangeGrob(c(map_present_distance, map_future_fixed_distance, map_future_variable_distance,
                      map_present_coastal_retention, map_future_fixed_coastal_retention, map_future_variable_coastal_retention),
                    nrow=3,ncol=2,
                    top=""),
       device="png", bg="white", width=20, height=15, units="cm", dpi=600)



################################################################################
# Figure S2 - DENSITY DISTRIBUTION - Adult temperature and larval temperatures
################################################################################
load("Data_figure_larval_temp.RData")
load("Data_figure_adult_temperature.RData")

density_adult_present <- draw_density(data_decadal_adult_temperature, "present", "Present temperature in spawning sites", c(15, 26), "Temperature (°C)", "a")
density_adult_future <- draw_density(data_decadal_adult_temperature, "future", "Future temperature in spawning sites", c(15, 26), "Temperature (°C)", "b")
density_adult_difference <- draw_density(data_decadal_adult_temperature, "diff", "Temperature difference in spawning sites", c(1, 3.6), "Δ temperature (°C)", "c")

density_larval_present <- draw_density(data_decadal_larval_temperature, "present", "Present temperature over larval trajectories", c(15, 26), "Temperature (°C)", "d")
density_larval_future <- draw_density(data_decadal_larval_temperature, "future", "Future temperature over larval trajectories", c(15, 26), "Temperature (°C)", "e")
density_larval_difference <- draw_density(data_decadal_larval_temperature, "diff_fixed", "Temperature difference over larval trajectories", c(-6, 6), "Δ temperature (°C)", "f")

ggsave("Figure S2.png",
       marrangeGrob(c(density_adult_present, density_adult_future, density_adult_difference,
                      density_larval_present, density_larval_future, density_larval_difference),
                    nrow=3,ncol=2,
                    top=""),
       device=png, bg="white", width=20, height=15, units="cm", dpi=600)



################################################################################
# Figure S3 - DENSITY DISTRIBUTION - Spawning onset and PLD
################################################################################
load("Data_figure_spawning_onset.RData")
load("Data_figure_PLD.RData")

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



################################################################################
# FIGURE S4 - DENSITY DISTRIBUTION - Dispersal distance and total settlement success (future - present differences)
################################################################################
load("Data_figure_connectivity.RData")

density_fixed_distance <- draw_density(data_decadal_distance, "diff_fixed", "Warming effect (fixed spawning)", c(-100, 130), "Δ distance (km)", "a")
density_variable_distance <- draw_density(data_decadal_distance, "diff_variable", "Warming effect (variable spawning)", c(-100, 130), "Δ distance (km)", "b")
density_effect_distance <- draw_density(data_decadal_distance, "effect", "Phenology effect", c(-25, 40), "Phenology effect (km)", "c")

density_fixed_coastal_retention <- draw_density(data_decadal_coastal_retention, "diff_fixed", "Warming effect (fixed spawning)", c(-0.07, 0.1), "Δ total settlement success", "d")
density_variable_coastal_retention <- draw_density(data_decadal_coastal_retention, "diff_variable", "Warming effect (variable spawning)", c(-0.07, 0.1), "Δ total settlement success", "e")
density_effect_coastal_retention <- draw_density(data_decadal_coastal_retention, "effect", "Phenology effect", c(-0.15, 0.05), "Phenology effect", "f")

ggsave("Figure S4.png",
       marrangeGrob(c(density_fixed_distance, density_variable_distance, density_effect_distance,
                      density_fixed_coastal_retention, density_variable_coastal_retention, density_effect_coastal_retention),
                    nrow=3,ncol=2,
                    top=""),
       device="png", bg="white", width=20, height=15, units="cm", dpi=600)