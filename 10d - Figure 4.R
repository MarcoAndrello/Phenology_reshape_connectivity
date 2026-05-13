rm(list=ls())

library(tidyverse)
source("Functions - Map.R")

# Functions
check.NA <- function(x,variable) {
  x %>% filter(decade == levels(x$decade)[1]) %>% pull(variable) %>% is.nan %>% all -> check.1
  x %>% filter(decade == levels(x$decade)[1]) %>% pull(variable) %>% is.na %>% all -> check.2
  x %>% filter(decade == levels(x$decade)[2]) %>% pull(variable) %>% is.nan %>% all -> check.3
  x %>% filter(decade == levels(x$decade)[2]) %>% pull(variable) %>% is.na %>% all -> check.4
  return(any(check.1, check.2, check.3, check.4))
}

# Load id of zero productive sites
load("zero_prod_to_exclude.RData")

v.species <- c("Diplodus_sargus","Diplodus_vulgaris","Paracentrotus_lividus")

i.species <- 1
# v.stat <- c("spawning_onset","mean_PLD_fixed_release", "mean_PLD_variable_release")
v.stat <- c("distance","coastal_retention")

# for (i.species in 1 : 3) {
species <- v.species[i.species]



# ################################################################################
# # COMPOSE FIGURE BIPLOT: NOT USED
# ################################################################################
# 
# biplot_distance <- ggplot(data_decadal_distance) +
#   geom_point(aes(x=diff_fixed, y=diff_variable, col=effect), size=0.1, alpha = 0.6) +
#   scale_color_continuous_c4a_div("gmt.polar",
#                                  limits=c(-70, 110)) +
#   xlab("Δ distance (fixed spawning)") + ylab("Δ distance (variable spawning)") +
#   labs(title = "Distance", col="Phenology\neffect") +
#   geom_hline(yintercept=0,linewidth=0.1) +
#   geom_vline(xintercept=0,linewidth=0.1) +
#   geom_abline(intercept=0, slope=1, linetype="dashed",linewidth=0.1) +
#   theme_biplot() +
#   annotate("text",
#            x = -250, y = Inf,
#            label = "a",
#            hjust = 0, vjust = 1.5,
#            size = 4) +  
#   coord_cartesian(xlim = c(-150, 150), clip = "off")
# biplot_distance
# 
# biplot_coastal_retention <- ggplot(data_decadal_coastal_retention) +
#   geom_point(aes(x=diff_fixed, y=diff_variable, col=effect), size=0.1, alpha = 0.6) +
#   scale_color_continuous_c4a_div("gmt.polar",
#                                  limits=c(-0.16, 0.06)) +
#   xlab("Δ coastal retention (fixed spawning)") + ylab("Δ coastal retention (variable spawning)") +
#   labs(title = "Coastal retention", col="Phenology\neffect") +
#   geom_hline(yintercept=0,linewidth=0.1) +
#   geom_vline(xintercept=0,linewidth=0.1) +
#   geom_abline(intercept=0, slope=1, linetype="dashed",linewidth=0.1) +
#   theme_biplot() +
#   annotate("text",
#            x = -0.25, y = Inf,
#            label = "b",
#            hjust = 0, vjust = 1.5,
#            size = 4) +  
#   coord_cartesian(xlim = c(-0.15, 0.16), clip = "off")
# biplot_coastal_retention
# 
# ggsave("Figure CONNECTIVITY BIPLOT.png",
#        marrangeGrob(c(biplot_distance, biplot_coastal_retention),
#                     nrow=1,ncol=2,
#                     top="Phenology effects on projected changes in connectivity"),
#        device="png", bg="white", width=20, height=10, units="cm", dpi=600)
