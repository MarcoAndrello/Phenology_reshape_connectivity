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

################################################################################
# CONNECTIVITY (DISPERSAL DISTANCE AND TOTAL SETTLEMENT SUCCESS)
################################################################################

load(paste0("data_connectivity_",species,"_mu01.RData"))

data %>% select(ID.polygon, distance, coastal_retention, year, i.decade, scen_spawning) %>%
  mutate(decade = case_when(i.decade == 1 & scen_spawning == 1 ~ "present",
                            i.decade == 2 & scen_spawning == 1 ~ "future_fixed",
                            i.decade == 2 & scen_spawning == 2 ~ "future_variable")) -> data_yearly_long

# calculate p-value of the comparison
pvalue <- list() # on metrics/stat
## Define pvalue
pvalue[[1]] <- pvalue[[2]] <- array(NA,dim=c(length(levels(data_yearly_long$ID.polygon)), # 1313 polygons
                                             2))                                     # 2 scenarios: future fixed and future variable; 
# Loop on the two scenarios and polygons
i.scen <- 1
for (i.scen in 1 : 2) {
  if(i.scen == 1) data_yearly_long %>%
    filter(decade %in% c("present","future_fixed")) %>%
    mutate(decade = factor(decade, levels=c("present","future_fixed"))) -> dat1
  if(i.scen == 2) data_yearly_long %>%
    filter(decade %in% c("present","future_variable")) %>%
    mutate(decade = factor(decade, levels=c("present","future_variable"))) -> dat1
  i.pol <- 268
  for (i.pol in 1 : 1313) {
    if (i.pol %% 100 == 0) cat(i.scen, i.pol,"\n"); flush.console()
    dat2 <- filter(dat1, ID.polygon==i.pol)
    if (dim(dat2)[1] == 0) next
    if (!check.NA(dat2,"distance")) {
      mod <- lm(distance ~ decade,dat2, na.action = na.omit)
      pvalue[[1]][i.pol,i.scen] <- summary(mod)$coefficients[2,4]
    }
    if (!check.NA(dat2,"coastal_retention")) {
      mod <- lm(coastal_retention ~ decade,dat2, na.action = na.omit)
      pvalue[[2]][i.pol,i.scen] <- summary(mod)$coefficients[2,4]
    }
    
  }
  
}
for (i in 1 : length(pvalue)){
  pvalue[[i]] <- as.data.frame(pvalue[[i]])
  names(pvalue[[i]]) <- c("future_fixed","future_variable")
  pvalue[[i]]$ID.polygon <- factor(levels(data_yearly_long$ID.polygon))
}
names(pvalue) <- v.stat

# Create data decadal, for plotting Figure 4
# Distance
data_yearly_long %>%
  select(-coastal_retention) %>%
    group_by(ID.polygon, decade) %>%
  # mean value per polygon  (mean decadal)
  summarise(distance = mean(distance, na.rm=T)) %>%
  pivot_wider(values_from=distance, names_from=decade) %>%
  # differences
  mutate(diff_fixed = future_fixed - present,
         diff_variable = future_variable - present) %>%
  # pvalues
  left_join(pvalue[[1]] %>% rename(pvalue_fixed = future_fixed,
                              pvalue_variable = future_variable)) -> data_decadal_distance
# Coastal retention
data_yearly_long %>%
  select(-distance) %>%
  group_by(ID.polygon, decade) %>%
  summarise(coastal_retention = mean(coastal_retention, na.rm=T)) %>%
  pivot_wider(values_from=coastal_retention, names_from=decade) %>%
  mutate(diff_fixed = future_fixed - present,
         diff_variable = future_variable - present) %>%
  left_join(pvalue[[2]] %>% rename(pvalue_fixed = future_fixed,
                                   pvalue_variable = future_variable)) -> data_decadal_coastal_retention

data_decadal_distance %>% mutate(effect = diff_variable - diff_fixed) -> data_decadal_distance
data_decadal_coastal_retention %>% mutate(effect = diff_variable - diff_fixed) -> data_decadal_coastal_retention
save(data_decadal_distance, data_decadal_coastal_retention, file="Data_figure_connectivity.RData")

# Stats
data_decadal_distance$diff_fixed %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
# data_decadal_distance$diff_fixed %>% range(na.rm=T)#quantile(c(0.01, 0.5, 0.99),na.rm=T)
data_decadal_distance$diff_variable %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_distance$effect %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_coastal_retention$diff_fixed %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
# data_decadal_coastal_retention$diff_fixed %>% quantile(c(0.01, 0.5, 0.99),na.rm=T)
data_decadal_coastal_retention$diff_variable %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_coastal_retention$diff_variable %>% quantile(c(0.01, 0.5, 0.99),na.rm=T)
data_decadal_coastal_retention$effect %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_coastal_retention$effect %>% quantile(c(0.001, 0.5, 0.99),na.rm=T)

################################################################################
# COMPOSE FIGURE 4
################################################################################
load("Data_figure_connectivity.RData")

# Ricrea l'ID.polygon iniziale (da 1 a 1325 poligoni)
c(1:1325) %>% .[-zero_prod_to_exclude] %>% factor -> data_decadal_distance$ID.polygon
c(1:1325) %>% .[-zero_prod_to_exclude] %>% factor -> data_decadal_coastal_retention$ID.polygon

# Distance
map_fixed_distance <- draw_map(data_decadal_distance, "diff_fixed", "pvalue_fixed", "Warming effect (fixed spawning)", fill_range=c(-100, 130), map_type="difference", legend_title="Δ distance\n(km)", panel_label="a")
map_variable_distance <- draw_map(data_decadal_distance, "diff_variable", "pvalue_variable", "Warming effect (variable spawning)", fill_range=c(-100, 130),"difference","Δ distance\n(km)", "b")
map_effect_distance  <- draw_map(data_decadal_distance, "effect", "pvalue_variable", "Phenology effect", fill_range=c(-25, 40),"effect","Phenology\neffect (km)", "c")
# Coastal retention
map_fixed_coastal_retention <- draw_map(data_decadal_coastal_retention, "diff_fixed", "pvalue_fixed", "Warming effect (fixed spawning)", fill_range=c(-0.07, 0.1), map_type="difference", legend_title="Δ total\nsettlement\nsuccess", panel_label="d")
map_variable_coastal_retention <- draw_map(data_decadal_coastal_retention, "diff_variable", "pvalue_variable", "Warming effect (variable spawning)", fill_range=c(-0.07, 0.1),"difference","Δ total\nsettlement\nsuccess", "e")
map_effect_coastal_retention  <- draw_map(data_decadal_coastal_retention, "effect", "pvalue_variable", "Phenology effect", fill_range=c(-0.15, 0.05),"effect","Phenology\neffect", "f")

# ggsave("Figure 4.png",
ggsave("Figure 4.pdf",
       marrangeGrob(c(map_fixed_distance, map_variable_distance, map_effect_distance,
                      map_fixed_coastal_retention, map_variable_coastal_retention, map_effect_coastal_retention),
                    nrow=3,ncol=2,
                    top=""),
       device=cairo_pdf, width=20, height=15, units="cm", dpi=600)


# Density plots
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



################################################################################
# COMPOSE SUPPLEMENTARY FIGURE: present values
################################################################################
load("Data_figure_connectivity.RData")

data_decadal_distance$present %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_distance$future_fixed %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
# data_decadal_distance$future_fixed %>% quantile(c(0.01, 0.5, 0.99),na.rm=T)
data_decadal_distance$future_variable %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_coastal_retention$present %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_coastal_retention$future_fixed %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)
data_decadal_coastal_retention$future_variable %>% quantile(c(0.025, 0.5, 0.975),na.rm=T)

# Distance
map_present_distance <- draw_map(data_decadal_distance, "present", NULL, "Dispersal distance (present)", fill_range=c(0, 180), map_type="value", legend_title="Distance (km)", panel_label="a")
map_future_fixed_distance <- draw_map(data_decadal_distance, "future_fixed", NULL, "Dispersal distance (future, fixed spawning)", fill_range=c(0, 180),"value","Distance (km)", "b")
map_future_variable_distance <- draw_map(data_decadal_distance, "future_variable", NULL, "Dispersal distance (future, variable spawning)", fill_range=c(0, 180),"value","Distance (km)", "c")
# Coastal retention
map_present_coastal_retention <- draw_map(data_decadal_coastal_retention, "present", NULL, "Total settlement success (present)", fill_range=c(0, 0.25), map_type="value", legend_title="Total\nsettlement\nsuccess", panel_label="d")
map_future_fixed_coastal_retention <- draw_map(data_decadal_coastal_retention, "future_fixed", NULL, "Total settlement success (future, fixed spawning)", fill_range=c(0, 0.25),"value","Total\nsettlement\nsuccess", "e")
map_future_variable_coastal_retention <- draw_map(data_decadal_coastal_retention, "future_variable", NULL, "Total settlement success (future, variable spawning)", fill_range=c(0, 0.25),"value","Total\nsettlement\nsuccess", "f")

ggsave("Figure S1.png",
       marrangeGrob(c(map_present_distance, map_future_fixed_distance, map_future_variable_distance,
                      map_present_coastal_retention, map_future_fixed_coastal_retention, map_future_variable_coastal_retention),
                    nrow=3,ncol=2,
                    top=""),
       device="png", bg="white", width=20, height=15, units="cm", dpi=600)









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
