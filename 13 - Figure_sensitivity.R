# 13 - Figure sensitivity analysis

rm(list=ls())

library(tidyverse)
library(ggh4x)
library(gridExtra)

# Theme for plots
theme_sensitivity <- function() {
  theme_light() + 
    theme(
      legend.position="right",
      plot.title = element_text(hjust=0.5, size=9),
      legend.title=element_text(size=8, hjust=0.5),
      legend.text=element_text(size=8),
      strip.text = element_text(size = 8), 
      plot.margin=unit(c(0.25,0.25,0.5,0.25),"cm"),
      axis.title.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.x=element_text(size=8),
      axis.title.y=element_text(size=8),
      axis.text.y=element_text(size=8),
      legend.margin=margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
    )
}

# Temperature spawning sites

# Bind the RCP45 and the RCP85 dataframes
load("output/Data_figure_spawning_temperature_RCP45.RData")
data_decadal_spawning_temperature %>%
  mutate(RCP="RCP45") ->
  data1
load("output/Data_figure_spawning_temperature_RCP85.RData")
data_decadal_spawning_temperature %>%
  mutate(RCP="RCP85") %>%
  rbind(data1) -> data2
rm(data1, data_decadal_spawning_temperature)
# Pivot longer
data2 %>% rename(difference=diff) %>%
pivot_longer(cols=c(contemporary, future, difference)) %>%
  mutate(name=factor(name,levels=c("contemporary","future","difference"))) %>%
  rename(Temperature=value) -> data
data %>%
  ggplot(aes(x = RCP, y = Temperature)) +
  geom_violin(colour = "grey25",fill="gray75",linewidth = 0.4) +
  facet_wrap2(vars(name), scales = "free_y") +
  facetted_pos_scales(y = list(name == "contemporary" ~ scale_y_continuous(limits = c(15, 26)),
                               name == "future" ~ scale_y_continuous(limits = c(15, 26)))) +
  labs(title="Temperature in spawning sites", y="Temperature °C") +
  theme_sensitivity()  ->
  sens_spawning_temp



# Temperature during transport

# Bind the RCP45 and the RCP85 dataframes
load("output/Data_figure_pelagic_temp_RCP45.RData")
data_decadal_pelagic_temperature %>%
  mutate(RCP="RCP45") ->
  data1
load("output/Data_figure_pelagic_temp_RCP85.RData")
data_decadal_pelagic_temperature %>%
  mutate(RCP="RCP85") %>%
  rbind(data1) -> data2
rm(data1, data_decadal_pelagic_temperature)
# Pivot longer
data2 %>% rename(difference=diff_baseline) %>%
  pivot_longer(cols=c(contemporary, future, difference)) %>%
  mutate(name=factor(name,levels=c("contemporary","future","difference"))) %>%
  rename(Temperature=value) -> data
data %>% 
  ggplot(aes(x=RCP,y=Temperature)) + 
  geom_violin(colour = "grey25",fill="gray75",linewidth = 0.4) +
  facet_wrap2(vars(name), scales="free_y") +
    facetted_pos_scales(y = list(name == "contemporary" ~ scale_y_continuous(limits = c(15, 25), labels = \(x) sprintf("%.0f", x)),
                                 name == "future" ~ scale_y_continuous(limits = c(15, 25), labels = \(x) sprintf("%.0f", x)))) +
  labs(title="Temperature during transport", y="Temperature °C") +
    theme_sensitivity() ->
  sens_dispersal_temp



# Spawning onset

# Bind the RCP45 and the RCP85 dataframes
load("output/Data_figure_spawning_onset_RCP45.RData")
data_decadal_so %>%
  mutate(RCP="RCP45") ->
  data1
load("output/Data_figure_spawning_onset_RCP85.RData")
data_decadal_so %>%
  mutate(RCP="RCP85") %>%
  rbind(data1) -> data2
rm(data1, data_decadal_so)
# Pivot longer
data2 %>% rename(difference=diff) %>%
  pivot_longer(cols=c(contemporary, future, difference)) %>%
  mutate(name=factor(name,levels=c("contemporary","future","difference"))) %>%
  rename(Day=value) -> data
data %>% 
  ggplot(aes(x=RCP,y=Day)) + 
  geom_violin(colour = "grey25", fill="gray75", linewidth = 0.4) +
  facet_wrap2(vars(name), scales="free_y") +
    facetted_pos_scales(y = list(name == "contemporary" ~ scale_y_continuous(limits = c(40, 160)),
                                 name == "future" ~ scale_y_continuous(limits = c(40, 160))))+
  labs(title="Spawning onset", y="Day of year") +
    theme_sensitivity() ->
  sens_so


       
# PD

# Bind the RCP45 and the RCP85 dataframes
load("output/Data_figure_PD_RCP45.RData")
data_decadal_PD %>%
  mutate(RCP="RCP45") ->
  data1
load("output/Data_figure_PD_RCP85.RData")
data_decadal_PD %>%
  mutate(RCP="RCP85") %>%
  rbind(data1) -> data2
rm(data1, data_decadal_PD)
# Pivot longer
data2 %>%
  rename(future_responsive=future_climate_responsive, difference_baseline=diff_baseline, difference_responsive=diff_climate_responsive) %>%
  pivot_longer(cols=c(contemporary, future_baseline, future_responsive, difference_baseline, difference_responsive)) %>%
  mutate(name=factor(name,levels=c("contemporary","future_baseline","future_responsive", "difference_baseline", "difference_responsive"))) %>%
  rename(PD=value) -> data
data %>% 
    ggplot(aes(x=RCP,y=PD)) + 
    geom_violin(colour = "grey25", fill="gray75", linewidth = 0.4) +
    facet_wrap2(vars(name), scales="free_y", ncol=5) +
    facetted_pos_scales(y = list(name == "contemporary" ~ scale_y_continuous(limits = c(9.5, 20)),
                                 name == "future_baseline" ~ scale_y_continuous(limits = c(9.5, 20)),
                                 name == "future_responsive" ~ scale_y_continuous(limits = c(9.5, 20)),
                                 name == "difference_baseline" ~ scale_y_continuous(limits = c(-6, 3)),
                                 name == "difference_responsive" ~ scale_y_continuous(limits = c(-6, 3)))) +
    labs(title="Pelagic duration (PD)", y="PD (days)") +
    # Trick to have an empty legend space on the right
    geom_point(aes(fill = ""), alpha = 0) +
    scale_fill_manual(name = NULL, values = "white", breaks = "", labels = "") +
    theme_sensitivity() +
    theme(legend.position = "right",
          legend.key.width = grid::unit(1, "cm"),
          legend.key.height = grid::unit(1, "cm")) ->
    sens_PD




# Connectivity: total settlement success

load("output/Data_figure_connectivity_RCP45_mu2.RData")
data_decadal_tss %>%
  mutate(RCP="RCP45", mu=0.02) ->
  data_temp
load("output/Data_figure_connectivity_RCP45_mu10.RData")
data_decadal_tss %>%
  mutate(RCP="RCP45", mu=0.10) %>%
  rbind(data_temp) -> data_temp
load("output/Data_figure_connectivity_RCP45_mu20.RData")
data_decadal_tss %>%
  mutate(RCP="RCP45", mu=0.20) %>%
  rbind(data_temp) -> data_temp
load("output/Data_figure_connectivity_RCP85_mu2.RData")
data_decadal_tss %>%
  mutate(RCP="RCP85", mu=0.02) %>%
  rbind(data_temp) -> data_temp
load("output/Data_figure_connectivity_RCP85_mu10.RData")
data_decadal_tss %>%
  mutate(RCP="RCP85", mu=0.10) %>%
  rbind(data_temp) -> data_temp
load("output/Data_figure_connectivity_RCP85_mu20.RData")
data_decadal_tss %>%
  mutate(RCP="RCP85", mu=0.20) %>%
  rbind(data_temp) -> data_temp

# Pivot longer
data_temp %>%
  rename(future_responsive=future_climate_responsive, difference_baseline=diff_baseline, difference_responsive=diff_climate_responsive) %>%
  pivot_longer(cols=c(contemporary, future_baseline, future_responsive, difference_baseline, difference_responsive)) %>%
  mutate(name=factor(name,levels=c("contemporary","future_baseline","future_responsive", "difference_baseline", "difference_responsive")),
         mu = factor(mu)) %>%
  rename(settlement_success=value) -> data
data %>% 
    ggplot(aes(x=RCP,y=settlement_success, fill=mu)) + 
    geom_violin(colour = "grey25",linewidth = 0.4, position = position_dodge(width = 1)) +
    facet_wrap2(vars(name), scales="free_y", ncol=5) +
    facetted_pos_scales(y = list(name == "contemporary" ~ scale_y_continuous(limits = c(0, 0.85)),
                                 name == "future_baseline" ~ scale_y_continuous(limits = c(0, 0.85)),
                                 name == "future_responsive" ~ scale_y_continuous(limits = c(0, 0.85)),
                                 name == "difference_baseline" ~ scale_y_continuous(limits = c(-0.4, 0.4)),
                                 name == "difference_responsive" ~ scale_y_continuous(limits = c(-0.4, 0.4)))) +
  labs(title="Total settlement success", y="Settlement success", fill="μ") +
  theme_sensitivity() ->
  sens_settlement



# Connectivity: distance

load("output/Data_figure_connectivity_RCP45_mu2.RData")
data_decadal_distance %>%
  mutate(RCP="RCP45", mu=0.02) ->
  data_temp
load("output/Data_figure_connectivity_RCP45_mu10.RData")
data_decadal_distance %>%
  mutate(RCP="RCP45", mu=0.10) %>%
  rbind(data_temp) -> data_temp
load("output/Data_figure_connectivity_RCP45_mu20.RData")
data_decadal_distance %>%
  mutate(RCP="RCP45", mu=0.20) %>%
  rbind(data_temp) -> data_temp
load("output/Data_figure_connectivity_RCP85_mu2.RData")
data_decadal_distance %>%
  mutate(RCP="RCP85", mu=0.02) %>%
  rbind(data_temp) -> data_temp
load("output/Data_figure_connectivity_RCP85_mu10.RData")
data_decadal_distance %>%
  mutate(RCP="RCP85", mu=0.10) %>%
  rbind(data_temp) -> data_temp
load("output/Data_figure_connectivity_RCP85_mu20.RData")
data_decadal_distance %>%
  mutate(RCP="RCP85", mu=0.20) %>%
  rbind(data_temp) -> data_temp

# Pivot longer
data_temp %>%
  rename(future_responsive=future_climate_responsive, difference_baseline=diff_baseline, difference_responsive=diff_climate_responsive) %>%
  pivot_longer(cols=c(contemporary, future_baseline, future_responsive, difference_baseline, difference_responsive)) %>%
  mutate(name=factor(name,levels=c("contemporary","future_baseline","future_responsive", "difference_baseline", "difference_responsive")),
         mu = factor(mu)) %>%
  rename(distance=value) -> data
data %>% 
    ggplot(aes(x=RCP,y=distance, fill=mu)) + 
    geom_violin(colour = "grey25",linewidth = 0.4, position = position_dodge(width = 1)) +
    facet_wrap2(vars(name), scales="free_y", ncol=5) +
    facetted_pos_scales(y = list(name == "contemporary" ~ scale_y_log10(limits = c(0.1, 350)),
                                 name == "future_baseline" ~ scale_y_log10(limits = c(0.1, 350)),
                                 name == "future_responsive" ~ scale_y_log10(limits = c(0.1, 350)),
                                 name == "difference_baseline" ~ scale_y_continuous(limits = c(-140, 175)),
                                 name == "difference_responsive" ~ scale_y_continuous(limits = c(-140, 175)))) +
  labs(title="Dispersal distance", y="Dispersal distance (km)", fill="μ") +
  theme_sensitivity() ->
  sens_distance
#

ggsave("Figure S3.png",
       marrangeGrob(c(sens_spawning_temp, sens_dispersal_temp, sens_so),
                    nrow=3,ncol=1,
                    top=""),
       device="png", bg="white", width=15, height=15, units="cm", dpi=600)

ggsave("Figure S4.png",
       marrangeGrob(c(sens_PD, sens_distance, sens_settlement),
                    nrow=3,ncol=1,
                    top=""),
       device="png", bg="white", width=25, height=15, units="cm", dpi=600)

