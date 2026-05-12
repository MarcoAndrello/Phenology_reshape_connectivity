rm(list=ls())

library(ggplot2)
library(dplyr)
library(grid)

days <- 1:365
threshold <- 17
spawn_duration <- 90

# funzione temperatura stagionale
temp_fun <- function(day, mean, amp=7, phase=100){
    mean + amp*sin(2*pi*(day-phase)/365)
}

# scenari
scenarios <- data.frame(
    scenario=c("Present",
               "Future (fixed phenology)",
               "Future (shifting phenology)"),
    mean_temp=c(17,21,21)
)

# dataset temperature
df <- scenarios |>
    rowwise() |>
    do(data.frame(
        day=days,
        coastal=temp_fun(days,.$mean_temp),
        scenario=.$scenario
    ))

df$scenario <- factor(df$scenario,
                      levels=scenarios$scenario)

# funzione per trovare spawning start
find_spawn_start <- function(temp){
    idx <- which(stats::filter(temp>threshold, rep(1,6), sides=1)==6)[1]
    if(is.na(idx)) return(120)
    idx
}

# calcolo spawn start per scenario
spawn_starts <- df |>
    group_by(scenario) |>
    summarise(start=find_spawn_start(coastal))

# fixed phenology usa quello del presente
spawn_starts$start[spawn_starts$scenario=="Future (fixed phenology)"] <-
    spawn_starts$start[spawn_starts$scenario=="Present"]

# finestra spawning
spawn <- spawn_starts |>
    mutate(
        xmin=start,
        xmax=start+spawn_duration
    )

# posizione etichetta spawning season (centro banda)
spawn_labels <- spawn |>
    mutate(
        x=(xmin+xmax)/2,
        y=11,
        label="Spawning\nseason"
    )

# PLD concettuale (lunghezze molto diverse)
pld <- spawn |>
    rowwise() |>
    mutate(
        xstart = start + 20,
        y = temp_fun(xstart,
                     scenarios$mean_temp[scenarios$scenario==scenario]),
        xend = ifelse(scenario=="Future (fixed phenology)",
                      xstart + 25,
                      xstart + 60)
    )

p <- ggplot() +
    
    geom_rect(data=spawn,
              aes(xmin=xmin,xmax=xmax,ymin=-Inf,ymax=Inf),
              fill="grey85",
              alpha=0.5) +
    
    geom_line(data=df,
              aes(day,coastal),
              colour="#E66101",
              linewidth=1.6) +
    
    geom_hline(yintercept=threshold,
               linetype="dashed",
               linewidth=0.8,
               colour="grey40") +
    
    annotate("text",
             x = 290,
             y = threshold - 2,
             label = "Spawning threshold\n(17°C)",
             size = 4,
             hjust = 1,
             colour = "grey40") +
    
    geom_segment(data=pld,
                 aes(x=xstart,xend=xend,y=y,yend=y),
                 arrow=arrow(length=unit(0.28,"cm")),
                 linewidth=1.3,
                 colour="#2C7FB8") +
    
    geom_text(data = pld,
              aes(x = xend+25,
                  y = y,
                  label = "PLD"),
              vjust = 0.5,
              size = 4,
              colour="#2C7FB8") +
    
    geom_text(data=spawn_labels,
              aes(x=x,y=y,label=label),
              size=4,
              colour="grey40") +
    
    facet_wrap(~scenario,ncol=1, scales="free_x") +
    
    coord_cartesian(xlim=c(30,365),ylim=c(8,30)) +
    
    labs(x="Day of year",
         y="Coastal temperature (°C)") +
    
    theme_classic(base_size=15) +
    theme(
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 13),
        axis.title = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.spacing = unit(1.3,"lines")
    )
p

ggsave("Figure 1.png",
       p,
       width=6,
       height=9,
       dpi=600)
