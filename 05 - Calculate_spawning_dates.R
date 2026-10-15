# 05 - Calculate spawning dates

rm(list=ls())

library(tidyverse)
library(sf)
library(forecast)

scen <- 45

################################################################################
# Read files
################################################################################

# Read polygon layer
pol <- st_read("sites/sites.shp")


################################################################################
# CALCULATE SPAWNING DATES
################################################################################

# Read temperature
load(paste0("output/temperature_dataset_Diplodus_sargus_RCP",scen,".RData"))


# Finding winter minimum: harmonic regression
wm <- array(NA,dim=c(1325,10,2))
i.decade <- 1
for(i.decade in 1 : 2) {
  i.year <- 1
  for (i.year in 1 : 10) {
    cat(i.decade,i.year,"\n"); flush.console()
    # i.site <- 480 # Bonifacio
    for (i.site in 1 : 1325) {
      y <- ts(temperature[i.site,,i.year,i.decade], frequency = 365)
      fit_fourier <- tslm(y ~ trend + fourier(y, K = 1))
      wm[i.site,i.year,i.decade] <- which.min(as.numeric(fitted(fit_fourier))[1:181])
    }
  }
}
par(mfrow=c(5,2))
for(i.decade in 1 : 2) {
  for (i.year in 1 : 10) {
    hist(wm[,i.year,i.decade],main=paste("Decade",i.decade,"Year",i.year),breaks=20)
  }
}


# Spawning onset criterion: > 17 degrees for six days after winter minimum
spawn<- array(F,dim=c(1325,365,10,2))
dimnames(spawn) <- dimnames(temperature)
first_day<- last_day <- number_release_days <- array(NA,c(nrow(pol),10,2))
i.decade <- 1
for (i.decade in 1 : 2) {
  i.year <- 1
  for (i.year in 1 : 10) {
    cat(i.decade, i.year,"\n"); flush.console()
    i.pol <- 1
    for (i.pol in 1 : 1325) {
      t_med <- vector()
      for (i in 1 : 181) t_med[i] <- sum(temperature[i.pol,i:(i+5),i.year,i.decade])
      # mask: only values after winter minimum
      t_med_masked <- c(rep(NA,wm[i.pol,i.year,i.decade]),t_med[(wm[i.pol,i.year,i.decade]+1) : length(t_med)])
      first_day[i.pol,i.year,i.decade] <- which(t_med_masked > (6*17))[1]
      last_day[i.pol,i.year,i.decade] <- first_day[i.pol,i.year,i.decade] + 89
      if(!is.na(first_day[i.pol,i.year,i.decade])) {
        spawn[i.pol,
              first_day[i.pol,i.year,i.decade]:min(last_day[i.pol,i.year,i.decade],181), # max last day is 181
              i.year,
              i.decade] <- T
      }
      number_release_days[i.pol,i.year,i.decade] <- sum(spawn[i.pol,,i.year,i.decade])
    }
  }
}

par(mfrow=c(1,1))
plot(as.vector(first_day[,,1]), as.vector(first_day[,,2]),xlab="Decade 2020",ylab="Decade 2090")
abline(a=0,b=1)

plot(as.vector(number_release_days[,,1]), as.vector(number_release_days[,,2]),xlab="Decade 2020",ylab="Decade 2090")
plot(as.vector(first_day[,,1]), as.vector(number_release_days[,,1]),xlab="First day",ylab="Number release days")
plot(as.vector(first_day[,,2]), as.vector(number_release_days[,,2]),xlab="First day",ylab="Number release days")

save(spawn, first_day, number_release_days, file=paste0("output/spawning_dataset_Diplodus_sargus_RCP",scen,".RData"))


################################################################################
# Plot harmonic regression (Figure S1)
################################################################################
i.site <- 480
# Time series of daily temperature during one year
y <- ts(temperature[i.site,,1,1], frequency = 365)
# Fit a time-series model with a fourier series of order 1
fit_fourier <- tslm(y ~ trend + fourier(y, K = 1))
# Plot temperature data
doy <- seq_along(y)
plot(doy, as.numeric(y), type = "l", col = "grey50", xlab = "Day of Year", ylab = "Temperature (°C)", xlim = c(1, 365))
# Add the fitted time series
lines(doy,fitted(fit_fourier),col="red", lwd=2)
# Plot horizontal line indicating 17 °C
abline(h=17,col="blue",lty=2)
# Plot vertical line corresponding to first day of spawning
abline(v=first_day[i.site,1,1],col="darkgreen",lwd=2)
temperature[i.site,132:137,1,1]
