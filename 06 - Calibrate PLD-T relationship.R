# 06 - Calibrate PLD-T relationship.R
rm(list=ls())

library(tidyverse)

# Predicting the beta0i intercept of the OConnor et al 2007 relationship
# OConnor et al 2007 Eq. S11:
b0i.func <- function(x) log(x[2]) - (-1.3439341)*(log(x[1])-log(15)) - (-0.2775613)*(log(x[1])-log(15))^2
# The argument x is a vector with two elements
# x[1]: temperature
# x[2]: PLD
## Variances of the mixed model of O'Connor et al (from their Table S2)
## Needed to calculate the weight wi below
0.75299956 -> tau2
0.02296691 -> s2
# ## Testing the function with the example from O'Connor et al SI ll. 160
# b0i.func(c(10,3.83))
# # It works!


# Diplodus sargus
# Using values from DI Franco & Guidetti 2011:
# 14.4 and 17.6 days. we add 3 days (time from spawning to hatching): 17.4 and 20.6 
b0i.func(c(19.5,17.4)) -> b0.OLS.1
b0i.func(c(19.5,20.6)) -> b0.OLS.2
b0.OLS <- mean(c(b0.OLS.1,b0.OLS.2))
# gives 3.47
#Now we combine this OLS estimator of beta0i with the average intercept of the multilevel model
wi<-2*tau2/(2*tau2+s2) # 2 observations available
wi*b0.OLS+(1-wi)*3.1671863
# gives 3.310


