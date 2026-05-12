# 02 - Change ichthyop output filenames
# It changes the name of the output files of Ichthyop, so as each file is named as YYYY-MM-DD.nc

rm(list=ls())

library(tidyverse)

setwd("PATH TO ICHTHYOP OUTPUT FOLDER")
file_names <- list.files()

i <- 1
for (i in 1 : length(file_names)) {
    new_name <- paste0(substr(file_names[i],1,10),".nc")
    file.rename(file_names[i],
                new_name)
}
