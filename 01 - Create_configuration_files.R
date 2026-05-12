# 01 - Create configuration files for Ichthyop simulations

rm(list=ls())

library(XML)
library(lubridate)

fileName<-"ichthyop_configuration_files/Config_base.xml"

v_year <- c(2020:2029, 2090:2099)
i.year <- 1
k <- 1
i.year <- 1
for (i.year in c(1:20)) {
    year <- v_year[i.year]
    cat(year,"\n")
    zero_date <- ymd(paste0((year-1),"-12-31"))
     
    # Parse the xml file
    doc = xmlParse(fileName)

    # Loop on days and save a configuration file for each
    i <- 1
    for (i in 1 : 365) {
        release_date <- zero_date + i
        initial_time <- paste("year",year(release_date),
                              "month",sprintf("%02d",month(release_date)),
                              "day",sprintf("%02d",day(release_date)),
                              "at 21:00")

        out_prefix <- release_date
        xml_name <- paste0("ichthyop_configuration_files/Config_",k,".xml")

        # Find and modify variables in the parsed xml file
        ## Beginning of simulation
        a <- getNodeSet(doc, "/icstructure/block/parameters/parameter[key='initial_time']/value")
        xmlValue(a[[1]]) <- initial_time
        ## Prefix of the NetCDF output file
        a <- getNodeSet(doc, "/icstructure/block/parameters/parameter[key='file_prefix']/value")
        xmlValue(a[[1]]) <- out_prefix

        # Save the new xml configuration file
        saveXML(doc,file=xml_name)
        k <- k + 1
    }
}

