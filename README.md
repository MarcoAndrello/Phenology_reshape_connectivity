# Phenology_reshape_connectivity
Code and data for the study "Phenological shifts reshape connectivity under climate change"

01 - Create_configuration_files.R
Creates the .xml configuration files for the Ichthyop simulations. It starts from the template file "ichthyop_configuration_files/Config_base.xml" and creates one file for each day of each year (2020 to 2029 and 2090 to 2099)

02 - Change_output_filenames.R
It changes the name of the output files of Ichthyop, so as each file is named as YYYY-MM-DD.nc

03 - Read_temperature_spawning sites.R
It reads the sea surface temperature in spawning sites for each day and year. It then rearranges the dates starting from March 1st until February 28th

04 - Calculate_temperature_on_larval_dispersal.R
It calculates the mean temperature experienced by larvae over the first days of their larval dispersal. It produces one file for each year: each file is as a list object ("mean_temp") that containes the mean temperature for each released drifter (132241 drifters) in each day of the season (181 days, from March 1st to Aug 30th)

05 - Calculate_spawning_dates.R
It calculates an array giving whether spawning occurs for each site, day, year and decade

06 - Calibrate PLD-T relationship.r
It calculates the beta0 parameter of the relationship between temperature and PLD estimated by O'Connor

07 - Calculate_mean_PLD.R
It calculates the expected PLD of each drifter as a function of its mean temperature. It produces one file for each year: each file is as a list object ("data_PLD") that containes a dataframe for each day of the season (181 days, from March 1st to Aug 30th); in each dataframe, there are the sites of release, year, release day and calculated PLD for each released drifter (132241 drifters).

08 - Calculate connectivity_v08.R
It calculates dispersal distance and the connectivity matrix using the calculated spawning dates (from 05 - Calculate_spawning_dates.R) and mean PLD (from 07 - Calculate_mean_PLD.R), for each site and year, under the two spawning scenarios. Results are saved into the "connectivity" folder.

09 - Calculate_connectivity_variables_v02.R
It calculates various connectivity variables, among which total settlement success, for each site and year, and saves results in data_connectivity_Diplodus_sargus_mu01.RData. It calculates average connectivity matrices for each decade and spawning scenario.

