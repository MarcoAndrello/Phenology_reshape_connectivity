01 - Create_configuration_files.R
Creates the .xml configuration files for the Ichthyop simulations. It starts from the template file "ichthyop_configuration_files/Config_base.xml" and creates one file for each day of each year (2020 to 2029 and 2090 to 2099)

02 - Change_output_filenames.R
It changes the name of the output files of Ichthyop, so as each file is named as YYYY-MM-DD.nc

03 - Read_temperature_spawning sites.R
It reads the sea surface temperature in spawning sites for each day and year. It then arranges the data into an array.

04 - Calculate_temperature_on_dispersal.R
It calculates the mean temperature experienced by individuals over transport. It produces one file for each year: each file is as a list object ("mean_temp") that containes the mean temperature for each released drifter (132241 drifters) in each day of the season (181 days, from Jan 1st to June 30th).

05 - Calculate_spawning_dates.R
It calculates an array giving whether spawning occurs for each site, day, year and decade. Spawning onset is identified using a harmonic regression to identify the winter minimum and using the criterion that average water temperature should be > 17 °C for at least six days. It plots Figure S1 of the paper.

06 - Calibrate PD-T relationship.r
It calculates the beta0 parameter of the relationship between temperature and pelagic duration PD estimated by O'Connor et al (2007), using species-specific values on pelagic duration in Diplodus sargus.

07 - Calculate_mean_PD.R
It calculates the expected pelagic duration (PD) of each drifter as a function of its mean temperature. It produces one file for each year: each file is as a list object ("data_PLD") that containes a dataframe for each day of the season (181 days, from March 1st to Aug 30th); in each dataframe, there are the sites of release, year, release day and calculated PD for each released drifter (132241 drifters).

08 - Calculate connectivity_v08.R
It calculates dispersal distance and the connectivity matrix using the calculated spawning dates (from 05 - Calculate_spawning_dates.R) and mean PD (from 07 - Calculate_mean_PD.R), for each site and year, under the two phenology scenarios. Results are saved into the "output/connectivity" folder.

09 - Calculate_connectivity_variables_v02.R
It calculates various connectivity variables, among which total settlement success, for each site and year, and saves results in output/data_connectivity_Diplodus_sargus_muXX.RData. It calculates average connectivity matrices for each decade and spawning scenario.

10 - Data_analysis.R
It performs the statistical analysis for the six variables analysed in the study: Temperature in spawning sites, Temperature during transport, Spawning onset, Pelagic duration (PD), Dispersal distance, Total settlement success
For each variable, it prepares the datasets to plot the figures (e.g. "Data_figure_spawning_temperature_RCP45.RData")

11 - Figure 1.R
It plots Figure 1 of the paper.

12 - Figure results.R
It calculates the statistics (median, 95 percentile interval, range) of the decadal variables and differences; it plots the figures of the results of the paper (Figure 2, 3, 4, 5, and S2).

13 - Figure sensitivity.R
It plots the figures of the sensitivity analyses to the RCP scenario and mortality rate (Figure S3, S4).

Functions - Map.R
Some helper functions to check for NA and draw the maps