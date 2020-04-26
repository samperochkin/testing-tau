The files in the results folder were produced using the following sets of functions:

"sim-main/results/dt_main_app.csv" -- fromApplication
"sim-main/results/dt_main_high_0.csv" -- functionsHigh2
"sim-main/results/dt_main_high_1.csv" -- functionsHigh
"sim-main/results/dt_main_high_2.csv" -- functionsHigh
"sim-main/results/dt_main_high_3.csv" -- functionsHigh
"sim-main/results/dt_main_low_1.csv" -- functionsLow2
"sim-main/results/dt_main_low_2.csv" -- functionsLow2
"sim-main/results/dt_main_low_3.csv" -- functionsLowBoot
"sim-main/results/dt_main_low_4.csv" -- functionsLowBoot

"sim-main/results/dt_main_high_1.csv" 



# IMPORTANT NOTE :
The results for tau=0 from the files

"sim-main/results/dt_main_high_1.csv" 
"sim-main/results/dt_main_high_2.csv"
"sim-main/results/dt_main_high_3.csv"

should be omitted, as many of them have NA pvalues (missing the correction for sigma0 < sigma1 in performTests from the functionsHigh folder) 