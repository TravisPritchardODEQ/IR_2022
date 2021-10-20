source("Parameters/Chl/fun_chl_data.R")
source("Parameters/Chl/fun_chl_analysis.R")


Results_censored_chla <- chla_data("IR_Dev")



chla_summary <- chl_assessment(Results_censored_chla)
