source("Parameters/Temperature/fun_temp_data.R")
source("Parameters/Temperature/fun_temp_analysis.R")



Results_censored_temp <- temp_data('IR_Dev')

temperature_analysis <- fun_temp_analysis(Results_censored_temp, write_excel = TRUE)

#temperature_analysis[['spawn_ws_au_categorization']]
