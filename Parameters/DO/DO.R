library(tidyverse)
library(lubridate)
library(odeqIRtools)


source('Parameters/DO/fun_DO_data.R')
source("Parameters/DO/fun_DO_year_round_analysis.R")
source("Parameters/DO/fun_DO_spawn_analysis.R")



Results_censored_DO <- DO_data('IR_Dev')


DO_Year_Round <- fun_DO_year_round(df = Results_censored_DO, write_excel = TRUE)

DO_Spawn <- fun_DO_spawn(df = Results_censored_DO, write_excel = TRUE)
