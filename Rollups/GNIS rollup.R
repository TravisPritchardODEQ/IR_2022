library(tidyverse)
library(odeqIRtools)
library(openxlsx)


# Load in rollup function -----------------------------------------------------------------------------------------

source('Rollups/GNIS_Rollup_function.R')
source('Rollups/poll_asses_join.R')

# Bring in assessments --------------------------------------------------------------------------------------------


## Temperature -----------------------------------------------------------------------------------------------------


### Year round ------------------------------------------------------------------------------------------------------



temp_yr_WS_station <- read.xlsx('Rollups/Rollup Assessment/temperature-assessments-corrected crit periods.xlsx',
                                sheet = 'YrRnd WS station cat') %>%
  GNIS_rollup(periods = TRUE) %>%
  join_pollu_assess()




# ## Spawning -----------------------------------------------------------------------------------------------------



temp_spawn_WS_station <- read.xlsx('Rollups/Rollup Assessment/temperature-assessments-corrected crit periods.xlsx',
                                sheet = 'Spawn WS station cat') %>%
  GNIS_rollup(periods = TRUE) %>%
  join_pollu_assess()


#write.xlsx(temp_WS_station_GNIS_rollup, file = 'Rollups/Rollup outputs/temperature_GNIS_rollup.xlsx')


## Bacteria --------------------------------------------------------------------------------------------------------


### Freshwater ----------------------------------------------------------------------------------------------------


bacteria_fresh_WS_station <- read.xlsx('Rollups/Rollup Assessment/bacteria freshwater contact.xlsx',
                                   sheet = 'WS station categorization') 


bacteria_fresh_WS_station_GNIS_rollup <- GNIS_rollup(bacteria_fresh_WS_station)
bacteria_fresh_WS_station_GNIS_rollup <- join_pollu_assess(bacteria_fresh_WS_station_GNIS_rollup)

#write.xlsx(bacteria_fresh_WS_station_GNIS_rollup, file = 'Rollups/Rollup outputs/bacteria_fresh_GNIS_rollup.xlsx')


### Coastal Contact -------------------------------------------------------------------------------------------------

bacteria_coast_WS_station <- read.xlsx('Rollups/Rollup Assessment/bacteria coast contact.xlsx',
                                       sheet = 'WS station categorization')  %>%
  GNIS_rollup() %>%
  join_pollu_assess()



#write.xlsx(bacteria_coast_WS_station_GNIS_rollup, file = 'Rollups/Rollup outputs/bacteria_coast_GNIS_rollup.xlsx')


## Chl -----------------------------------------------------------------------------------------------------------

chl_WS_station <- read.xlsx("Rollups/Rollup Assessment/chl-a.xlsx",
                            sheet = 'WS station categorization') %>%
  GNIS_rollup() %>%
  join_pollu_assess()


## DO --------------------------------------------------------------------------------------------------------------


### DO year round  ----------------------------------------------------------------------------------------------

DO_yr_cont <- read.xlsx('Rollups/Rollup Assessment/DO Year Round.xlsx',
                        sheet = 'Yr Rnd Cont WS Station Cat') %>%
  select(AU_ID, AU_GNIS_Name, Pollu_ID, wqstd_code,DO_Class, period, MLocID, IR_category)

DO_yr_inst <- read.xlsx('Rollups/Rollup Assessment/DO Year Round.xlsx',
                        sheet = 'Yr Rnd Instant WS Station Cat') %>%
  select(AU_ID, AU_GNIS_Name, Pollu_ID, wqstd_code,DO_Class, period, MLocID, IR_category)

DO_yr <- bind_rows(DO_yr_cont, DO_yr_inst) %>%
  GNIS_rollup(periods = TRUE, DO = TRUE) %>%
  join_pollu_assess()


### DO Spawn --------------------------------------------------------------------------------------------------------

DO_sp_cont <- read.xlsx('Rollups/Rollup Assessment/DO Spawn.xlsx',
                        sheet = 'Spawn Cont WS Station Cat') %>%
  mutate(period = ifelse(period == 'Spawn', "spawn", period )) %>%
  select(AU_ID, AU_GNIS_Name, Pollu_ID, wqstd_code,DO_Class, period, MLocID, IR_category)

DO_sp_inst <- read.xlsx('Rollups/Rollup Assessment/DO Spawn.xlsx',
                        sheet = 'Spawn Instant WS Station Cat') %>%
  mutate(period = ifelse(period == 'Spawn', "spawn", period )) %>%
  select(AU_ID, AU_GNIS_Name, Pollu_ID, wqstd_code,DO_Class, period, MLocID, IR_category)

DO_spawn <- bind_rows(DO_sp_cont, DO_sp_inst) %>%
  GNIS_rollup(periods = TRUE, DO = TRUE) %>%
  join_pollu_assess()

##  pH ------------------------------------------------------------------------------------------------------------

ph_WS_station <- read.xlsx('Rollups/Rollup Assessment/pH.xlsx',
                           sheet = 'pH WS station cat') %>%
  GNIS_rollup() %>%
  join_pollu_assess()


## Tox_AL ----------------------------------------------------------------------------------------------------------


### others ------------------------------------------------------------------------------------------------------


tox_AL_other_WS_station <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                                     sheet = 'tox_AL_WS_cats') %>%
  GNIS_rollup() %>%
  join_pollu_assess()


### hardness metals -------------------------------------------------------------------------------------------------

tox_AL_hard_WS_station <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                                     sheet = 'tox_AL_hard_WS_cats') %>%
  GNIS_rollup() %>%
  join_pollu_assess()



### penta -----------------------------------------------------------------------------------------------------------

tox_AL_penta_WS_station <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                                    sheet = 'tox_AL_penta_WS_cats') %>%
  GNIS_rollup() %>%
  join_pollu_assess()



### Ammonia --------------------------------------------------------------------------------------------------------


tox_AL_ammonia_WS_station <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                                     sheet = 'tox_AL_Ammonia_WS_cats') %>%
  GNIS_rollup() %>%
  join_pollu_assess()



### Aluminum --------------------------------------------------------------------------------------------------------

tox_AL_aluminum_WS_station <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                                       sheet = 'tox_AL_Aluminum_WS_cats') %>%
  GNIS_rollup() %>%
  join_pollu_assess()



## Tox HH ----------------------------------------------------------------------------------------------------------

tox_HH_WS_station <- read.xlsx('Rollups/Rollup Assessment/Tox_HH.xlsx',
                                        sheet = 'HH Tox WS Station Cat') %>%
  GNIS_rollup() %>%
  join_pollu_assess()


# Turbidity ---------------------------------------------------------------------------------------------------------
turbidity_WS_station <- read.xlsx('Rollups/Rollup Assessment/turbidity.xlsx',
                               sheet = 'Turb WS categorization') %>%
  GNIS_rollup() %>%
  join_pollu_assess()





# All put together ------------------------------------------------------------------------------------------------

WS_GNIS_rollup <- bind_rows(temp_yr_WS_station,
                            temp_spawn_WS_station,
                            bacteria_fresh_WS_station_GNIS_rollup,
                            bacteria_coast_WS_station,
                            chl_WS_station,
                            DO_yr,
                            DO_spawn,
                            ph_WS_station,
                            tox_AL_other_WS_station,
                            tox_AL_hard_WS_station,
                            tox_AL_penta_WS_station,
                            tox_AL_ammonia_WS_station,
                            tox_AL_aluminum_WS_station,
                            tox_HH_WS_station,
                            turbidity_WS_station
                            )

write.xlsx(WS_GNIS_rollup, file = 'Rollups/Rollup outputs/GNIS_rollup.xlsx')
