library(tidyverse)
library(odeqIRtools)
library(openxlsx)


# Load in rollup function -----------------------------------------------------------------------------------------

source('Rollups/Other_rollup_function.R')
source('Rollups/poll_asses_join.R')



# Load data and rollup ---------------------------------------------------------------------------------------------


## Temperature -----------------------------------------------------------------------------------------------------


### Year round ------------------------------------------------------------------------------------------------------


temp_yr_AU <- read.xlsx('Rollups/Rollup Assessment/temperature-assessments-corrected crit periods.xlsx',
                                sheet = 'YrRnd Other AU cat') %>%
  AU_rollup_other(periods = TRUE) %>%
  join_pollu_assess()


### Spawn -----------------------------------------------------------------------------------------------------------


temp_spawn_AU <- read.xlsx('Rollups/Rollup Assessment/temperature-assessments-corrected crit periods.xlsx',
                        sheet = 'Spawn Other AU cat') %>%
  AU_rollup_other(periods = TRUE) %>%
  join_pollu_assess()

## Bacteria --------------------------------------------------------------------------------------------------------


### Freshwater ----------------------------------------------------------------------------------------------------


bacteria_fresh_AU <- read.xlsx('Rollups/Rollup Assessment/bacteria freshwater contact.xlsx',
                                       sheet = "Other AU categorization"  ) %>%
  AU_rollup_other() %>%
  join_pollu_assess()



### Coastal Contact -------------------------------------------------------------------------------------------------

bacteria_coast_AU <- read.xlsx('Rollups/Rollup Assessment/bacteria coast contact.xlsx',
                                       sheet = "Other AU categorization")  %>%
  AU_rollup_other() %>%
  join_pollu_assess()


## chl -------------------------------------------------------------------------------------------------------------

chl_AU <- read.xlsx("Rollups/Rollup Assessment/chl-a.xlsx",
                            sheet = "Other AU categorization" ) %>%
  AU_rollup_other() %>%
  join_pollu_assess()



##  pH ------------------------------------------------------------------------------------------------------------

ph_AU <- read.xlsx('Rollups/Rollup Assessment/pH.xlsx',
                           sheet = "pH other AU cat" ) %>%
  AU_rollup_other() %>%
  join_pollu_assess()

## Tox_AL ----------------------------------------------------------------------------------------------------------


### others ------------------------------------------------------------------------------------------------------


tox_AL_other_AU <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                                     sheet = 'tox_AL_other_cats') %>%
  AU_rollup_other() %>%
  join_pollu_assess()


### hardness metals -------------------------------------------------------------------------------------------------

tox_AL_hard_AU <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                                    sheet = 'tox_AL_hard_other_cats') %>%
  AU_rollup_other() %>%
  join_pollu_assess()



### penta -----------------------------------------------------------------------------------------------------------

tox_AL_penta_AU <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                                     sheet = 'tox_AL_penta_other_cats') %>%
  AU_rollup_other() %>%
  join_pollu_assess()



### Ammonia --------------------------------------------------------------------------------------------------------


tox_AL_ammonia_AU <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                                       sheet = 'tox_AL_Ammonia_other_cats') %>%
  AU_rollup_other() %>%
  join_pollu_assess()



### Aluminum --------------------------------------------------------------------------------------------------------

tox_AL_aluminum_AU <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                                        sheet = 'tox_AL_Aluminum_other_cats') %>%
  AU_rollup_other() %>%
  join_pollu_assess()


## Tox HH ----------------------------------------------------------------------------------------------------------

tox_HH_AU <- read.xlsx('Rollups/Rollup Assessment/Tox_HH.xlsx',
                               sheet = "HH Tox Other AU Cat"  ) %>%
  mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code)) %>%
  AU_rollup_other() %>%
  join_pollu_assess()


## Turbidity ---------------------------------------------------------------------------------------------------------
turbidity_AU <- read.xlsx('Rollups/Rollup Assessment/turbidity.xlsx',
                                  sheet = "Turb other categorization") %>%
  AU_rollup_other() %>%
  join_pollu_assess()



## DO --------------------------------------------------------------------------------------------------------------

### year round ------------------------------------------------------------------------------------------------------


DO_yr_cont <- read.xlsx('Rollups/Rollup Assessment/DO Year Round.xlsx',
                        sheet = 'Yr Rnd Cont Other AU Cat') %>%
  select(AU_ID, Pollu_ID, wqstd_code,DO_Class, period,  IR_category, Rationale, recordID) 

DO_yr_inst <- read.xlsx('Rollups/Rollup Assessment/DO Year Round.xlsx',
                        sheet = 'Yr Rnd Instant Other AU Cat') %>%
  select(AU_ID, Pollu_ID, wqstd_code,DO_Class, period,  IR_category, Rationale, recordID) 

DO_yr <- bind_rows(DO_yr_cont, DO_yr_inst) %>%
  mutate(recordID = ifelse(!is.na(DO_Class), paste0(recordID, "-", DO_Class), recordID )) %>%
  AU_rollup_other(DO = TRUE) %>%
  join_pollu_assess()



### Spawn -------------------------------------------------------------------------------------------------------



DO_sp_cont <- read.xlsx('Rollups/Rollup Assessment/DO Spawn.xlsx',
                        sheet = 'Spawn Cont Other AU Cat') %>%
  mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-",
                           wqstd_code,"-", period, "-", DO_Class )) %>%
  select(AU_ID, Pollu_ID, wqstd_code,DO_Class, period,  IR_category, Rationale, recordID) 

DO_sp_inst <- read.xlsx('Rollups/Rollup Assessment/DO Spawn.xlsx',
                        sheet = 'Spawn Instant Other AU Cat') %>%
  mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-",
                           wqstd_code,"-", period, "-", DO_Class )) %>%
  select(AU_ID, Pollu_ID, wqstd_code,DO_Class, period,  IR_category, Rationale, recordID) 

DO_sp <- bind_rows(DO_sp_cont, DO_sp_inst) %>%
  mutate(period = ifelse(period == 'Spawn', "spawn", period )) %>%
  AU_rollup_other(DO = TRUE) %>%
  join_pollu_assess()

# Put it together -------------------------------------------------------------------------------------------------

rollup_AU_others <- bind_rows(temp_yr_AU,
                             temp_spawn_AU,
                             bacteria_fresh_AU,
                             bacteria_coast_AU,
                             chl_AU,
                             ph_AU,
                             tox_AL_other_AU,
                             tox_AL_hard_AU,
                             tox_AL_penta_AU,
                             tox_AL_ammonia_AU,
                             tox_AL_aluminum_AU,
                             tox_HH_AU,
                             turbidity_AU,
                             DO_yr,
                             DO_sp) %>%
  arrange(AU_ID, Assessment) %>%
  select(AU_ID, Pollu_ID,wqstd_code, Assessment,  Pollutant, period, DO_Class,assessed_2022, AU_previous_IR_category,
         `2022_IR_category`,AU_final_status, Rationale, recordID, AU_delist)

load('Rollups/all_other_stations.Rdata')

all_other_stations <- all_other_stations %>%
  mutate( Pollu_ID= as.character(Pollu_ID),
          wqstd_code = as.character(wqstd_code))

rollup_AU_others <- rollup_AU_others %>%
  left_join(all_other_stations)



load('Rollups/rollup helper/AU_prev_cat.Rdata')
AU_prev_cat <- AU_prev_cat %>%
  mutate(wqstd_code = as.character(wqstd_code),
         Pollu_ID = as.character(Pollu_ID)) %>%
  rename(previous_rationale = Rationale) %>%
  select(-IR_category) %>%
  distinct() %>%
  select(-Assessed_in_2018)

rollup_AU_others <- rollup_AU_others %>%
  left_join(AU_prev_cat) %>%
  mutate(Rationale = case_when(assessed_2022 == 'No' ~ previous_rationale,
                               TRUE ~ Rationale),
         year_assessed = case_when(assessed_2022 == 'No' ~ year_assessed,
                                   TRUE ~ as.integer(2022)),
         Year_listed = case_when(is.na(Year_listed) & AU_final_status %in% c('5', '4A') ~ as.integer(2022),
                                 TRUE ~ Year_listed)) %>%
  mutate(recordID = case_when(is.na(period) ~ paste0(year_assessed, "-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code),
                              !is.na(DO_Class) ~ paste0(year_assessed, "-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-",
                                                        wqstd_code,"-", period, "-", DO_Class ),
                              !is.na(period) & is.na(DO_Class) ~paste0(year_assessed, "-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-",
                                                                       wqstd_code,"-", period) ) )

save(rollup_AU_others, file = 'Rollups/rollup_AU_others.Rdata')
