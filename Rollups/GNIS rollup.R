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
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale)) %>%
  GNIS_rollup() %>%
  join_pollu_assess()


## DO --------------------------------------------------------------------------------------------------------------


### DO year round  ----------------------------------------------------------------------------------------------

DO_yr_cont <- read.xlsx('Rollups/Rollup Assessment/DO Year Round.xlsx',
                        sheet = 'Yr Rnd Cont WS Station Cat') %>%
  select(AU_ID, AU_GNIS_Name, Pollu_ID, wqstd_code,DO_Class, period, MLocID, IR_category, Rationale) %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale))

DO_yr_inst <- read.xlsx('Rollups/Rollup Assessment/DO Year Round.xlsx',
                        sheet = 'Yr Rnd Instant WS Station Cat') %>%
  select(AU_ID, AU_GNIS_Name, Pollu_ID, wqstd_code,DO_Class, period, MLocID, IR_category, Rationale)%>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale))

DO_yr <- bind_rows(DO_yr_cont, DO_yr_inst) %>%
  GNIS_rollup(periods = TRUE, DO = TRUE) %>%
  join_pollu_assess()


### DO Spawn --------------------------------------------------------------------------------------------------------

DO_sp_cont <- read.xlsx('Rollups/Rollup Assessment/DO Spawn.xlsx',
                        sheet = 'Spawn Cont WS Station Cat') %>%
  mutate(period = ifelse(period == 'Spawn', "spawn", period )) %>%
  select(AU_ID, AU_GNIS_Name, Pollu_ID, wqstd_code,DO_Class, period, MLocID, IR_category, Rationale) %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale))

DO_sp_inst <- read.xlsx('Rollups/Rollup Assessment/DO Spawn.xlsx',
                        sheet = 'Spawn Instant WS Station Cat') %>%
  mutate(period = ifelse(period == 'Spawn', "spawn", period )) %>%
  select(AU_ID, AU_GNIS_Name, Pollu_ID, wqstd_code,DO_Class, period, MLocID, IR_category, Rationale) %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale))

DO_spawn <- bind_rows(DO_sp_cont, DO_sp_inst) %>%
  GNIS_rollup(periods = TRUE, DO = TRUE) %>%
  join_pollu_assess()

##  pH ------------------------------------------------------------------------------------------------------------

ph_WS_station <- read.xlsx('Rollups/Rollup Assessment/pH.xlsx',
                           sheet = 'pH WS station cat') %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale)) %>%
  GNIS_rollup() %>%
  join_pollu_assess()


## Tox_AL ----------------------------------------------------------------------------------------------------------


### others ------------------------------------------------------------------------------------------------------


tox_AL_other_WS_station <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                                     sheet = 'tox_AL_WS_cats') %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale)) %>%
  GNIS_rollup() %>%
  join_pollu_assess()


### hardness metals -------------------------------------------------------------------------------------------------

tox_AL_hard_WS_station <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                                     sheet = 'tox_AL_hard_WS_cats') %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale)) %>%
  GNIS_rollup() %>%
  join_pollu_assess()



### penta -----------------------------------------------------------------------------------------------------------

tox_AL_penta_WS_station <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                                    sheet = 'tox_AL_penta_WS_cats') %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale)) %>%
  GNIS_rollup() %>%
  join_pollu_assess()



### Ammonia --------------------------------------------------------------------------------------------------------


tox_AL_ammonia_WS_station <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                                     sheet = 'tox_AL_Ammonia_WS_cats') %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale)) %>%
  GNIS_rollup() %>%
  join_pollu_assess()



### Aluminum --------------------------------------------------------------------------------------------------------

tox_AL_aluminum_WS_station <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                                       sheet = 'tox_AL_Aluminum_WS_cats') %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale)) %>%
  GNIS_rollup() %>%
  join_pollu_assess()



## Tox HH ----------------------------------------------------------------------------------------------------------

tox_HH_WS_station <- read.xlsx('Rollups/Rollup Assessment/Tox_HH.xlsx',
                                        sheet = 'HH Tox WS Station Cat') %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale)) %>%
  GNIS_rollup() %>%
  join_pollu_assess()


# Turbidity ---------------------------------------------------------------------------------------------------------
turbidity_WS_station <- read.xlsx('Rollups/Rollup Assessment/turbidity.xlsx',
                               sheet = 'Turb WS categorization') %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale)) %>%
  GNIS_rollup() %>%
  join_pollu_assess()





# All put together ------------------------------------------------------------------------------------------------

WS_GNIS_param_rollup <- bind_rows(temp_yr_WS_station,
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
                            ) %>%
  mutate(Rationale = str_replace_all(Rationale, 'Â', ''))

WS_GNIS_rollup <- WS_GNIS_param_rollup %>%
  mutate(GNIS_final_IR_category = ifelse(is.na(GNIS_final_IR_category), AU_previous_IR_category, as.character(GNIS_final_IR_category) )) %>%
  mutate(GNIS_final_IR_category = factor(GNIS_final_IR_category, levels=c("Unassessed", '3D',"3", "3B", "2", "5", '4A' ), ordered=TRUE),) %>%
  group_by(AU_ID, AU_GNIS_Name) %>%
  summarise(GNIS_status =  case_when(all(is.na(GNIS_final_IR_category)) & max(AU_final_status) %in% c("4B","4C","4A", "5","4" ) ~ "Impaired",
                                   all(is.na(GNIS_final_IR_category)) & max(AU_final_status) %in% c("3C","3D","3B", "3") ~ "Insufficient Data",
                                   all(is.na(GNIS_final_IR_category)) & max(AU_final_status) %in% c("2","3D","3B") ~ "Attaining",
                                   max(GNIS_final_IR_category, na.rm = TRUE) %in% c("4B","4C","4A", "5","4" ) ~ "Impaired",
                                max(GNIS_final_IR_category, na.rm = TRUE) %in% c("3C","3D","3B", "3") ~ "Insufficient Data",
                                max(GNIS_final_IR_category, na.rm = TRUE) %in% c("2","3D","3B") ~ "Attaining",
                                TRUE ~ 'ERROR'),
         Category_2_Pollutants = ifelse(length(str_c(unique(Pollutant[GNIS_final_IR_category ==  "2" ])[!is.na(unique(Pollutant[GNIS_final_IR_category ==  "2" ]))], 
                                                     collapse  = "; ")) > 0, 
                                        str_c(unique(Pollutant[GNIS_final_IR_category ==  "2"])[!is.na(unique(Pollutant[GNIS_final_IR_category ==  "2"]))],
                                              collapse  = "; "),
                                        "."),
         Category_5_Pollutants = ifelse(length(str_c(unique(Pollutant[GNIS_final_IR_category ==  "5" ])[!is.na(unique(Pollutant[GNIS_final_IR_category ==  "5" ]))], 
                                                     collapse  = "; ")) > 0, 
                                        str_c(unique(Pollutant[GNIS_final_IR_category ==  "5"])[!is.na(unique(Pollutant[GNIS_final_IR_category ==  "5"]))],
                                              collapse  = "; "),
                                        ".") ,
         Category_4_Pollutants= ifelse(length(str_c(unique(Pollutant[GNIS_final_IR_category ==  "4" ])[!is.na(unique(Pollutant[GNIS_final_IR_category ==  "4" ]))], 
                                                    collapse  = "; ")) > 0, 
                                       str_c(unique(Pollutant[GNIS_final_IR_category ==  "4"])[!is.na(unique(Pollutant[GNIS_final_IR_category ==  "4"]))],
                                             collapse  = "; "),
                                       "."),
         Category_4A_Pollutants= ifelse(length(str_c(unique(Pollutant[GNIS_final_IR_category ==  "4A" ])[!is.na(unique(Pollutant[GNIS_final_IR_category ==  "4A" ]))], 
                                                     collapse  = "; ")) > 0, 
                                        str_c(unique(Pollutant[GNIS_final_IR_category ==  "4A"])[!is.na(unique(Pollutant[GNIS_final_IR_category ==  "4A"]))],
                                              collapse  = "; "),
                                        "."),
         Category_4B_Pollutants= ifelse(length(str_c(unique(Pollutant[GNIS_final_IR_category ==  "4B" ])[!is.na(unique(Pollutant[GNIS_final_IR_category ==  "4B" ]))], 
                                                     collapse  = "; ")) > 0, 
                                        str_c(unique(Pollutant[GNIS_final_IR_category ==  "4B"])[!is.na(unique(Pollutant[GNIS_final_IR_category ==  "4B"]))],
                                              collapse  = "; "),
                                        "."),
         Category_4C_Pollutants= ifelse(length(str_c(unique(Pollutant[GNIS_final_IR_category ==  "4C" ])[!is.na(unique(Pollutant[GNIS_final_IR_category ==  "4C" ]))], 
                                                     collapse  = "; ")) > 0, 
                                        str_c(unique(Pollutant[GNIS_final_IR_category ==  "4C"])[!is.na(unique(Pollutant[GNIS_final_IR_category ==  "4C"]))],
                                              collapse  = "; "),
                                        "."),
         Category_3_Pollutants= ifelse(length(str_c(unique(Pollutant[GNIS_final_IR_category ==  "3" ])[!is.na(unique(Pollutant[GNIS_final_IR_category ==  "3" ]))], 
                                                    collapse  = "; ")) > 0, 
                                       str_c(unique(Pollutant[GNIS_final_IR_category ==  "3"])[!is.na(unique(Pollutant[GNIS_final_IR_category ==  "3"]))],
                                             collapse  = "; "),
                                       "."),
         
         Category_3B_Pollutants= ifelse(length(str_c(unique(Pollutant[GNIS_final_IR_category ==  "3B" ])[!is.na(unique(Pollutant[GNIS_final_IR_category ==  "3B" ]))], 
                                                     collapse  = "; ")) > 0, 
                                        str_c(unique(Pollutant[GNIS_final_IR_category ==  "3B"])[!is.na(unique(Pollutant[GNIS_final_IR_category ==  "3B"]))],
                                              collapse  = "; "),
                                        "."),
         Category_3C_Pollutants= ifelse(length(str_c(unique(Pollutant[GNIS_final_IR_category ==  "3C" ])[!is.na(unique(Pollutant[GNIS_final_IR_category ==  "3C" ]))], 
                                                     collapse  = "; ")) > 0, 
                                        str_c(unique(Pollutant[GNIS_final_IR_category ==  "3C"])[!is.na(unique(Pollutant[GNIS_final_IR_category ==  "3C"]))],
                                              collapse  = "; "),
                                        "."),
         Category_3D_Pollutants= ifelse(length(str_c(unique(Pollutant[GNIS_final_IR_category ==  "3D" ])[!is.na(unique(Pollutant[GNIS_final_IR_category ==  "3D" ]))], 
                                                     collapse  = "; ")) > 0, 
                                        str_c(unique(Pollutant[GNIS_final_IR_category ==  "3D"])[!is.na(unique(Pollutant[GNIS_final_IR_category ==  "3D"]))],
                                              collapse  = "; "),
                                        "."))


load('Rollups/rollup helper/AU_prev_cat.Rdata')
AU_prev_cat <- AU_prev_cat %>%
  mutate(wqstd_code = as.character(wqstd_code),
         Pollu_ID = as.character(Pollu_ID)) %>%
  rename(previous_rationale = Rationale) %>%
  select(-IR_category) %>%
  distinct() %>%
  select(-Assessed_in_2018)

WS_AU_rollup <- WS_GNIS_param_rollup %>%
  mutate(AU_final_status =factor(AU_final_status, levels=c("Unassessed", '3D',"3", "3B", "2", "5", '4A' ), ordered=TRUE),
         Rationale = str_replace_all(Rationale, 'Â', '')) %>%
  group_by(AU_ID, Pollutant, Assessment, Pollu_ID, wqstd_code, period, DO_Class) %>%
  summarise(stations =  paste(stations[!is.na(stations)], collapse = "; "),
            AU_final_status = max(AU_final_status),
            assessed_2022 = case_when(all(GNIS_IR_category == "Unassessed") ~ "No",
                                      any(GNIS_final_IR_category!= "Unassessed") ~ "Yes",
                                      TRUE ~ "Error"),
            AU_delist = first(AU_delist),
            Rationale = paste(unique(Rationale), collapse = " ~ ")
            
            ) %>%
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

wb <- createWorkbook()
addWorksheet(wb, sheetName = "WS_GNIS_param_rollup")
addWorksheet(wb, sheetName = "WS_AU_rollup")

header_st <- createStyle(textDecoration = "Bold", border = "Bottom")
freezePane(wb, "WS_GNIS_param_rollup", firstRow = TRUE) 
freezePane(wb, "WS_AU_rollup", firstRow = TRUE)

writeData(wb = wb, sheet = "WS_GNIS_param_rollup", x = WS_GNIS_param_rollup, headerStyle = header_st)
writeData(wb = wb, sheet = "WS_AU_rollup", x = WS_AU_rollup, headerStyle = header_st)

saveWorkbook(wb, 'Rollups/Rollup outputs/GNIS_rollup.xlsx', overwrite = TRUE) 



save(WS_AU_rollup, WS_GNIS_rollup,WS_GNIS_param_rollup, file = 'Rollups/WS_AU_GNIS_rollup.Rdata')