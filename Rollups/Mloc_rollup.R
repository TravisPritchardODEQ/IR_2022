library(tidyverse)
library(odeqIRtools)
library(openxlsx)


# Load in rollup function -----------------------------------------------------------------------------------------

source('Rollups/MLOC_Rollup_function.R')
source('Rollups/poll_asses_join.R')

# Bring in assessments --------------------------------------------------------------------------------------------


## Temperature -----------------------------------------------------------------------------------------------------


### Year round ------------------------------------------------------------------------------------------------------



temp_yr_WS_station <- read.xlsx('Rollups/Rollup Assessment/temperature-assessments-corrected crit periods.xlsx',
                                sheet = 'YrRnd WS station cat') %>%
  Mloc_Rollup_function(periods = TRUE) %>%
  join_pollu_assess()




# ## Spawning -----------------------------------------------------------------------------------------------------



temp_spawn_WS_station <- read.xlsx('Rollups/Rollup Assessment/temperature-assessments-corrected crit periods.xlsx',
                                   sheet = 'Spawn WS station cat') %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code)) %>%
  Mloc_Rollup_function(periods = TRUE) %>%
  join_pollu_assess()


#write.xlsx(temp_WS_station_Mloc_Rollup_function, file = 'Rollups/Rollup outputs/temperature_Mloc_Rollup_function.xlsx')


## Bacteria --------------------------------------------------------------------------------------------------------


### Freshwater ----------------------------------------------------------------------------------------------------


bacteria_fresh_WS_station <- read.xlsx('Rollups/Rollup Assessment/bacteria freshwater contact.xlsx',
                                       sheet = 'WS station categorization') %>%
  Mloc_Rollup_function() %>%
  join_pollu_assess()


#write.xlsx(bacteria_fresh_WS_station_Mloc_Rollup_function, file = 'Rollups/Rollup outputs/bacteria_fresh_Mloc_Rollup_function.xlsx')


### Coastal Contact -------------------------------------------------------------------------------------------------

bacteria_coast_WS_station <- read.xlsx('Rollups/Rollup Assessment/bacteria coast contact.xlsx',
                                       sheet = 'WS station categorization')  %>%
  Mloc_Rollup_function() %>%
  join_pollu_assess()



#write.xlsx(bacteria_coast_WS_station_Mloc_Rollup_function, file = 'Rollups/Rollup outputs/bacteria_coast_Mloc_Rollup_function.xlsx')


## Chl -----------------------------------------------------------------------------------------------------------

chl_WS_station <- read.xlsx("Rollups/Rollup Assessment/chl-a.xlsx",
                            sheet = 'WS station categorization') %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale)) %>%
  Mloc_Rollup_function() %>%
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
  Mloc_Rollup_function(periods = TRUE, DO = TRUE) %>%
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
  Mloc_Rollup_function(periods = TRUE, DO = TRUE) %>%
  join_pollu_assess()

##  pH ------------------------------------------------------------------------------------------------------------

ph_WS_station <- read.xlsx('Rollups/Rollup Assessment/pH.xlsx',
                           sheet = 'pH WS station cat') %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale)) %>%
  Mloc_Rollup_function() %>%
  join_pollu_assess()


## Tox_AL ----------------------------------------------------------------------------------------------------------


### others ------------------------------------------------------------------------------------------------------


tox_AL_other_WS_station <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                                     sheet = 'tox_AL_WS_cats') %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale)) %>%
  Mloc_Rollup_function() %>%
  join_pollu_assess()


### hardness metals -------------------------------------------------------------------------------------------------

tox_AL_hard_WS_station <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                                    sheet = 'tox_AL_hard_WS_cats') %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale)) %>%
  Mloc_Rollup_function() %>%
  join_pollu_assess()



### penta -----------------------------------------------------------------------------------------------------------

tox_AL_penta_WS_station <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                                     sheet = 'tox_AL_penta_WS_cats') %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale)) %>%
  Mloc_Rollup_function() %>%
  join_pollu_assess()



### Ammonia --------------------------------------------------------------------------------------------------------


tox_AL_ammonia_WS_station <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                                       sheet = 'tox_AL_Ammonia_WS_cats') %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale)) %>%
  Mloc_Rollup_function() %>%
  join_pollu_assess()



### Aluminum --------------------------------------------------------------------------------------------------------

tox_AL_aluminum_WS_station <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                                        sheet = 'tox_AL_Aluminum_WS_cats') %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale)) %>%
  Mloc_Rollup_function() %>%
  join_pollu_assess()



## Tox HH ----------------------------------------------------------------------------------------------------------

tox_HH_WS_station <- read.xlsx('Rollups/Rollup Assessment/Tox_HH.xlsx',
                               sheet = 'HH Tox WS Station Cat') %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale)) %>%
  Mloc_Rollup_function() %>%
  join_pollu_assess()


# Turbidity ---------------------------------------------------------------------------------------------------------
turbidity_WS_station <- read.xlsx('Rollups/Rollup Assessment/turbidity.xlsx',
                                  sheet = 'Turb WS categorization') %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale)) %>%
  Mloc_Rollup_function() %>%
  join_pollu_assess()




# Biocriteria -----------------------------------------------------------------------------------------------------

MWCF_SS <- read.xlsx('Rollups/Rollup Assessment/IR_2022_Biocriteria_for_BPJ.xlsx',
                     sheet = 'MWCF_SS') %>%
  rename(IR_category = IR_Cat,
         GNIS_previous_IR_impairement = `Previous_Impaired_AU:GNIS`) %>%
  mutate(IR_category = str_remove(IR_category, 'Cat')) %>%
  mutate(Pollu_ID = "156",
         wqstd_code = "5",
         Delist = as.character(Delist)) %>%
  select(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code,IR_category, Rationale, GNIS_previous_IR_impairement, Delist)

MWCF_MS <- read.xlsx('Rollups/Rollup Assessment/IR_2022_Biocriteria_for_BPJ.xlsx',
                     sheet = 'MWCF_MS')%>%
  rename(IR_category = IR_Cat_ave,
         GNIS_previous_IR_impairement = `Previous_Impaired_AU:GNIS`) %>%
  mutate(IR_category = str_remove(IR_category, 'Cat')) %>%
  mutate(Pollu_ID = "156",
         wqstd_code = "5",
         Delist = as.character(Delist)) %>%
  select(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code,IR_category, Rationale, GNIS_previous_IR_impairement, Delist)


WCCP_SS <- read.xlsx('Rollups/Rollup Assessment/IR_2022_Biocriteria_for_BPJ.xlsx',
                     sheet = 'WCCP_SS') %>%
  rename(IR_category = IR_Cat) %>%
  mutate(IR_category = str_remove(IR_category, 'Cat')) %>%
  mutate(Pollu_ID = "156",
         wqstd_code = "5",
         Delist = as.character(Delist)) %>%
  select(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code,IR_category, Rationale, Delist)

WCCP_MS <- read.xlsx('Rollups/Rollup Assessment/IR_2022_Biocriteria_for_BPJ.xlsx',
                     sheet = 'WCCP_MS') %>%
  rename(IR_category = IR_Cat_ave,
         GNIS_previous_IR_impairement = `Previous_Impaired_AU:GNIS`) %>%
  mutate(IR_category = str_remove(IR_category, 'Cat')) %>%
  mutate(Pollu_ID = "156",
         wqstd_code = "5",
         Delist = as.character(Delist)) %>%
  select(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code,IR_category, Rationale, GNIS_previous_IR_impairement, Delist)

biocriteria_together <- bind_rows(MWCF_MS, MWCF_SS, WCCP_MS, WCCP_SS)

WS_biocriteria <- biocriteria_together %>%
  filter(str_detect(AU_ID, "WS", negate = FALSE)) 

biocriteria_WS_station <- WS_biocriteria %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale)) %>%
  Mloc_Rollup_function() %>%
  join_pollu_assess()

# All put together ------------------------------------------------------------------------------------------------

WS_MLocID_param_rollup <- bind_rows(temp_yr_WS_station,
                                  temp_spawn_WS_station,
                                  bacteria_fresh_WS_station,
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
                                  turbidity_WS_station,
                                  biocriteria_WS_station
) %>%
  mutate(Rationale = str_replace_all(Rationale, 'Â', ''))

WS_MlocID_rollup <- WS_MLocID_param_rollup %>%
  group_by(AU_ID, MLocID) %>%
  summarise(MLocID_status =  case_when(max(MLocID_IR_category, na.rm = TRUE) %in% c("4B","4C","4A", "5","4" ) ~ "Impaired",
                                     max(MLocID_IR_category, na.rm = TRUE) %in% c("3C","3D","3B", "3") ~ "Insufficient Data",
                                     max(MLocID_IR_category, na.rm = TRUE) %in% c("2","3D","3B") ~ "Attaining",
                                     TRUE ~ 'ERROR'),
            Category_2_Pollutants = ifelse(length(str_c(unique(Pollutant[MLocID_IR_category ==  "2" ])[!is.na(unique(Pollutant[MLocID_IR_category ==  "2" ]))], 
                                                        collapse  = "; ")) > 0, 
                                           str_c(unique(Pollutant[MLocID_IR_category ==  "2"])[!is.na(unique(Pollutant[MLocID_IR_category ==  "2"]))],
                                                 collapse  = "; "),
                                           "."),
            Category_5_Pollutants = ifelse(length(str_c(unique(Pollutant[MLocID_IR_category ==  "5" ])[!is.na(unique(Pollutant[MLocID_IR_category ==  "5" ]))], 
                                                        collapse  = "; ")) > 0, 
                                           str_c(unique(Pollutant[MLocID_IR_category ==  "5"])[!is.na(unique(Pollutant[MLocID_IR_category ==  "5"]))],
                                                 collapse  = "; "),
                                           ".") ,
            Category_4_Pollutants= ifelse(length(str_c(unique(Pollutant[MLocID_IR_category ==  "4" ])[!is.na(unique(Pollutant[MLocID_IR_category ==  "4" ]))], 
                                                       collapse  = "; ")) > 0, 
                                          str_c(unique(Pollutant[MLocID_IR_category ==  "4"])[!is.na(unique(Pollutant[MLocID_IR_category ==  "4"]))],
                                                collapse  = "; "),
                                          "."),
            Category_4A_Pollutants= ifelse(length(str_c(unique(Pollutant[MLocID_IR_category ==  "4A" ])[!is.na(unique(Pollutant[MLocID_IR_category ==  "4A" ]))], 
                                                        collapse  = "; ")) > 0, 
                                           str_c(unique(Pollutant[MLocID_IR_category ==  "4A"])[!is.na(unique(Pollutant[MLocID_IR_category ==  "4A"]))],
                                                 collapse  = "; "),
                                           "."),
            Category_4B_Pollutants= ifelse(length(str_c(unique(Pollutant[MLocID_IR_category ==  "4B" ])[!is.na(unique(Pollutant[MLocID_IR_category ==  "4B" ]))], 
                                                        collapse  = "; ")) > 0, 
                                           str_c(unique(Pollutant[MLocID_IR_category ==  "4B"])[!is.na(unique(Pollutant[MLocID_IR_category ==  "4B"]))],
                                                 collapse  = "; "),
                                           "."),
            Category_4C_Pollutants= ifelse(length(str_c(unique(Pollutant[MLocID_IR_category ==  "4C" ])[!is.na(unique(Pollutant[MLocID_IR_category ==  "4C" ]))], 
                                                        collapse  = "; ")) > 0, 
                                           str_c(unique(Pollutant[MLocID_IR_category ==  "4C"])[!is.na(unique(Pollutant[MLocID_IR_category ==  "4C"]))],
                                                 collapse  = "; "),
                                           "."),
            Category_3_Pollutants= ifelse(length(str_c(unique(Pollutant[MLocID_IR_category ==  "3" ])[!is.na(unique(Pollutant[MLocID_IR_category ==  "3" ]))], 
                                                       collapse  = "; ")) > 0, 
                                          str_c(unique(Pollutant[MLocID_IR_category ==  "3"])[!is.na(unique(Pollutant[MLocID_IR_category ==  "3"]))],
                                                collapse  = "; "),
                                          "."),
            
            Category_3B_Pollutants= ifelse(length(str_c(unique(Pollutant[MLocID_IR_category ==  "3B" ])[!is.na(unique(Pollutant[MLocID_IR_category ==  "3B" ]))], 
                                                        collapse  = "; ")) > 0, 
                                           str_c(unique(Pollutant[MLocID_IR_category ==  "3B"])[!is.na(unique(Pollutant[MLocID_IR_category ==  "3B"]))],
                                                 collapse  = "; "),
                                           "."),
            Category_3C_Pollutants= ifelse(length(str_c(unique(Pollutant[MLocID_IR_category ==  "3C" ])[!is.na(unique(Pollutant[MLocID_IR_category ==  "3C" ]))], 
                                                        collapse  = "; ")) > 0, 
                                           str_c(unique(Pollutant[MLocID_IR_category ==  "3C"])[!is.na(unique(Pollutant[MLocID_IR_category ==  "3C"]))],
                                                 collapse  = "; "),
                                           "."),
            Category_3D_Pollutants= ifelse(length(str_c(unique(Pollutant[MLocID_IR_category ==  "3D" ])[!is.na(unique(Pollutant[MLocID_IR_category ==  "3D" ]))], 
                                                        collapse  = "; ")) > 0, 
                                           str_c(unique(Pollutant[MLocID_IR_category ==  "3D"])[!is.na(unique(Pollutant[MLocID_IR_category ==  "3D"]))],
                                                 collapse  = "; "),
                                           "."))


wb <- createWorkbook()
addWorksheet(wb, sheetName = "WS_MLocID_param_rollup")
addWorksheet(wb, sheetName = "WS_MlocID_rollup")

header_st <- createStyle(textDecoration = "Bold", border = "Bottom")
freezePane(wb, "WS_MLocID_param_rollup", firstRow = TRUE) 
freezePane(wb, "WS_MlocID_rollup", firstRow = TRUE)

writeData(wb = wb, sheet = "WS_MLocID_param_rollup", x = WS_MLocID_param_rollup, headerStyle = header_st)
writeData(wb = wb, sheet = "WS_MlocID_rollup", x = WS_MlocID_rollup, headerStyle = header_st)

saveWorkbook(wb, 'Rollups/Rollup outputs/MLoc_rollup.xlsx', overwrite = TRUE) 


