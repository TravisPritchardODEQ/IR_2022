library(tidyverse)
library(odeqIRtools)
library(openxlsx)


# Load in rollup function -----------------------------------------------------------------------------------------

source('Rollups/GNIS_Rollup_function.R')
source('Rollups/poll_asses_join.R')
load('Rollups/rollup helper/AU_to_ben_use.Rdata')
# Bring in assessments --------------------------------------------------------------------------------------------


## Temperature -----------------------------------------------------------------------------------------------------


### Year round ------------------------------------------------------------------------------------------------------



temp_yr_WS_station <- read.xlsx('Rollups/Rollup Assessment/temperature-assessments.xlsx',
                                sheet = 'YrRnd WS station cat') %>%
  GNIS_rollup(periods = TRUE) %>%
  join_pollu_assess()%>%   mutate(GNIS_IR_category = as.character(GNIS_IR_category))




# ## Spawning -----------------------------------------------------------------------------------------------------



temp_spawn_WS_station <- read.xlsx('Rollups/Rollup Assessment/temperature-assessments.xlsx',
                                sheet = 'Spawn WS station cat') %>%
  GNIS_rollup(periods = TRUE) %>%
  join_pollu_assess()%>%   mutate(GNIS_IR_category = as.character(GNIS_IR_category))


#write.xlsx(temp_WS_station_GNIS_rollup, file = 'Rollups/Rollup outputs/temperature_GNIS_rollup.xlsx')


## Bacteria --------------------------------------------------------------------------------------------------------


### Freshwater ----------------------------------------------------------------------------------------------------


bacteria_fresh_WS_station <- read.xlsx('Rollups/Rollup Assessment/bacteria freshwater contact.xlsx',
                                   sheet = 'WS station categorization') 


bacteria_fresh_WS_station_GNIS_rollup <- GNIS_rollup(bacteria_fresh_WS_station)
bacteria_fresh_WS_station_GNIS_rollup <- join_pollu_assess(bacteria_fresh_WS_station_GNIS_rollup)

#write.xlsx(bacteria_fresh_WS_station_GNIS_rollup, file = 'Rollups/Rollup outputs/bacteria_fresh_GNIS_rollup.xlsx')


### Coastal Contact -------------------------------------------------------------------------------------------------

# bacteria_coast_WS_station <- read.xlsx('Rollups/Rollup Assessment/bacteria coast contact.xlsx',
#                                        sheet = 'WS station categorization')  %>%
#   GNIS_rollup() %>%
#   join_pollu_assess()%>%   mutate(GNIS_IR_category = as.character(GNIS_IR_category))



#write.xlsx(bacteria_coast_WS_station_GNIS_rollup, file = 'Rollups/Rollup outputs/bacteria_coast_GNIS_rollup.xlsx')


## Chl -----------------------------------------------------------------------------------------------------------

chl_WS_station <- read.xlsx("Rollups/Rollup Assessment/chl-a.xlsx",
                            sheet = 'WS station categorization') %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale)) %>%
  GNIS_rollup() %>%
  join_pollu_assess()%>%   mutate(GNIS_IR_category = as.character(GNIS_IR_category))


## DO --------------------------------------------------------------------------------------------------------------


### DO year round  ----------------------------------------------------------------------------------------------

DO_yr_cont <- read.xlsx('Rollups/Rollup Assessment/DO Year Round.xlsx',
                        sheet = 'Yr Rnd Cont WS Station Cat') %>%
  select(AU_ID, AU_GNIS_Name, Pollu_ID, wqstd_code,DO_Class, period, MLocID, IR_category, Rationale, Delist, GNIS_previous_IR_impairement) %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale))

DO_yr_inst <- read.xlsx('Rollups/Rollup Assessment/DO Year Round.xlsx',
                        sheet = 'Yr Rnd Instant WS Station Cat') %>%
  select(AU_ID, AU_GNIS_Name, Pollu_ID, wqstd_code,DO_Class, period, MLocID, IR_category, Rationale, Delist, GNIS_previous_IR_impairement) %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale))

DO_yr <- bind_rows(DO_yr_cont, DO_yr_inst) %>%
  GNIS_rollup(periods = TRUE, DO = TRUE) %>%
  join_pollu_assess()%>%   mutate(GNIS_IR_category = as.character(GNIS_IR_category))


### DO Spawn --------------------------------------------------------------------------------------------------------

DO_sp_cont <- read.xlsx('Rollups/Rollup Assessment/DO Spawn.xlsx',
                        sheet = 'Spawn Cont WS Station Cat') %>%
  mutate(period = ifelse(period == 'Spawn', "spawn", period ),
         Delist = as.character(Delist)) %>%
  select(AU_ID, AU_GNIS_Name, Pollu_ID, wqstd_code,DO_Class, period, MLocID, IR_category, Rationale, Delist, GNIS_previous_IR_impairement) %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale))

DO_sp_inst <- read.xlsx('Rollups/Rollup Assessment/DO Spawn.xlsx',
                        sheet = 'Spawn Instant WS Station Cat') %>%
  mutate(period = ifelse(period == 'Spawn', "spawn", period ),
         Delist = as.character(Delist)) %>%
  select(AU_ID, AU_GNIS_Name, Pollu_ID, wqstd_code,DO_Class, period, MLocID, IR_category, Rationale, Delist, GNIS_previous_IR_impairement) %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale))

DO_spawn <- bind_rows(DO_sp_cont, DO_sp_inst) %>%
  GNIS_rollup(periods = TRUE, DO = TRUE) %>%
  join_pollu_assess()%>%   mutate(GNIS_IR_category = as.character(GNIS_IR_category))

##  pH ------------------------------------------------------------------------------------------------------------

ph_WS_station <- read.xlsx('Rollups/Rollup Assessment/pH.xlsx',
                           sheet = 'pH WS station cat') %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale)) %>%
  GNIS_rollup() %>%
  join_pollu_assess()%>%   mutate(GNIS_IR_category = as.character(GNIS_IR_category))


## Tox_AL ----------------------------------------------------------------------------------------------------------


### others ------------------------------------------------------------------------------------------------------


tox_AL_other_WS_station <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                                     sheet = 'tox_AL_WS_cats') %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale)) %>%
  GNIS_rollup() %>%
  join_pollu_assess()%>%   mutate(GNIS_IR_category = as.character(GNIS_IR_category))


### hardness metals -------------------------------------------------------------------------------------------------

tox_AL_hard_WS_station <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                                     sheet = 'tox_AL_hard_WS_cats') %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale)) %>%
  GNIS_rollup() %>%
  join_pollu_assess()%>%   mutate(GNIS_IR_category = as.character(GNIS_IR_category))



### penta -----------------------------------------------------------------------------------------------------------

tox_AL_penta_WS_station <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                                    sheet = 'tox_AL_penta_WS_cats') %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale)) %>%
  GNIS_rollup() %>%
  join_pollu_assess()%>%   mutate(GNIS_IR_category = as.character(GNIS_IR_category))



### Ammonia --------------------------------------------------------------------------------------------------------


tox_AL_ammonia_WS_station <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                                     sheet = 'tox_AL_Ammonia_WS_cats') %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale)) %>%
  GNIS_rollup() %>%
  join_pollu_assess()%>%   mutate(GNIS_IR_category = as.character(GNIS_IR_category))



### Aluminum --------------------------------------------------------------------------------------------------------

tox_AL_aluminum_WS_station <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                                       sheet = 'tox_AL_Aluminum_WS_cats') %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale)) %>%
  GNIS_rollup() %>%
  join_pollu_assess()%>%   mutate(GNIS_IR_category = as.character(GNIS_IR_category))



## Tox HH ----------------------------------------------------------------------------------------------------------

tox_HH_WS_station <- read.xlsx('Rollups/Rollup Assessment/Tox_HH.xlsx',
                                        sheet = 'HH Tox WS Station Cat') %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale)) %>%
  GNIS_rollup() %>%
  join_pollu_assess()%>%   mutate(GNIS_IR_category = as.character(GNIS_IR_category))


# Turbidity ---------------------------------------------------------------------------------------------------------
turbidity_WS_station <- read.xlsx('Rollups/Rollup Assessment/turbidity.xlsx',
                               sheet = 'Turb WS categorization') %>%
  mutate(Rationale = paste0(MLocID, ":", " ", Rationale)) %>%
  GNIS_rollup() %>%
  join_pollu_assess()%>%   
  mutate(GNIS_IR_category = as.character(GNIS_IR_category))



# Biocriteria -----------------------------------------------------------------------------------------------------

MWCF_SS <- read.xlsx('Rollups/Rollup Assessment/Biocriteria.xlsx',
                     sheet = 'MWCF_SS') %>%
  rename(IR_category = IR_Cat,
         GNIS_previous_IR_impairement = `Previous_Impaired_AU:GNIS`) %>%
  mutate(IR_category = str_remove(IR_category, 'Cat')) %>%
  mutate(Pollu_ID = "156",
         wqstd_code = "5",
         Delist = as.character(Delist)) %>%
  select(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code,IR_category, Rationale, GNIS_previous_IR_impairement, Delist)

MWCF_MS <- read.xlsx('Rollups/Rollup Assessment/Biocriteria.xlsx',
                     sheet = 'MWCF_MS')%>%
  rename(IR_category = IR_Cat_ave,
         GNIS_previous_IR_impairement = `Previous_Impaired_AU:GNIS`) %>%
  mutate(IR_category = str_remove(IR_category, 'Cat')) %>%
  mutate(Pollu_ID = "156",
         wqstd_code = "5",
         Delist = as.character(Delist)) %>%
  select(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code,IR_category, Rationale, GNIS_previous_IR_impairement, Delist)


WCCP_SS <- read.xlsx('Rollups/Rollup Assessment/Biocriteria.xlsx',
                     sheet = 'WCCP_SS') %>%
  rename(IR_category = IR_Cat) %>%
  mutate(IR_category = str_remove(IR_category, 'Cat')) %>%
  mutate(Pollu_ID = "156",
         wqstd_code = "5",
         Delist = as.character(Delist)) %>%
  select(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code,IR_category, Rationale, Delist)

WCCP_MS <-read.xlsx('Rollups/Rollup Assessment/Biocriteria.xlsx',
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
  GNIS_rollup() %>%
  join_pollu_assess()%>%  
  mutate(GNIS_IR_category = as.character(GNIS_IR_category))




# Unassessed params ------------------------------------------------------------------------------------------------------
load('Rollups/unassessed_prev_AU_cat.Rdata')



unassessed <- data.frame(
  stringsAsFactors = FALSE,
  Pollu_ID = as.character(unassessed_prev_AU_cat$Pollu_ID),
  wqstd_code = as.character(unassessed_prev_AU_cat$wqstd_code),
  AU_ID = NA_character_,
  AU_GNIS_Name = NA_character_,
  MLocID = NA_character_,
  IR_category= NA_character_,
  Rationale = NA_character_,
  GNIS_previous_IR_impairement = NA_character_,
  Delist = NA_character_
) %>%
  distinct() %>%
  GNIS_rollup() %>%
  filter(!is.na(AU_ID)) %>%
  filter( !(GNIS_IR_category == 'Unassessed' & GNIS_previous_IR_impairement == "Not previously listed")) %>%
  join_pollu_assess()


# All put together ------------------------------------------------------------------------------------------------

WS_GNIS_param_rollup <- bind_rows(temp_yr_WS_station,
                            temp_spawn_WS_station,
                            bacteria_fresh_WS_station_GNIS_rollup,
                            #bacteria_coast_WS_station,
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
                            biocriteria_WS_station,
                            unassessed
                            ) %>%
  mutate(Rationale = str_replace_all(Rationale, 'Â', '')) %>%
  left_join(select(AU_to_ben_use, AU_ID, AU_Name, AU_Description))


num_col <-  ncol(WS_GNIS_param_rollup)
num_col_2 <- num_col-2

WS_GNIS_param_rollup <- WS_GNIS_param_rollup[,c(1, 
                                                 num_col-1,
                                                 num_col, 
                                                 2:num_col_2)]

WS_GNIS_rollup <- WS_GNIS_param_rollup %>%
  filter(!is.na(AU_GNIS_Name)) %>%
  mutate(GNIS_final_IR_category = ifelse(is.na(GNIS_final_IR_category), AU_previous_IR_category, as.character(GNIS_final_IR_category) )) %>%
  mutate(GNIS_final_IR_category = factor(GNIS_final_IR_category, levels=c("Unassessed", '3D',"3", "3B", "3C", "2", "5", '4A' ), ordered=TRUE),) %>%
  group_by(AU_ID,AU_Name,AU_Description, AU_GNIS_Name) %>%
  summarise(GNIS_status =  case_when(all(is.na(GNIS_final_IR_category)) & max(AU_final_status) %in% c("4B","4C","4A", "5","4" ) ~ "Impaired",
                                   all(is.na(GNIS_final_IR_category)) & max(AU_final_status) %in% c("3C","3D","3B", "3", "3C") ~ "Insufficient Data",
                                   all(is.na(GNIS_final_IR_category)) & max(AU_final_status) %in% c("2","3D","3B") ~ "Attaining",
                                   max(GNIS_final_IR_category, na.rm = TRUE) %in% c("4B","4C","4A", "5","4" ) ~ "Impaired",
                                max(GNIS_final_IR_category, na.rm = TRUE) %in% c("3C","3D","3B", "3", "3C") ~ "Insufficient Data",
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
  mutate(AU_final_status =factor(AU_final_status, levels=c("Unassessed", '3D',"3", "3B", "3C","2", "5", '4A', '4B', '4C' ), ordered=TRUE),
         Rationale = str_replace_all(Rationale, 'Â', '')) %>%
  group_by(AU_ID, AU_Name,AU_Description, Pollutant, Assessment, Pollu_ID, wqstd_code, period, DO_Class) %>%
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
                                   TRUE ~ 2022),
         Year_listed = case_when(is.na(Year_listed) & AU_final_status %in% c('5', '4A') ~ 2022,
                                 TRUE ~ Year_listed)) %>%
  mutate(recordID = case_when(is.na(period) ~ paste0(year_assessed, "-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code),
                              !is.na(DO_Class) ~ paste0(year_assessed, "-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-",
                                                        wqstd_code,"-", period, "-", DO_Class ),
                              !is.na(period) & is.na(DO_Class) ~paste0(year_assessed, "-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-",
                                                                      wqstd_code,"-", period) ) )

delistings_GNIS <- WS_GNIS_param_rollup %>%
  filter(GNIS_remove_impairment == "Yes")

delistings_WS_AU <- WS_AU_rollup %>%
  filter(AU_delist == "Yes") %>%
  select(AU_ID, AU_Name, AU_Description, Pollutant, Assessment,stations, Pollu_ID, wqstd_code, period,DO_Class,AU_final_status, Rationale, AU_delist)


WS_GNIS_param_rollup2 <- WS_GNIS_param_rollup %>%
  rename(GNIS_22_category = GNIS_IR_category,
         GNIS_final_category = GNIS_final_IR_category) %>%
  filter(!is.na(GNIS_final_category))

wb <- createWorkbook()
addWorksheet(wb, sheetName = "WS_GNIS_param_rollup")
addWorksheet(wb, sheetName = "GNIS_Map_Display")
addWorksheet(wb, sheetName = "WS_AU_rollup")

header_st <- createStyle(textDecoration = "Bold", border = "Bottom")
freezePane(wb, "WS_GNIS_param_rollup", firstRow = TRUE) 
freezePane(wb, "WS_AU_rollup", firstRow = TRUE)
freezePane(wb, "GNIS_Map_Display", firstRow = TRUE)

writeData(wb = wb, sheet = "WS_GNIS_param_rollup", x = WS_GNIS_param_rollup2, headerStyle = header_st)
writeData(wb = wb, sheet = "WS_AU_rollup", x = WS_AU_rollup, headerStyle = header_st)
writeData(wb = wb, sheet = "GNIS_Map_Display", x = WS_GNIS_rollup, headerStyle = header_st)

saveWorkbook(wb, 'Rollups/Rollup outputs/GNIS_rollup.xlsx', overwrite = TRUE) 



save(WS_AU_rollup, WS_GNIS_rollup,WS_GNIS_param_rollup,delistings_GNIS, delistings_WS_AU, file = 'Rollups/WS_AU_GNIS_rollup.Rdata')
