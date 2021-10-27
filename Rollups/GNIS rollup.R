library(tidyverse)
library(odeqIRtools)
library(openxlsx)


# Load in rollup function -----------------------------------------------------------------------------------------

source('Rollups/GNIS_Rollup_function.R')
# Load in previous assessment conclusions -------------------------------------------------------------------------


previous_WS_GNIS_assessments <- odeqIRtools::WS_GNIS_previous_listings



# Bring in assessments --------------------------------------------------------------------------------------------


## Temperature -----------------------------------------------------------------------------------------------------


### Year round ------------------------------------------------------------------------------------------------------

Year_rnd_temp_prev_cat <- previous_WS_GNIS_assessments %>%
  filter(Pollu_ID == 132,
         wqstd_code == 12,
         period == 'year_round') %>%
  select(-Char_Name)


temp_yr_WS_station <- read.xlsx('Rollups/Rollup Assessment/temperature-assessments-corrected crit periods.xlsx',
                                sheet = 'YrRnd WS station cat') 

temp_yr_WS_station_GNIS_rollup <- temp_yr_WS_station %>%
  mutate(IR_category = factor(IR_category, levels=c("3", "3B", "2", "5" ), ordered=TRUE),
         AU_GNIS = str_c(AU_ID, AU_GNIS_Name, sep = ";")) %>%
  group_by(AU_ID, AU_GNIS, Pollu_ID, wqstd_code, period) %>%
  summarise(stations =  paste(MLocID, collapse = "; "),
            GNIS_IR_category = max(IR_category)) %>%
  full_join(Year_rnd_temp_prev_cat, by = c("AU_ID", "AU_GNIS", "Pollu_ID", "wqstd_code", "period")) %>%
  mutate(GNIS_IR_category = case_when(is.na(GNIS_IR_category) ~ "Unassessed",
                                      TRUE ~as.character(GNIS_IR_category)),
         GNIS_previous_IR_impairement = case_when(is.na(GNIS_previous_IR_impairement) ~ "Not previously listed",
                                                  TRUE ~ GNIS_previous_IR_impairement)) %>%
  separate(AU_GNIS, c(NA, "AU_GNIS_Name"),sep = ";") %>%
  arrange(AU_ID, AU_GNIS_Name) 


# ## Spawning -----------------------------------------------------------------------------------------------------

Spawn_temp_prev_cat <- previous_WS_GNIS_assessments %>%
  filter(Pollu_ID == 132,
         wqstd_code == 12,
         period == 'Spawn') %>%
  mutate(period = "spawn") %>%
  select(-Char_Name)


temp_spawm_WS_station <- read.xlsx('Rollups/Rollup Assessment/temperature-assessments-corrected crit periods.xlsx',
                                sheet = 'Spawn WS station cat') 



temp_spawn_WS_station_GNIS_rollup <- temp_spawm_WS_station %>%
  mutate(IR_category = factor(IR_category, levels=c("3", "3B", "2", "5" ), ordered=TRUE),
         AU_GNIS = str_c(AU_ID, AU_GNIS_Name, sep = ";")) %>%
  group_by(AU_ID, AU_GNIS,  Pollu_ID, wqstd_code, period) %>%
  summarise(stations =  paste(MLocID, collapse = "; "),
            GNIS_IR_category = max(IR_category)) %>%
  full_join(Spawn_temp_prev_cat, by = c("AU_ID", "AU_GNIS", "Pollu_ID", "wqstd_code", "period")) %>%
  mutate(GNIS_IR_category = case_when(is.na(GNIS_IR_category) ~ "Unassessed",
                                      TRUE ~as.character(GNIS_IR_category)),
         GNIS_previous_IR_impairement = case_when(is.na(GNIS_previous_IR_impairement) ~ "Not previously listed",
                                                  TRUE ~ GNIS_previous_IR_impairement)) %>%
  separate(AU_GNIS, c(NA, "AU_GNIS_Name"),sep = ";") %>%
  arrange(AU_ID, AU_GNIS_Name) 
 

temp_WS_station_GNIS_rollup <- temp_yr_WS_station_GNIS_rollup %>%
  bind_rows(temp_spawn_WS_station_GNIS_rollup) %>%
  ungroup() %>%
  mutate(GNIS_final_IR_category = case_when(GNIS_IR_category == "Unassessed" ~ GNIS_previous_IR_impairement,
                                            GNIS_previous_IR_impairement == 'Not previously listed' ~ GNIS_IR_category,
                                            GNIS_IR_category == '2' ~ '2',
                                            GNIS_IR_category == '5' ~ '5',
                                            grepl('3', GNIS_IR_category) &GNIS_previous_IR_impairement == 'Not previously listed' ~ GNIS_IR_category,
                                            grepl('3', GNIS_IR_category) &GNIS_previous_IR_impairement != 'Not previously listed' ~ GNIS_previous_IR_impairement,
  )) %>%
  mutate(GNIS_final_IR_category = str_remove(GNIS_final_IR_category, 'Category ')) %>%
  mutate(GNIS_final_IR_category = factor(GNIS_final_IR_category, levels=c("3", "3B", "2", "5", '4A' ), ordered=TRUE)) %>%
  group_by(AU_ID, period) %>%
  mutate(AU_final_status = max(GNIS_final_IR_category)) %>%
  ungroup() %>%
  mutate(GNIS_remove_impairment = case_when(GNIS_final_IR_category == 2 & GNIS_previous_IR_impairement %in% c("Category 5", "Category 4A") ~ "Yes",
                                            TRUE ~ "No"
                                            ),
         AU_delist = case_when(AU_final_status == 2 & GNIS_previous_IR_impairement != 'Not previously listed' ~ "Yes",
                               GNIS_previous_IR_impairement == 'Not previously listed' ~ "Not Applicable",
                               TRUE ~ "No"
                               ))%>%
  ungroup() %>%
  arrange(AU_ID, AU_GNIS_Name) 

#write.xlsx(temp_WS_station_GNIS_rollup, file = 'Rollups/Rollup outputs/temperature_GNIS_rollup.xlsx')


## Bacteria --------------------------------------------------------------------------------------------------------


### Freshwater ----------------------------------------------------------------------------------------------------


bacteria_fresh_WS_station <- read.xlsx('Rollups/Rollup Assessment/bacteria freshwater contact.xlsx',
                                   sheet = 'WS station categorization') 


bacteria_fresh_WS_station_GNIS_rollup <- GNIS_rollup(bacteria_fresh_WS_station)

write.xlsx(bacteria_fresh_WS_station_GNIS_rollup, file = 'Rollups/Rollup outputs/bacteria_fresh_GNIS_rollup.xlsx')


### Coastal Contact -------------------------------------------------------------------------------------------------

bacteria_fresh_WS_station <- read.xlsx('Rollups/Rollup Assessment/bacteria coast contact.xlsx',
                                       sheet = 'WS station categorization') 


bacteria_coast_WS_station_GNIS_rollup <- GNIS_rollup(bacteria_fresh_WS_station)

write.xlsx(bacteria_coast_WS_station_GNIS_rollup, file = 'Rollups/Rollup outputs/bacteria_coast_GNIS_rollup.xlsx')