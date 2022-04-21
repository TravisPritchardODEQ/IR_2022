library(tidyverse)
library(odeqIRtools)
library(openxlsx)


# Load in rollup function -----------------------------------------------------------------------------------------

source('Rollups/Other_rollup_function.R')
source('Rollups/poll_asses_join.R')
load('Rollups/rollup helper/AU_to_ben_use.Rdata')


# Load data and rollup ---------------------------------------------------------------------------------------------


## Temperature -----------------------------------------------------------------------------------------------------


### Year round ------------------------------------------------------------------------------------------------------


temp_yr_AU <- read.xlsx('Rollups/Rollup Assessment/temperature-assessments.xlsx',
                                sheet = 'YrRnd Other AU cat') %>%
  AU_rollup_other(periods = TRUE) %>%
  join_pollu_assess()


### Spawn -----------------------------------------------------------------------------------------------------------


temp_spawn_AU <- read.xlsx('Rollups/Rollup Assessment/temperature-assessments.xlsx',
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


### Copper ----------------------------------------------------------------------------------------------------------

tox_AL_copper_AU <- read.xlsx('Rollups/Rollup Assessment/Tox_AL.xlsx',
                                sheet = 'tox_AL_Copper_other_cats') %>%
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
  select(AU_ID, Pollu_ID, wqstd_code,DO_Class, period,  IR_category, Rationale, recordID,Delist, AU_previous_IR_category) 

DO_yr_inst <- read.xlsx('Rollups/Rollup Assessment/DO Year Round.xlsx',
                        sheet = 'Yr Rnd Instant Other AU Cat') %>%
  select(AU_ID, Pollu_ID, wqstd_code,DO_Class, period,  IR_category, Rationale, recordID,Delist, AU_previous_IR_category) 

DO_yr <- bind_rows(DO_yr_cont, DO_yr_inst) %>%
  mutate(recordID = ifelse(!is.na(DO_Class), paste0(recordID, "-", DO_Class), recordID )) %>%
  AU_rollup_other(DO = TRUE) %>%
  join_pollu_assess()



### Spawn -------------------------------------------------------------------------------------------------------



DO_sp_cont <- read.xlsx('Rollups/Rollup Assessment/DO Spawn.xlsx',
                        sheet = 'Spawn Cont Other AU Cat') %>%
  mutate(Delist = as.character(Delist)) %>%
  mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-",
                           wqstd_code,"-", period, "-", DO_Class )) %>%
  select(AU_ID, Pollu_ID, wqstd_code,DO_Class, period,  IR_category, Rationale, recordID,Delist, AU_previous_IR_category) 

DO_sp_inst <- read.xlsx('Rollups/Rollup Assessment/DO Spawn.xlsx',
                        sheet = 'Spawn Instant Other AU Cat') %>%
  mutate(Delist = as.character(Delist)) %>%
  mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-",
                           wqstd_code,"-", period, "-", DO_Class )) %>%
  select(AU_ID, Pollu_ID, wqstd_code,DO_Class, period,  IR_category, Rationale, recordID,Delist, AU_previous_IR_category) 

DO_sp <- bind_rows(DO_sp_cont, DO_sp_inst) %>%
  mutate(period = ifelse(period == 'Spawn', "spawn", period )) %>%
  AU_rollup_other(DO = TRUE) %>%
  join_pollu_assess()



# Biocriteria -----------------------------------------------------------------------------------------------------


MWCF_SS <- read.xlsx('Rollups/Rollup Assessment/Biocriteria.xlsx',
                     sheet = 'MWCF_SS') %>%
  rename(IR_category = IR_Cat,
         GNIS_previous_IR_impairement = `Previous_Impaired_AU:GNIS`,
         AU_previous_IR_category = Previous.Status) %>%
  mutate(IR_category = str_remove(IR_category, 'Cat')) %>%
  mutate(Pollu_ID = "156",
         wqstd_code = "5",
         Delist = as.character(Delist),
         GNIS_previous_IR_impairement =  as.character(GNIS_previous_IR_impairement)) %>%
  mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code )) %>%
  select(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code,IR_category, Rationale,
         GNIS_previous_IR_impairement,AU_previous_IR_category, recordID, Delist)

MWCF_MS <- read.xlsx('Rollups/Rollup Assessment/Biocriteria.xlsx',
                     sheet = 'MWCF_MS') %>%
  rename(IR_category = IR_Cat_ave,
         GNIS_previous_IR_impairement = `Previous_Impaired_AU:GNIS`,
         AU_previous_IR_category = Previous.Status) %>%
  mutate(IR_category = str_remove(IR_category, 'Cat')) %>%
  mutate(Pollu_ID = "156",
         wqstd_code = "5",
         Delist = as.character(Delist),
         GNIS_previous_IR_impairement =  as.character(GNIS_previous_IR_impairement)) %>%
  mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code )) %>%
  select(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code,IR_category, Rationale,
         GNIS_previous_IR_impairement,AU_previous_IR_category, recordID, Delist)



WCCP_SS <- read.xlsx('Rollups/Rollup Assessment/Biocriteria.xlsx',
                     sheet = 'WCCP_SS') %>%
  rename(IR_category = IR_Cat,
         GNIS_previous_IR_impairement = `Previous_Impaired_AU:GNIS`,
         AU_previous_IR_category = Previous.Status) %>%
  mutate(IR_category = str_remove(IR_category, 'Cat')) %>%
  mutate(Pollu_ID = "156",
         wqstd_code = "5",
         Delist = as.character(Delist),
         GNIS_previous_IR_impairement =  as.character(GNIS_previous_IR_impairement)) %>%
  mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code )) %>%
  select(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code,IR_category, Rationale,
         GNIS_previous_IR_impairement,AU_previous_IR_category, recordID, Delist)


WCCP_MS <- read.xlsx('Rollups/Rollup Assessment/Biocriteria.xlsx',
                     sheet = 'WCCP_MS') %>%
  rename(IR_category = IR_Cat_ave,
         GNIS_previous_IR_impairement = `Previous_Impaired_AU:GNIS`,
         AU_previous_IR_category = Previous.Status) %>%
  mutate(IR_category = str_remove(IR_category, 'Cat')) %>%
  mutate(Pollu_ID = "156",
         wqstd_code = "5",
         Delist = as.character(Delist),
         GNIS_previous_IR_impairement =  as.character(GNIS_previous_IR_impairement)) %>%
  mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code )) %>%
  select(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code,IR_category, Rationale,
         GNIS_previous_IR_impairement,AU_previous_IR_category, recordID, Delist)


biocriteria_together <- bind_rows(MWCF_MS, MWCF_SS, WCCP_MS, WCCP_SS)



SR_biocriteria <-  biocriteria_together %>%
  filter(str_detect(AU_ID, "WS", negate = TRUE)) 

biocriteria_AU <- SR_biocriteria %>%
  AU_rollup_other() %>%
  join_pollu_assess()


# HABs ------------------------------------------------------------------------------------------------------------

HABs_AU <- read.xlsx('Rollups/Rollup Assessment/HABs.xlsx') %>%
  group_by(AU_ID) %>%
  summarise(Waterbody = max(Waterbody),
            `2018/2020` = first(`2018/2020`),
            `2022` = first(`2022`),
            Rationale = first(Rationale)) %>%
  rename(IR_category = `2022`,
         AU_previous_IR_category = '2018/2020') %>%
  mutate(IR_category = str_remove(IR_category, 'Cat '),
         Delist = NA_character_,
         Pollu_ID = "174",
         wqstd_code = NA_character_) %>%
  mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code )) %>%
  AU_rollup_other() %>%
  join_pollu_assess()
  



# Unassessed ------------------------------------------------------------------------------------------------------

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
  AU_previous_IR_category = NA_character_,
  Delist = NA_character_,
  recordID = NA_character_
) %>%
  distinct() %>%
  AU_rollup_other() %>%
  filter(!is.na(AU_ID)) %>%
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
                             tox_AL_copper_AU,
                             tox_HH_AU,
                             turbidity_AU,
                             DO_yr,
                             DO_sp,
                             biocriteria_AU,
                             HABs_AU,
                             unassessed) %>%
  arrange(AU_ID, Assessment) %>%
  left_join(select(AU_to_ben_use, AU_ID, AU_Name, AU_Description)) %>%
  select(AU_ID, AU_Name, AU_Description, Pollu_ID,wqstd_code, Assessment,  Pollutant, period, DO_Class,assessed_2022, AU_previous_IR_category,
         `2022_IR_category`,AU_final_status, Rationale, recordID, AU_delist)

load('Rollups/all_other_stations.Rdata')

all_other_stations <- all_other_stations %>%
  mutate( Pollu_ID= as.character(Pollu_ID),
          wqstd_code = as.character(wqstd_code))

rollup_AU_others <- rollup_AU_others %>%
  left_join(all_other_stations) %>%
  distinct()



load('Rollups/rollup helper/AU_prev_cat.Rdata')
AU_prev_cat <- AU_prev_cat %>%
  mutate(wqstd_code = as.character(wqstd_code),
         Pollu_ID = as.character(Pollu_ID)) %>%
  rename(previous_rationale = Rationale) %>%
  select(-IR_category) %>%
  distinct() %>%
  select(-Assessed_in_2018, -DO_Class)

rollup_AU_others <- rollup_AU_others %>%
  left_join(AU_prev_cat, c("AU_ID", "Pollu_ID", "wqstd_code", "period")) %>%
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

TMDL_updates_import <- read.xlsx("//deqHQ1/WQASSESSMENT/2022IRFiles/Communications/DEQ Internal comment/IR_2022_draft_ImpairedWaters_Comments_RM.xlsx",
                                 sheet = 'update lookup')


TMDL_updates <- TMDL_updates_import %>%
  select(-Comment, -action_ID, -TMDL, -recordID, -AU_final_status, -DO_Class) %>%
  distinct()



rollup_AU_others <- rollup_AU_others %>%
  left_join(TMDL_updates) %>%
  mutate(AU_final_status = case_when(AU_final_status %in% c('5', '4A') & !is.na(AU_status_update) ~ AU_status_update,
                                         TRUE ~ as.character(AU_final_status)
  ))

delist_AU_other <- rollup_AU_others %>%
  filter(AU_delist == "Yes") %>%
  select(AU_ID,AU_Name, AU_Description, Pollutant, Assessment,stations, Pollu_ID, wqstd_code, period,DO_Class,AU_final_status, Rationale, AU_delist)

save(rollup_AU_others,delist_AU_other, file = 'Rollups/rollup_AU_others.Rdata')
