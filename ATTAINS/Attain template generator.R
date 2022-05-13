library(tidyverse)
library(lubridate)
library(openxlsx)


# Setup -----------------------------------------------------------------------------------------------------------

AU_all_rollup_file <- "C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2022/Final List/AU_all_rollup.xlsx"

AU_All <- read.xlsx(AU_all_rollup_file)

AU_All_2022 <- AU_All %>%
  filter(year_assessed == 2022) %>%
  mutate(AU_parameter_category = factor(AU_parameter_category, 
                                        levels=c("Unassessed", '3D',"3", "3B", "3C", "2", '4B', '4C', '4','4A', "5" ), 
                                        ordered=TRUE)) %>%
  group_by(AU_ID, Pollu_ID, wqstd_code, period) %>%
  filter(AU_parameter_category == max(AU_parameter_category)) %>%
  filter(row_number()==1)

# Assessments Tab ---------------------------------------------------------------------------------------------------------------

Assessments <- AU_All_2022 %>%
  ungroup() %>%
  select(AU_ID) %>%
  distinct() %>%
  rename(ASSESSMENT_UNIT_ID = AU_ID) %>%
  mutate(Agency_Code = 'S',
         CYCLE_LAST_ASSESSED = 2022) 
  
  


# Use Tab-----------------------------------------------------------------------------------------------------------------

AU_BU <- read.xlsx(AU_all_rollup_file,
                   sheet = "AU_BU") 


AU_BU_2022 <- AU_BU %>%
  group_by(AU_ID, Pollu_ID, wqstd_code, period, ben_use) %>%
  filter(AU_parameter_category == max(AU_parameter_category)) %>%
  filter(row_number()==1) %>%
  filter(AU_ID %in% AU_All_2022$AU_ID) 

Uses <- AU_BU_2022 %>%
  mutate(AU_parameter_category = factor(AU_parameter_category, 
                                        levels=c("Unassessed", '3D',"3", "3B", "3C", "2", '4B', '4C', '4','4A', "5" ), 
                                        ordered=TRUE)) %>%
  mutate(ben_use = case_when(ben_use == "Fishing" ~ "fishing",
                             ben_use == "Private Domestic Water Supply" ~ "Private domestic water supply",
                             ben_use == "Public Domestic Water Supply" ~ "Public domestic water supply",
                             ben_use == "Fish and Aquatic Life" ~ "fish and aquatic life",
                             ben_use == "Water Contact Recreation" ~ "water contact recreation",
                             ben_use == "Aesthetic Quality" ~ "aesthetic quality",
                             ben_use == "Livestock Watering" ~ "livestock watering",
                             ben_use == "Boating" ~ "boating",
                             TRUE ~ "ERROR"
  )) %>%
  mutate(ben_use = case_when(ben_use == 'fish and aquatic life' & period == 'spawn' ~ 'fish and aquatic life - spawning',
                             TRUE ~ ben_use)) %>%
  mutate(pollutant_strd = case_when(!is.na(period) ~ paste0(Pollutant, "- ", period),
                                    wqstd_code == 15 ~  paste0(Pollutant, "- Aquatic Life Toxics"),
                                    wqstd_code == 16 ~  paste0(Pollutant, "- Human Health Toxics"),
                                    TRUE ~ Pollutant),
         pollutant_strd_cat = paste0(AU_parameter_category, ": ", pollutant_strd)) %>%
  group_by(AU_ID, ben_use) %>%
  summarise(Category = max(AU_parameter_category),
            USE_COMMENT = str_c(pollutant_strd_cat, sep = ";", collapse = "; " )) %>%
  ungroup() %>%
  mutate(USE_COMMENT = str_sub(USE_COMMENT,1,3996),
         USE_ATTAINMENT_CODE = case_when(str_detect(Category, '5') | str_detect(Category, '4') ~ 'N',
                                         Category %in% c('2') ~ 'F',
                                         str_detect(Category, '3') ~ 'I' )) %>%
  select(AU_ID, ben_use, USE_ATTAINMENT_CODE, Category, USE_COMMENT) %>%
  rename(ASSESSMENT_UNIT_ID = AU_ID,
         USE_NAME = ben_use) %>%
  mutate(USE_AGENCY_CODE = 'S',
         USE_TREND = NA_character_,
         USE_THREATENED = NA_character_,
         USE_ASMT_BASIS = NA_character_,
         USE_MONITORING_START = NA_character_,
         USE_MONITORING_END= NA_character_,
         USE_ASMT_DATE = NA_character_,
         USE_ASSESSOR_NAME = NA_character_,
         USE_COMMENT = USE_COMMENT,
         USE_STATE_IR_CAT = Category,
         USE_ORG_QUALIFIER_FLAG = NA_character_ ) %>%
  select(ASSESSMENT_UNIT_ID,USE_NAME, USE_ATTAINMENT_CODE, USE_AGENCY_CODE, USE_TREND, USE_THREATENED,USE_ASMT_BASIS,
         USE_MONITORING_START, USE_MONITORING_END, USE_ASMT_DATE, USE_ASSESSOR_NAME, USE_COMMENT,
         USE_STATE_IR_CAT, USE_ORG_QUALIFIER_FLAG)



# Parameters tab --------------------------------------------------------------------------------------------------


## Get attains pollutant name --------------------------------------------------------------------------------------

library(DBI)
con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")


Attains_polluname_LU <- DBI::dbGetQuery(con, "SELECT cast([Pollu_ID] AS varchar) as Pollu_ID
                                        ,[Attains_PolluName]
                                        ,[Attains_Group] 
                                        FROM [IntegratedReport].[dbo].[LU_Pollutant]
                                        "
) 

delistings <- read.xlsx(AU_all_rollup_file,
                        sheet = "Delistings") %>%
  select(AU_ID, Pollu_ID, wqstd_code, period, DO_Class, Attains_code, ATTAINS_delist_reason)

delist_dups <- delistings %>%
  group_by(AU_ID, Pollu_ID, wqstd_code, period, DO_Class) %>%
  summarise(n = n()) %>%
  filter(n > 1)

if(nrow(delist_dups) > 0 ) {
  stop('delising dups')
}


parameters0 <- AU_BU_2022 %>%
  left_join(Attains_polluname_LU) %>%
  left_join(delistings) %>%
  mutate(ben_use = case_when(ben_use == "Fishing" ~ "fishing",
                             ben_use == "Private Domestic Water Supply" ~ "Private domestic water supply",
                             ben_use == "Public Domestic Water Supply" ~ "Public domestic water supply",
                             ben_use == "Fish and Aquatic Life" ~ "fish and aquatic life",
                             ben_use == "Water Contact Recreation" ~ "water contact recreation",
                             ben_use == "Aesthetic Quality" ~ "aesthetic quality",
                             ben_use == "Livestock Watering" ~ "livestock watering",
                             ben_use == "Boating" ~ "boating",
                             TRUE ~ "ERROR"
  )) %>%
  mutate(ben_use = case_when(ben_use == 'fish and aquatic life' & period == 'spawn' ~ 'fish and aquatic life - spawning',
                             TRUE ~ ben_use))
  

Parameters <- parameters0 %>%
  mutate(AU_parameter_category = factor(AU_parameter_category, 
                                        levels=c("Unassessed", '3D',"3", "3B", "3C", "2", '4B', '4C', '4','4A', "5" ), 
                                        ordered=TRUE)) %>%
  group_by(AU_ID, Attains_PolluName) %>%
  mutate(PARAM_STATUS_NAME =case_when(str_detect(max(AU_parameter_category), '5') | str_detect(max(AU_parameter_category), '4') ~ 'Cause',
                                      max(AU_parameter_category) %in% c('2') ~ 'Meeting Criteria',
                                      str_detect(max(AU_parameter_category), '3') ~ 'Insufficient Information' 
                                      ),
         PARAM_ATTAINMENT_CODE = case_when(str_detect(AU_parameter_category, '5') | str_detect(AU_parameter_category, '4') ~ 'Not meeting criteria',
                                            AU_parameter_category %in% c('2') ~ 'Meeting Criteria',
                                            str_detect(AU_parameter_category, '3') ~ 'Not enough information'),
         PARAM_TREND = NA_character_,
         PARAM_COMMENT = Rationale,
         PARAM_AGENCY_CODE = "S",
         PARAM_POLLUTANT_INDICATOR = case_when(PARAM_STATUS_NAME == 'Cause' & Attains_PolluName %in% c('HABITAT ALTERATIONS', 'FLOW REGIME MODIFICATION', 'BENTHIC MACROINVERTEBRATES BIOASSESSMENTS') ~ 'N',
                                         PARAM_STATUS_NAME == 'Cause' ~ 'Y',
                                         TRUE ~ NA_character_),
         PARAM_YEAR_LISTED = case_when(PARAM_STATUS_NAME == 'Cause' ~ Year_listed,
                                        TRUE ~ NA_character_),
         PARAM_TARGET_TMDL_DATE = NA_character_,
         PARAM_EXPECTED_TO_ATTAIN = NA_character_,
         PARAM_PRIORITY_RANKING = case_when(str_detect(AU_parameter_category, '4') & is.na(TMDL_Priority) ~ 'Low',
                                            TRUE ~ TMDL_Priority),
         PARAM_CONSENT_DECREE_CYCLE = NA_character_,
         PARAM_ALT_LISTING_ID = NA_character_,
         PARAM_STATE_IR_CAT = AU_parameter_category,
         PARAM_ORG_QUALIFIER_FLAG = NA_character_,
         PARAM_DELISTING_REASON = Attains_code,
         PARAM_DELISTING_COMMENT = ATTAINS_delist_reason,
         PARAM_DELISTING_AGENCY = ifelse(!is.na(PARAM_DELISTING_REASON), 'S', NA_character_ )
         
         ) %>%
  select(AU_ID, Attains_PolluName, ben_use, PARAM_STATUS_NAME, PARAM_ATTAINMENT_CODE,PARAM_TREND,
         PARAM_COMMENT, PARAM_AGENCY_CODE, PARAM_POLLUTANT_INDICATOR,  PARAM_YEAR_LISTED, PARAM_TARGET_TMDL_DATE,
         PARAM_EXPECTED_TO_ATTAIN, PARAM_PRIORITY_RANKING, PARAM_CONSENT_DECREE_CYCLE,PARAM_ALT_LISTING_ID, 
         PARAM_STATE_IR_CAT, PARAM_ORG_QUALIFIER_FLAG, PARAM_DELISTING_REASON, PARAM_DELISTING_COMMENT, 
         PARAM_DELISTING_AGENCY)



# Seasons ---------------------------------------------------------------------------------------------------------


seasons_2020 <- read.csv("C:/Users/tpritch/Documents/IR_2022/ATTAINS/2020 Download/seasons.csv") %>%
  select(ASSESSMENT_UNIT_ID, PARAM_NAME, PARAM_USE_NAME, PARAM_ATTAINMENT_CODE)

seasons0 <- AU_BU_2022 %>%
  filter(period == "spawn") %>%
  left_join(Attains_polluname_LU)

AUs <- unique(seasons0$AU_ID)

## query spawn dates ---------------------------------------------------------------------------------------------

con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")

query_language <- "SELECT distinct [AU_ID]
      ,cast(Pollu_ID AS varchar) as Pollu_ID
      ,[SpawnCode] as temp_SpawnCode
      ,[DO_SpawnCode]
      
  FROM [IntegratedReport].[dbo].[InputRaw]
WHERE AU_ID in   ({AUs*})"

spawn_query <- glue::glue_sql(query_language, .con = con)


Spawn_codes <- DBI::dbGetQuery(con, spawn_query)
Spawn_dates <-  DBI::dbGetQuery(con, "SELECT TOP (1000) [SpawnCode]
      ,[SpawnStart]
      ,[SpawnEnd]
  FROM [IntegratedReport].[dbo].[LU_Spawn]")


Seasons_1 <- seasons0 %>%
  select(AU_ID, Pollu_ID, Attains_PolluName,ben_use, AU_parameter_category  ) %>%
  mutate(AU_parameter_category = factor(AU_parameter_category, 
                                        levels=c("Unassessed", '3D',"3", "3B", "3C", "2", '4B', '4C', '4','4A', "5" ), 
                                        ordered=TRUE)) %>%
  left_join(Spawn_codes) %>%
  mutate(SpawnCode = case_when(Attains_PolluName == 'TEMPERATURE' ~ temp_SpawnCode,
                               Attains_PolluName == "DISSOLVED OXYGEN" ~ DO_SpawnCode)) %>%
  filter(!is.na(SpawnCode),
         SpawnCode != 0,
         SpawnCode != 99 ) %>%
  select(-temp_SpawnCode, -DO_SpawnCode) %>%
  left_join(Spawn_dates) %>%
  filter(!is.na(SpawnStart)) %>%
  mutate( SpawnStart = ifelse(!is.na(SpawnStart), paste0(SpawnStart, "/",2020 ), SpawnStart ),
          SpawnEnd= ifelse(!is.na(SpawnEnd), paste0(SpawnEnd, "/", 2020), SpawnEnd ),
          SpawnStart = mdy(SpawnStart),
          SpawnEnd=mdy(SpawnEnd),
          SpawnEnd = if_else(SpawnEnd < SpawnStart, SpawnEnd + lubridate::years(1), # add a year if in spawn period carrying to next year
                             SpawnEnd),
          SpawnStart = if_else(SpawnEnd < SpawnStart, SpawnStart - lubridate::years(1), # subtract a year if in spawn period carrying from previous year
                               SpawnStart),
          length = SpawnEnd - SpawnStart + 1
  ) %>%
  group_by(AU_ID, Attains_PolluName, ben_use) %>%
  mutate(Category = max(AU_parameter_category)) %>%
  filter(length == max(length)) %>%
  group_by(AU_ID, Attains_PolluName, ben_use) %>%
  filter(row_number()==1) %>%
  mutate(PARAM_ATTAINMENT_CODE = case_when(str_detect(Category, '5') | str_detect(Category, '4') ~ 'Not meeting criteria',
                                           Category %in% c('2') ~ 'Meeting Criteria',
                                           str_detect(Category, '3') ~ 'Not enough information'),
         SEASON_START = format(SpawnStart, format="%d-%b"),
         SEASON_END = format(SpawnEnd, format="%d-%b")) %>%
  mutate(ben_use = case_when(ben_use == "Fishing" ~ "fishing",
                             ben_use == "Private Domestic Water Supply" ~ "Private domestic water supply",
                             ben_use == "Public Domestic Water Supply" ~ "Public domestic water supply",
                             ben_use == "Fish and Aquatic Life" ~ 'fish and aquatic life - spawning',
                             ben_use == "Water Contact Recreation" ~ "water contact recreation",
                             ben_use == "Aesthetic Quality" ~ "aesthetic quality",
                             ben_use == "Livestock Watering" ~ "livestock watering",
                             ben_use == "Boating" ~ "boating",
                             TRUE ~ "ERROR"
  )) %>%
  ungroup() %>%
  select(AU_ID, Attains_PolluName, ben_use,PARAM_ATTAINMENT_CODE, SEASON_START, SEASON_END ) %>%
  rename(ASSESSMENT_UNIT_ID = AU_ID, 
         PARAM_NAME = Attains_PolluName,
         PARAM_USE_NAME = ben_use)

Seasons <- Seasons_1 %>%
  anti_join(seasons_2020)


# Associated-action -----------------------------------------------------------------------------------------------

AU_All <- read.xlsx(AU_all_rollup_file)


Associated_actions <- AU_All %>%
  filter(!is.na(action_ID)) %>%
  left_join(Attains_polluname_LU) %>%
  select(AU_ID, Attains_PolluName, action_ID) %>%
  mutate(ACTION_TYPE = 'TMDL') %>%
  distinct()



# write excel doc -------------------------------------------------------------------------------------------------

wb <- createWorkbook()
addWorksheet(wb, sheetName = "Assessments")
addWorksheet(wb, sheetName = "Uses")
addWorksheet(wb, sheetName = "Parameters")
addWorksheet(wb, sheetName = "Seasons")
addWorksheet(wb, sheetName = "Associated_actions")

writeData(wb = wb, sheet = "Assessments", x = Assessments)
writeData(wb = wb, sheet = "Uses", x = Uses)
writeData(wb = wb, sheet = "Parameters", x = Parameters)
writeData(wb = wb, sheet = "Seasons", x = Seasons)
writeData(wb = wb, sheet = "Associated_actions", x = Associated_actions)


saveWorkbook(wb, 'C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2022/ATTAINS/ATTAINS_R_output.xlsx', overwrite = TRUE) 



