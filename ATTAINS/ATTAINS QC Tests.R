library(tidyverse)
library(openxlsx)



# Delistings ------------------------------------------------------------------------------------------------------

# read in AU_ALL deslitings file

Attains_params <- read.csv("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2022/ATTAINS/ATTAINS Downloads/parameters.csv")

delistings <- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2022/Final List/AU_all_rollup.xlsx",
                        sheet = 'Delistings') %>%
  mutate(AU_final_status = ifelse(AU_final_status == '4a', '4A', AU_final_status ))
  

delistings_Attains <- read.csv("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2022/ATTAINS/ATTAINS Downloads/parameters.csv") %>%
  filter(PARAM_DELISTING_REASON != "")

missing_AUs <- setdiff(delistings$AU_ID, delistings_Attains$ASSESSMENT_UNIT_ID)


missing_delistings <- delistings_Attains %>%
  filter(ASSESSMENT_UNIT_ID %in% missing_AUs)




# Random tests ----------------------------------------------------------------------------------------------------


AU_all_rollup_file <- "C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2022/Final List/AU_all_rollup.xlsx"

AU_All <- read.xlsx(AU_all_rollup_file,
                    sheet = "AU_BU")


library(DBI)
con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")


Attains_polluname_LU <- DBI::dbGetQuery(con, "SELECT cast([Pollu_ID] AS varchar) as Pollu_ID
                                        ,[Attains_PolluName]
                                        ,[Attains_Group] 
                                        FROM [IntegratedReport].[dbo].[LU_Pollutant]
                                        "
) 


AU_All_pollutant <- AU_All %>%
  left_join(Attains_polluname_LU) %>%
  mutate(PARAM_USE_NAME = case_when(ben_use == "Fishing" ~ "fishing",
                             ben_use == "Private Domestic Water Supply" ~ "Private domestic water supply",
                             ben_use == "Public Domestic Water Supply" ~ "Public domestic water supply",
                             ben_use == "Fish and Aquatic Life" ~ "fish and aquatic life",
                             ben_use == "Water Contact Recreation" ~ "water contact recreation",
                             ben_use == "Aesthetic Quality" ~ "aesthetic quality",
                             ben_use == "Livestock Watering" ~ "livestock watering",
                             ben_use == "Boating" ~ "boating",
                             TRUE ~ "ERROR"
  )) %>%
  mutate(PARAM_USE_NAME = case_when(PARAM_USE_NAME == 'fish and aquatic life' & period == 'spawn' ~ 'fish and aquatic life - spawning',
                             TRUE ~ PARAM_USE_NAME))


Attains_params <- read.csv("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2022/ATTAINS/ATTAINS Downloads/parameters.csv")

Attains_params_select <- Attains_params %>%
  select(ASSESSMENT_UNIT_ID, PARAM_NAME, PARAM_USE_NAME, PARAM_ATTAINMENT_CODE, PARAM_STATE_IR_CAT) %>%
  rename(AU_ID = ASSESSMENT_UNIT_ID,
         Attains_PolluName = PARAM_NAME)

AU_All_pollutant_2 <- AU_All_pollutant %>%
  left_join(Attains_params_select) %>%
  mutate(AU_ALL_attainment_code = case_when(str_detect(AU_parameter_category, '5') | str_detect(AU_parameter_category, '4') ~ 'Not meeting criteria',
                                            AU_parameter_category %in% c('2') ~ 'Meeting criteria',
                                            str_detect(AU_parameter_category, '3') ~ 'Not enough information')) 


AU_All_pollutant_2_diffs <- AU_All_pollutant_2 %>%
  filter(AU_ALL_attainment_code != PARAM_ATTAINMENT_CODE)


PARAM_ATTAINMENT_CODE = case_when(str_detect(AU_parameter_category, '5') | str_detect(AU_parameter_category, '4') ~ 'Not meeting criteria',
                                  AU_parameter_category %in% c('2') ~ 'Meeting Criteria',
                                  str_detect(AU_parameter_category, '3') ~ 'Not enough information')





rand_num <- sample(1:nrow(Attains_params), 100)


random_Attains <- Attains_params %>%
  filter(row_number() %in% rand_num)


