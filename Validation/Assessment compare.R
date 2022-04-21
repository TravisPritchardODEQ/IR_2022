library(tidyverse)
library(openxlsx)

#Read in 2020 IR

load("IR_20.RData")

IR_20 <- IR_20 %>%
  select(AU_ID,Pollutant, Pollu_ID, wqstd_code, period, IR_category) %>%
  rename(IR_category_20 = IR_category) %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code))
 
#Read in 2022 IR

IR_22 <- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2022/Draft List/Public Comment Edits/AU_all_rollup.xlsx") %>%
  select(AU_ID,Pollutant, Pollu_ID, wqstd_code, period, DO_Class, AU_final_status ) %>%
  rename(IR_category_22 = AU_final_status)



#Join together
joined <- full_join(IR_22, IR_20, by = c("AU_ID", "Pollu_ID", "wqstd_code", "period"))

#Data table of assessments in OR20, but not in 22
IR_20_no_22 <- joined %>%
  filter(is.na(IR_category_22)) %>%
  filter(!IR_category_20 %in% c("", "-", "Unassessed"),
         Pollutant.y != 'Dissolved Oxygen')





# Read in new rollup ----------------------------------------------------------------------------------------------

new_rollup <- read.xlsx('Rollups/Rollup outputs/AU_all_rollup-copper.xlsx')


missing_assessments <- joined %>%
  filter(is.na(IR_category_22) & is.na(Pollutant.x)) %>%
  select(AU_ID, Pollutant.y, Pollu_ID, wqstd_code, period, DO_Class, IR_category_20)


missing_assessments_new_rollup <- missing_assessments %>%
  left_join(new_rollup) %>%
  filter(IR_category_20 != "")

missing_to_load <- missing_assessments_new_rollup %>%
  filter(!is.na(AU_Name)) %>%
  select(AU_ID, AU_Name, AU_Description, Pollutant, Assessment, Pollu_ID, wqstd_code, period, DO_Class, stations, AU_final_status,
         assessed_2022, AU_delist, Rationale, previous_rationale, year_assessed, Year_listed, recordID, AU_status_update)

write.xlsx(missing_to_load, "Validation/missing_assessments_to_load.xlsx")



# second_run ------------------------------------------------------------------------------------------------------
load("IR_20.RData")

IR_20_a <- IR_20 %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code))


missing_to_load_2 <- IR_20_no_22 %>%
  left_join(IR_20_a)


missing_to_load_2a <- missing_to_load_2 %>%
  mutate(stations = NA_character_,
         AU_final_status = IR_category_20,
         assessed_2022 = "No",
         AU_delist = "No",
         Rationale = case_when(Rationale != "" ~ paste0("2018: ", Rationale),
                               TRUE ~ Rationale),
         previous_rationale = Rationale,
         year_assessed = case_when(Assessed_in_2018 == "YES" ~ '2018',
                                   TRUE ~ as.character(Year_listed)),
         recordID = paste0(year_assessed, "-", odeqIRtools::unique_AU(AU_ID),Pollu_ID, "-", wqstd_code ),
         AU_status_update = NA_character_
) %>%
  select(AU_ID, AU_Name, AU_Description, Pollutant, Assessment, Pollu_ID, wqstd_code, period, DO_Class, stations, AU_final_status,
         assessed_2022, AU_delist, Rationale, previous_rationale, year_assessed, Year_listed, recordID, AU_status_update)

write.xlsx(missing_to_load_2a, "Validation/missing_assessments_to_load2.xlsx")


