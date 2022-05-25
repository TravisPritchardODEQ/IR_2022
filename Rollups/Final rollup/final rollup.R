
#' This script will remake the rollup files based on the RTC edits
#' Make sure to change inputs to the teams version. Documents version is just for development purposes


library(tidyverse)
library(openxlsx)



# Remake rollup files ---------------------------------------------------------------------------------------------


## Mloc rollup -----------------------------------------------------------------------------------------------------


WS_MLocID_param_rollup <- read.xlsx("C:/Users/tpritch/Documents/IR_2022/Rollups/Final rollup/MLoc_rollup.xlsx")

WS_MlocID_rollup <- WS_MLocID_param_rollup %>%
  mutate(Pollutant = case_when(!is.na(period) ~ paste0(Pollutant, "- ", period),
                               wqstd_code == 15 ~ paste0(Pollutant, "- Aquatic Life"),
                               wqstd_code == 16 ~ paste0(Pollutant, "- Human Health"),
                               TRUE ~ Pollutant
  )) %>%
  group_by(AU_ID, AU_Name,AU_Description, MLocID) %>%
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

saveWorkbook(wb, 'Rollups/Final rollup/MLoc_rollup_final.xlsx', overwrite = TRUE) 


## GNIS ------------------------------------------------------------------------------------------------------------



WS_GNIS_param_rollup <- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2022/Final List/GNIS_rollup_final.xlsx")


GNIS_Map_Display <- WS_GNIS_param_rollup %>%
  mutate(GNIS_final_category = factor(GNIS_final_category, levels=c("Unassessed", '3D',"3", "3B", "3C", "2", "5", '4B','4C', '4A' ), ordered=TRUE)) %>%
  mutate(Pollutant = case_when(!is.na(period) ~ paste0(Pollutant, "- ", period),
                               wqstd_code == 15 ~ paste0(Pollutant, "- Aquatic Life"),
                               wqstd_code == 16 ~ paste0(Pollutant, "- Human Health"),
                               TRUE ~ Pollutant)) %>%
  group_by(AU_ID,AU_Name,AU_Description, AU_GNIS_Name) %>%
  summarise(GNIS_status =  case_when(all(is.na(GNIS_final_category)) & max(AU_final_status) %in% c("4B","4C","4A", "5","4" ) ~ "Impaired",
                                     all(is.na(GNIS_final_category)) & max(AU_final_status) %in% c("3C","3D","3B", "3", "3C") ~ "Insufficient Data",
                                     all(is.na(GNIS_final_category)) & max(AU_final_status) %in% c("2","3D","3B") ~ "Attaining",
                                     max(GNIS_final_category, na.rm = TRUE) %in% c("4B","4C","4A", "5","4" ) ~ "Impaired",
                                     max(GNIS_final_category, na.rm = TRUE) %in% c("3C","3D","3B", "3", "3C") ~ "Insufficient Data",
                                     max(GNIS_final_category, na.rm = TRUE) %in% c("2","3D","3B") ~ "Attaining",
                                     TRUE ~ 'ERROR'),
            Category_2_Pollutants = ifelse(length(str_c(unique(Pollutant[GNIS_final_category ==  "2" ])[!is.na(unique(Pollutant[GNIS_final_category ==  "2" ]))], 
                                                        collapse  = "; ")) > 0, 
                                           str_c(unique(Pollutant[GNIS_final_category ==  "2"])[!is.na(unique(Pollutant[GNIS_final_category ==  "2"]))],
                                                 collapse  = "; "),
                                           "."),
            Category_5_Pollutants = ifelse(length(str_c(unique(Pollutant[GNIS_final_category ==  "5" ])[!is.na(unique(Pollutant[GNIS_final_category ==  "5" ]))], 
                                                        collapse  = "; ")) > 0, 
                                           str_c(unique(Pollutant[GNIS_final_category ==  "5"])[!is.na(unique(Pollutant[GNIS_final_category ==  "5"]))],
                                                 collapse  = "; "),
                                           ".") ,
            Category_4_Pollutants= ifelse(length(str_c(unique(Pollutant[GNIS_final_category ==  "4" ])[!is.na(unique(Pollutant[GNIS_final_category ==  "4" ]))], 
                                                       collapse  = "; ")) > 0, 
                                          str_c(unique(Pollutant[GNIS_final_category ==  "4"])[!is.na(unique(Pollutant[GNIS_final_category ==  "4"]))],
                                                collapse  = "; "),
                                          "."),
            Category_4A_Pollutants= ifelse(length(str_c(unique(Pollutant[GNIS_final_category ==  "4A" ])[!is.na(unique(Pollutant[GNIS_final_category ==  "4A" ]))], 
                                                        collapse  = "; ")) > 0, 
                                           str_c(unique(Pollutant[GNIS_final_category ==  "4A"])[!is.na(unique(Pollutant[GNIS_final_category ==  "4A"]))],
                                                 collapse  = "; "),
                                           "."),
            Category_4B_Pollutants= ifelse(length(str_c(unique(Pollutant[GNIS_final_category ==  "4B" ])[!is.na(unique(Pollutant[GNIS_final_category ==  "4B" ]))], 
                                                        collapse  = "; ")) > 0, 
                                           str_c(unique(Pollutant[GNIS_final_category ==  "4B"])[!is.na(unique(Pollutant[GNIS_final_category ==  "4B"]))],
                                                 collapse  = "; "),
                                           "."),
            Category_4C_Pollutants= ifelse(length(str_c(unique(Pollutant[GNIS_final_category ==  "4C" ])[!is.na(unique(Pollutant[GNIS_final_category ==  "4C" ]))], 
                                                        collapse  = "; ")) > 0, 
                                           str_c(unique(Pollutant[GNIS_final_category ==  "4C"])[!is.na(unique(Pollutant[GNIS_final_category ==  "4C"]))],
                                                 collapse  = "; "),
                                           "."),
            Category_3_Pollutants= ifelse(length(str_c(unique(Pollutant[GNIS_final_category ==  "3" ])[!is.na(unique(Pollutant[GNIS_final_category ==  "3" ]))], 
                                                       collapse  = "; ")) > 0, 
                                          str_c(unique(Pollutant[GNIS_final_category ==  "3"])[!is.na(unique(Pollutant[GNIS_final_category ==  "3"]))],
                                                collapse  = "; "),
                                          "."),
            
            Category_3B_Pollutants= ifelse(length(str_c(unique(Pollutant[GNIS_final_category ==  "3B" ])[!is.na(unique(Pollutant[GNIS_final_category ==  "3B" ]))], 
                                                        collapse  = "; ")) > 0, 
                                           str_c(unique(Pollutant[GNIS_final_category ==  "3B"])[!is.na(unique(Pollutant[GNIS_final_category ==  "3B"]))],
                                                 collapse  = "; "),
                                           "."),
            Category_3C_Pollutants= ifelse(length(str_c(unique(Pollutant[GNIS_final_category ==  "3C" ])[!is.na(unique(Pollutant[GNIS_final_category ==  "3C" ]))], 
                                                        collapse  = "; ")) > 0, 
                                           str_c(unique(Pollutant[GNIS_final_category ==  "3C"])[!is.na(unique(Pollutant[GNIS_final_category ==  "3C"]))],
                                                 collapse  = "; "),
                                           "."),
            Category_3D_Pollutants= ifelse(length(str_c(unique(Pollutant[GNIS_final_category ==  "3D" ])[!is.na(unique(Pollutant[GNIS_final_category ==  "3D" ]))], 
                                                        collapse  = "; ")) > 0, 
                                           str_c(unique(Pollutant[GNIS_final_category ==  "3D"])[!is.na(unique(Pollutant[GNIS_final_category ==  "3D"]))],
                                                 collapse  = "; "),
                                           ".")) 


wb <- createWorkbook()
addWorksheet(wb, sheetName = "WS_GNIS_param_rollup")
addWorksheet(wb, sheetName = "GNIS_Map_Display")


header_st <- createStyle(textDecoration = "Bold", border = "Bottom")
freezePane(wb, "WS_GNIS_param_rollup", firstRow = TRUE) 

freezePane(wb, "GNIS_Map_Display", firstRow = TRUE)

writeData(wb = wb, sheet = "WS_GNIS_param_rollup", x = WS_GNIS_param_rollup, headerStyle = header_st)

writeData(wb = wb, sheet = "GNIS_Map_Display", x = GNIS_Map_Display, headerStyle = header_st)

saveWorkbook(wb, 'GNIS_rollup_final.xlsx', overwrite = TRUE) 


## AU_ID -----------------------------------------------------------------------------------------------------------


AU_all <- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2022/Final List/AU_all_rollup.xlsx")

Delistings<- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2022/Final List/AU_all_rollup.xlsx",
                       sheet = "Delistings")

con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")

LU_BU_Assessment <- DBI::dbReadTable(con, 'LU_BU_Assessment') %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code))

load('Rollups/rollup helper/AU_to_ben_use.Rdata')




AU_to_ben_use <- AU_to_ben_use %>%
  select(AU_ID, AU_UseCode) %>%
  mutate(AU_UseCode = as.character(AU_UseCode))

# Renames the columns to more common names
names(AU_to_ben_use) <- c("AU_ID", "ben_use_code")

LU_benuses <- DBI::dbReadTable(con, 'LU_BenUseCode')



names(LU_benuses) <- c("ben_use_code", "ben_use_id", "ben_use")

LU_benuses$ben_use_code <- as.numeric(LU_benuses$ben_use_code)

# This is a long form table of all the benefical uses that apply to a given AU
all_ben_uses <- AU_to_ben_use %>%
  mutate(ben_use_code = as.numeric(ben_use_code)) %>%
  left_join(LU_benuses) %>%
  filter(!is.na(ben_use),
         ben_use != "NULL")

# all_ben_uses_join <- all_ben_uses %>%
#   filter(AU_ID %in% AU_all$AU_ID) %>%
#   mutate(BU_status = "Unassessed") %>%
#   select(AU_ID, ben_use_id, ben_use,BU_status )


AU_all <- AU_all %>%
  mutate(AU_parameter_category = factor(AU_parameter_category, 
                                        levels=c("Unassessed", '3D',"3", "3B", "3C", "2", '4B', '4C', '4','4A', "5" ), ordered=TRUE)) %>%
  distinct()

all_ben_uses2 <- all_ben_uses %>%
  mutate(keep = "keep") %>%
  select(-ben_use)


AU_BU <- AU_all %>%
  left_join(select(LU_BU_Assessment, -Assessment), by = c("Pollu_ID", 'wqstd_code') ) %>%
  right_join(all_ben_uses2) %>%
  select(-keep) %>%
  filter(!is.na(Pollutant))



BU_rollup <- AU_BU %>%
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
  group_by(AU_ID, ben_use) %>%
  summarise(AU_Name = max(AU_Name, na.rm = TRUE),
            AU_Description = max(AU_Description, na.rm = TRUE),
            Category = max(AU_parameter_category)) %>%
  right_join(filter(all_ben_uses, AU_ID %in% AU_all$AU_ID)) %>%
  mutate(Category = as.character(Category),
         Category = ifelse(is.na(Category), 'Unassessed', Category )) %>%
  select(-ben_use_id) %>%
  group_by(AU_ID) %>%
  mutate(AU_Name = max(AU_Name, na.rm = TRUE),
         AU_Description = max(AU_Description, na.rm = TRUE)) %>%
  #mutate(Category = ifelse(is.na(Category), "-", Category)) %>%
  spread(ben_use, Category, fill = "-") 


map_display <- AU_all %>%
  mutate(pollutant_strd = case_when(!is.na(period) ~ paste0(Pollutant, "- ", period),
                                    wqstd_code == 15 ~  paste0(Pollutant, "- Aquatic Life Toxics"),
                                    wqstd_code == 16 ~  paste0(Pollutant, "- Human Health Toxics"),
                                    TRUE ~ Pollutant
  )) %>%
  group_by(AU_ID) %>%
  summarise(AU_status = case_when(any(str_detect(AU_parameter_category, '5') | str_detect(AU_parameter_category, '4'))~ 'Impaired',
                                  any(str_detect(AU_parameter_category, '2')) ~ "Attaining",
                                  all(str_detect(AU_parameter_category, '3')) ~ "Insufficient Data",
                                  TRUE ~ "ERROR"
  ),
  year_last_assessed = max(year_assessed, na.rm = TRUE),
  Year_listed = ifelse(AU_status == 'Impaired', as.integer(min(Year_listed,  na.rm = TRUE)), NA_integer_ ) ,
  Cat_5_count = length(pollutant_strd[AU_parameter_category == '5']),
  Cat_4_count = length(pollutant_strd[str_detect(AU_parameter_category, '4')]),
  Impaired_count = Cat_5_count + Cat_4_count,
  Impaired_parameters = str_flatten(pollutant_strd[!is.na(AU_parameter_category) & (str_detect(AU_parameter_category, '5') | str_detect(AU_parameter_category, '4'))], ","),
  Cat_2_count = length(pollutant_strd[AU_parameter_category == '2']),
  Attaining_parameters = str_flatten(pollutant_strd[!is.na(AU_parameter_category) & AU_parameter_category == '2'], ","),
  Cat_3_count = length(pollutant_strd[AU_parameter_category == '3']),
  Cat_3B_count = length(pollutant_strd[AU_parameter_category == '3B']),
  Cat_3D_count = length(pollutant_strd[AU_parameter_category == '3D']),
  Cat_3_count_total = sum(Cat_3_count, Cat_3B_count, Cat_3D_count),
  Insufficient_parameters = str_flatten(pollutant_strd[!is.na(AU_parameter_category) & str_detect(AU_parameter_category, '3')], ",")
  )


wb <- createWorkbook()
addWorksheet(wb, sheetName = "AU_all")
addWorksheet(wb, sheetName = "AU_BU")
addWorksheet(wb, sheetName = "BU_rollup")
addWorksheet(wb, sheetName = "Delistings")
addWorksheet(wb, sheetName = "map_display")

header_st <- createStyle(textDecoration = "Bold", border = "Bottom")
freezePane(wb, "AU_all", firstRow = TRUE) 
freezePane(wb, "AU_BU", firstRow = TRUE)
freezePane(wb, "BU_rollup", firstRow = TRUE)
freezePane(wb, "Delistings", firstRow = TRUE)
freezePane(wb, "map_display", firstRow = TRUE)

writeData(wb = wb, sheet = "AU_all", x = AU_all, headerStyle = header_st)
writeData(wb = wb, sheet = "AU_BU", x = AU_BU, headerStyle = header_st)
writeData(wb = wb, sheet = "BU_rollup", x = BU_rollup, headerStyle = header_st)
writeData(wb = wb, sheet = "Delistings", x = Delistings, headerStyle = header_st)
writeData(wb = wb, sheet = "map_display", x = map_display, headerStyle = header_st)

saveWorkbook(wb, "AU_all_rollup.xlsx", overwrite = TRUE) 



