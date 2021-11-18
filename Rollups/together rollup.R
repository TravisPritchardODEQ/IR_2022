

# AU rollup -------------------------------------------------------------------------------------------------------

load('Rollups/rollup_AU_others.Rdata')
load('Rollups/WS_AU_GNIS_rollup.Rdata')



# Join ------------------------------------------------------------------------------------------------------------

rollup_AU_others <- rollup_AU_others %>%
  select(-AU_previous_IR_category, -`2022_IR_category`) %>%
  mutate(AU_final_status = as.character(AU_final_status))

WS_AU_rollup <- WS_AU_rollup %>%
  mutate(AU_final_status = as.character(AU_final_status))



# AU_parameter file -----------------------------------------------------------------------------------------------

AU_all_parameter <- bind_rows(WS_AU_rollup, rollup_AU_others)


# AU_Ben_Use ---------------------------------------------------------------------------------------------------------


con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")
LU_benuses <- DBI::dbReadTable(con, 'LU_BenUseCode')

load('Rollups/rollup helper/AU_to_ben_use.Rdata')

BUs <- read.csv("//deqhq1/WQASSESSMENT/2018IRFiles/2018_WQAssessment/Final List/Misc/LU Bus.csv",
                stringsAsFactors = FALSE) %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         WQstd_code = as.character(WQstd_code))

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









AU_all <- AU_all_parameter %>%
  mutate(AU_final_status = factor(AU_final_status, 
                                  levels=c("Unassessed", '3D',"3", "3B", "2", "5", '4B', '4A', '4C' ), ordered=TRUE)) 

AU_BU <- AU_all %>%
  left_join(select(BUs, -Pollutant), by = c("Pollu_ID", 'wqstd_code' = "WQstd_code") )

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
  summarise(Category = max(AU_final_status)) %>%
  right_join(filter(all_ben_uses, AU_ID %in% AU_all$AU_ID)) %>%
  mutate(Category = as.character(Category),
         Category = ifelse(is.na(Category), 'Unassessed', Category )) %>%
  select(-ben_use_id) %>%
  #mutate(Category = ifelse(is.na(Category), "-", Category)) %>%
  spread(ben_use, Category, fill = "-") 

delist_AU_other <- delist_AU_other %>%
  mutate(AU_final_status = as.character(AU_final_status))
delistings_WS_AU <- delistings_WS_AU %>%
  mutate(AU_final_status = as.character(AU_final_status))



AU_delistings <- bind_rows(delist_AU_other, delistings_WS_AU) %>%
  arrange(AU_ID, Pollutant)


wb <- createWorkbook()
addWorksheet(wb, sheetName = "AU_all")
addWorksheet(wb, sheetName = "AU_BU")
addWorksheet(wb, sheetName = "BU_rollup")
addWorksheet(wb, sheetName = "Delistings")

header_st <- createStyle(textDecoration = "Bold", border = "Bottom")
freezePane(wb, "AU_all", firstRow = TRUE) 
freezePane(wb, "AU_BU", firstRow = TRUE)
freezePane(wb, "BU_rollup", firstRow = TRUE)
freezePane(wb, "Delistings", firstRow = TRUE)

writeData(wb = wb, sheet = "AU_all", x = AU_all, headerStyle = header_st)
writeData(wb = wb, sheet = "AU_BU", x = AU_BU, headerStyle = header_st)
writeData(wb = wb, sheet = "BU_rollup", x = BU_rollup, headerStyle = header_st)
writeData(wb = wb, sheet = "Delistings", x = AU_delistings, headerStyle = header_st)

saveWorkbook(wb, 'Rollups/Rollup outputs/AU_all_rollup.xlsx', overwrite = TRUE) 





