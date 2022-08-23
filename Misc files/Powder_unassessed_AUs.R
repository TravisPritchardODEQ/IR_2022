library(tidyverse)
library(openxlsx)




# Get AU information from database --------------------------------------------------------------------------------


database <- 'IR_Dev'

con <- DBI::dbConnect(odbc::odbc(), database)

AUs_db <- DBI::dbReadTable(con, 'AU')

AUs_powder <- AUs_db %>%
  filter(basin_name == "Powder")


# Get assessments -------------------------------------------------------------------------------------------------

assessments <- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2022/Final List/AU_all_rollup.xlsx")

assessments_bacteria <- assessments %>%
  filter(wqstd_code == 1)
  

# Joins -----------------------------------------------------------------------------------------------------------


powder_unassessed_total <- AUs_powder %>%
  anti_join(assessments, by = "AU_ID")

join_test <- assessments %>%
  filter(AU_ID %in% powder_unassessed_total$AU_ID)

powder_unassessed_bacteria <- AUs_powder %>%
  anti_join(assessments_bacteria, by = "AU_ID")

join_test2 <- assessments_bacteria %>%
  filter(AU_ID %in% powder_unassessed_bacteria$AU_ID)


# Write xlsx ------------------------------------------------------------------------------------------------------




wb <- createWorkbook()
addWorksheet(wb, sheetName = "Powder Unassessed Everything")
addWorksheet(wb, sheetName = "Powder Unassessed bacteria")

header_st <- createStyle(textDecoration = "Bold", border = "Bottom")

writeData(wb = wb, sheet = "Powder Unassessed Everything", x = powder_unassessed_total, headerStyle = header_st)
writeData(wb = wb, sheet = "Powder Unassessed bacteria", x = powder_unassessed_bacteria, headerStyle = header_st)

saveWorkbook(wb, 'Misc files/Powder_unassessed_AUs.xlsx', overwrite = TRUE) 
