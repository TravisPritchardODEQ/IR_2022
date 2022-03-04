library(tidyverse)
library(openxlsx)




# Bring in data ---------------------------------------------------------------------------------------------------

AU_rollup <- read.xlsx("//deqHQ1/WQASSESSMENT/2022IRFiles/Draft List/Rollup outputs/Actual Final Final Final/AU_all_rollup.xlsx")



# 1)	What do we have for impairments for coastal waters? ----------------------------------------------------------

# CL units

coastal_impairments <- AU_rollup %>%
  filter(str_detect(AU_ID, "CL|OC")) %>%
  filter(str_detect(AU_final_status, "4|5")) %>%
  arrange(AU_ID) %>%
  select(AU_ID, AU_Name, AU_Description, Pollutant, Assessment, period,assessed_2022, year_assessed, AU_final_status, Rationale )

coastal_impairments <- AU_rollup %>%
  filter(str_detect(AU_ID, "CL|OC")) %>%
  filter(str_detect(AU_final_status, "3B")) %>%
  arrange(AU_ID) %>%
  select(AU_ID, AU_Name, AU_Description, Pollutant, Assessment, period,assessed_2022, year_assessed, AU_final_status, Rationale )


# 2)	What have we assessed and put in category 3b? ----------------------------------------------------------------


cat_3b <- AU_rollup %>%
  filter(str_detect(AU_final_status, "3B")) %>%
  arrange(AU_ID) %>%
  select(AU_ID, AU_Name, AU_Description, Pollutant, Assessment, period,assessed_2022, year_assessed, AU_final_status, Rationale )
  



# 3)	What impairments for estuaries  ---------------------------------------

estuary_impairments <-  AU_rollup %>%
  filter(str_detect(AU_ID, "EB")) %>%
  filter(str_detect(AU_final_status, "4|5")) %>%
  arrange(AU_ID) %>%
  select(AU_ID, AU_Name, AU_Description, Pollutant, Assessment, period,assessed_2022, year_assessed, AU_final_status, Rationale )




# Write xlsx doc --------------------------------------------------------------------------------------------------

)

wb <- createWorkbook()
addWorksheet(wb, sheetName = "coastal_impairments")
addWorksheet(wb, sheetName = "cat_3b")
addWorksheet(wb, sheetName = "estuary_impairments")


header_st <- createStyle(textDecoration = "Bold", border = "Bottom")
freezePane(wb, "coastal_impairments", firstRow = TRUE) 
freezePane(wb, "cat_3b", firstRow = TRUE)
freezePane(wb, "estuary_impairments", firstRow = TRUE)


writeData(wb = wb, sheet = "coastal_impairments", x = coastal_impairments, headerStyle = header_st)
writeData(wb = wb, sheet = "cat_3b", x = cat_3b, headerStyle = header_st)
writeData(wb = wb, sheet = "estuary_impairments", x = estuary_impairments, headerStyle = header_st)


saveWorkbook(wb, 'Stats/JW_questions_2-3-22.xlsx', overwrite = TRUE) 


