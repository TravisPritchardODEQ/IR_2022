
pH_data <- cont_ph_raw("IR_Dev")


pH_cont <- pH_data[["pH_cont"]]%>%
  filter(str_detect(AU_ID, "101806|101810|102593|102575")) %>%
  mutate(pH_Max = 8.7)
pH_grab <- pH_data[["ph_grab"]]%>%
  filter(str_detect(AU_ID, "101806|101810|102593|102575"))%>%
  mutate(pH_Max = 8.7)




PH_assessments <- pH_assessment(pH_cont, pH_grab, write_xlsx = FALSE)


Other_categories  <- PH_assessments[["Other_categories"]]



wb <- createWorkbook()
addWorksheet(wb, sheetName = "pH_cont")
addWorksheet(wb, sheetName = "pH_grab")


header_st <- createStyle(textDecoration = "Bold", border = "Bottom")
freezePane(wb, "pH_cont", firstRow = TRUE) 
freezePane(wb, "pH_grab", firstRow = TRUE)


writeData(wb = wb, sheet = "pH_cont", x = pH_cont, headerStyle = header_st)
writeData(wb = wb, sheet = "pH_grab", x = pH_grab, headerStyle = header_st)



saveWorkbook(wb, 'Stats/aron_pH_data.xlsx', overwrite = TRUE) 