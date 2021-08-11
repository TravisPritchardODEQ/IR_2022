library(openxlsx)


fun_turb_analysis <- function(df, write_excel = TRUE){

#df <- Results_censored_turb


turb_data <- df %>%
  mutate(excursion = case_when(Result_cen >= Turb_Criteria ~ 1,
                               TRUE ~ 0)) 


turb_assessment <- turb_data %>%
  mutate(year = lubridate::year(SampleStartDate)) %>%
  group_by(AU_ID,  Pollu_ID, wqstd_code,  OWRD_Basin, year) %>%
  summarise(total_n = n(),
            total_excursions = sum(excursion),
            total_excursion_day = n_distinct(SampleStartDate[excursion == 1])) %>%
  ungroup() %>%
  group_by(AU_ID,  Pollu_ID, wqstd_code,  OWRD_Basin) %>%
  summarise(IR_category = case_when(max(total_excursion_day) > 45 ~ "5",
                                    max(total_excursion_day) <= 45 ~ "2"),
            Rationale = case_when(max(total_excursion_day) > 45 ~ str_c("Impaired: ", year[total_excursion_day > 45], ': ', total_excursion_day, " high turbidity days", collapse = "; "),
                                  max(total_excursion_day) <= 45 ~ "Attaining: All years of data show 45 or less high turbidity days per year."))%>%
  mutate(IR_category = factor(IR_category, levels=c("3D", "3", "3B", "2", "5" ), ordered=TRUE)) %>%
  mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code ))



if(write_excel){
  

wb <- createWorkbook()
addWorksheet(wb, sheetName = "Turb Data")
addWorksheet(wb, sheetName = "Turb AU categorization")


header_st <- createStyle(textDecoration = "Bold", border = "Bottom")
freezePane(wb, "Turb Data", firstRow = TRUE) 
freezePane(wb, "Turb AU categorization", firstRow = TRUE)

writeData(wb = wb, sheet = "Turb Data", x = turb_data, headerStyle = header_st)
writeData(wb = wb, sheet = "Turb AU categorization", x = turb_assessment, headerStyle = header_st)


print("Writing excel doc")
saveWorkbook(wb, "Parameters/Outputs/turbidity.xlsx", overwrite = TRUE) 

} 

turb <- list(data = turb_data,
             categorization = turb_assessment)

return(turb)

}