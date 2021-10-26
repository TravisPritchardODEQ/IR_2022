library(openxlsx)


fun_turb_analysis <- function(df, write_excel = TRUE){

#df <- Results_censored_turb


turb_data <- df %>%
  mutate(excursion = case_when(Result_cen >= Turb_Criteria ~ 1,
                               TRUE ~ 0)) 





turb_cat_fun <- function(df_data = turb_data, AU_type){
  
  
  if(AU_type == "other"){  
    group1 <- c('AU_ID', 'OWRD_Basin', 'Pollu_ID', 'wqstd_code', 'Char_Name', 'Turb_Criteria', 'year' )
    group2 <- c('AU_ID',  'Pollu_ID', 'wqstd_code', 'Char_Name',  'OWRD_Basin')
    inverse <- TRUE
    
    
  } else if (AU_type == "WS"){
    group1 <- c('AU_ID', 'MLocID', 'AU_GNIS_Name', 'OWRD_Basin', 'Pollu_ID', 'wqstd_code', 'Char_Name', 'Turb_Criteria', 'year' )
    group2 <- c('AU_ID','MLocID',  'AU_GNIS_Name', 'Pollu_ID', 'wqstd_code', 'Char_Name',  'OWRD_Basin')
    inverse <- FALSE
  }
  
  
  


turb_assessment <- df_data %>%
  mutate(year = lubridate::year(SampleStartDate)) %>%
  filter(str_detect(AU_ID, "WS", negate = inverse)) %>%
  group_by_at(group1) %>%
  #group_by(AU_ID,  Pollu_ID, wqstd_code,  OWRD_Basin, year) %>%
  summarise(total_n = n(),
            total_excursions = sum(excursion),
            total_excursion_day = n_distinct(SampleStartDate[excursion == 1])) %>%
  ungroup() %>%
  group_by_at(group2) %>%
  summarise(IR_category = case_when(max(total_excursion_day) > 45 ~ "5",
                                    max(total_excursion_day) <= 45 ~ "2"),
            Rationale = case_when(max(total_excursion_day) > 45 ~ paste0("Impaired: ",str_c( year[total_excursion_day > 45], ': ', total_excursion_day, " high turbidity days", collapse = "; ")),
                                  max(total_excursion_day) <= 45 ~ "Attaining: All years of data show 45 or less high turbidity days per year."))%>%
  mutate(IR_category = factor(IR_category, levels=c("3D", "3", "3B", "2", "5" ), ordered=TRUE))

turb_assessment <- join_prev_assessments(turb_assessment, AU_type = AU_type)


}




Turb_WS <- turb_cat_fun(df_data = turb_data, AU_type = "WS")
Turb_other <- turb_cat_fun(df_data = turb_data, AU_type = "other") %>%
  mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code))


WS_AU_rollup <- Turb_WS %>%
  select(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code,  OWRD_Basin, Char_Name, IR_category, Rationale) %>%
  ungroup() %>%
  group_by(AU_ID, Char_Name, Pollu_ID, wqstd_code,  OWRD_Basin) %>%
  summarise(IR_category_AU = max(IR_category),
            Rationale_AU = str_c(MLocID, ": ", Rationale, collapse =  " ~ " ) ) %>%
  mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code))


WS_AU_rollup <- join_prev_assessments(WS_AU_rollup, AU_type = "other")
if(write_excel){
  
 
wb <- createWorkbook()
addWorksheet(wb, sheetName = "Turb Data")
addWorksheet(wb, sheetName = "Turb WS categorization")
addWorksheet(wb, sheetName = "Turb WS AU rollup")
addWorksheet(wb, sheetName = "Turb other categorization")


header_st <- createStyle(textDecoration = "Bold", border = "Bottom")
freezePane(wb, "Turb Data", firstRow = TRUE) 
freezePane(wb, "Turb WS categorization", firstRow = TRUE)
freezePane(wb, "Turb WS AU rollup", firstRow = TRUE)
freezePane(wb, "Turb other categorization", firstRow = TRUE)

writeData(wb = wb, sheet = "Turb Data", x = turb_data, headerStyle = header_st)
writeData(wb = wb, sheet = "Turb WS categorization", x = Turb_WS, headerStyle = header_st)
writeData(wb = wb, sheet = "Turb WS AU rollup", x = WS_AU_rollup, headerStyle = header_st)
writeData(wb = wb, sheet = "Turb other categorization", x = Turb_other, headerStyle = header_st)

print("Writing excel doc")
saveWorkbook(wb, "Parameters/Outputs/turbidity.xlsx", overwrite = TRUE) 

} 

turb <- list(data = turb_data,
             Turb_WS = Turb_WS,
             WS_AU_rollup = WS_AU_rollup, 
             Turb_other = Turb_other)

return(turb)

}