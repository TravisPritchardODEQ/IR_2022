


chl_assessment <- function(df, write_excel = TRUE){

 # df <- Results_censored_chla



  library(tidyverse)
  library(lubridate)
  library(zoo)
  library(runner)
  library(openxlsx)
  library(odeqIRtools)
# Watershed units -------------------------------------------------------------------------------------------------



chla_data_month_ws <- df %>%
  filter(str_detect(AU_ID, "WS", negate = FALSE)) %>%
  mutate(yearmon = lubridate::floor_date(lubridate::as_date(SampleStartDate), "month")) %>%
  group_by(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code,  OWRD_Basin,  yearmon, Chla_Criteria) %>%
  summarise(month_average = mean(Result_cen, na.rm = TRUE),
            month_n = n() ) %>%
  ungroup()

WS_3mo <- chla_data_month_ws %>%
  group_by(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code,  OWRD_Basin,  Chla_Criteria, month_n) %>%
  dplyr::mutate(d = runner(x = data.frame(yearmon  = yearmon,
                                          month_average = month_average,
                                          month = as.yearmon(yearmon, "%m/%Y")),
                           k = "3 months",
                           lag = 0,
                           idx = yearmon,
                           f = function(x) list(x))) %>%
  dplyr::mutate(d = purrr::map(d, ~ .x %>%
                                 dplyr::summarise(avg_3mo = case_when(n_distinct(yearmon) == 3 ~ mean(month_average),
                                                                      TRUE ~ NA_real_),
                                                  months_3mo = paste(min(month), "-", max(month)) #str_c(month, collapse = ", ")
                                 )
  )) %>%
  tidyr::unnest_wider(d)


WS_category <- WS_3mo %>%
  mutate(month = as.yearmon(yearmon, "%m/%Y"),
         avg_3mo_excursion = case_when(is.na(avg_3mo) ~ 0,
                                      avg_3mo > Chla_Criteria ~ 1,
                                      TRUE ~ 0 )) %>%
  group_by(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code,  OWRD_Basin) %>%
  summarise(total_n = sum(month_n),
            num_3mo_avg_calculated = sum(!is.na(avg_3mo)),
            num_excur_3mo_avg = sum(avg_3mo_excursion),
            excur_3mo_avg_months = ifelse(max(avg_3mo_excursion, na.rm = TRUE) == 1,str_c(unique(months_3mo[avg_3mo_excursion == 1]), collapse = "; "),NA),
            num_monthly_excur = sum(month_average > Chla_Criteria),
            critical_excursions_month = binomial_excursions(n(), "Conventionals"),
            monthly_excur_month = str_c(unique(month[month_average > Chla_Criteria]), collapse = "; ")
            ) %>%
  ungroup() %>%
  mutate(IR_category = case_when(num_excur_3mo_avg >= 1 ~ "5",
                                 num_monthly_excur > critical_excursions_month  ~ "5",
                                 total_n >= 8 & num_monthly_excur < critical_excursions_month  ~ "2",
                                 num_3mo_avg_calculated == 0 & num_monthly_excur > 0 ~ "3B",
                                 num_3mo_avg_calculated == 0 & num_monthly_excur == 0 ~ "3",
                                 num_3mo_avg_calculated > 0 & num_excur_3mo_avg == 0 ~ "2",
                                 TRUE ~ 'ERROR'
                                 ),
         Rationale =  case_when(num_excur_3mo_avg >= 1 ~ paste0(MLocID, ": ", "Impaired: ", num_excur_3mo_avg, " 3-month averages exceed critertia: ", 
                                                                excur_3mo_avg_months, ": ", 
                                                                total_n, " total samples"),
                                num_monthly_excur > critical_excursions_month ~ paste0(MLocID, ": ","Impaired: ", 
                                                                                       num_monthly_excur, " month averages exceed criteria: ",
                                                                                       monthly_excur_month, ": ", 
                                                                                       total_n, " total samples") ,
                                total_n >= 8 & num_monthly_excur < critical_excursions_month  ~  paste0("Attaining: ",  
                                                                                                        "< 10% (binomial) monthly ",
                                                                                                        "averages exceed criteria: ", 
                                                                                                        num_monthly_excur, 
                                                                                                        " excursions of ", total_n, 
                                                                                                        " total samples"),
                                num_3mo_avg_calculated == 0 & num_monthly_excur > 0 ~  paste0(MLocID, "- Insuffcient data: ", 
                                                                                              num_monthly_excur, " month averages exceed criteria: ",
                                                                                              monthly_excur_month, ". No 3-month averages calculated: ", 
                                                                                              total_n, " total samples"),
                                num_3mo_avg_calculated == 0 & num_monthly_excur == 0 ~ paste0(MLocID, "- Insuffcient data: ", 
                                                                                              "0 month averages exceed criteria. No 3-month averages calculated: ", 
                                                                                              total_n, " total samples"),
                                num_3mo_avg_calculated > 0 & num_excur_3mo_avg == 0 ~ paste0(MLocID, "- Attaining: ", 
                                                                                             "0 3-month averages exceed criteria. < 10% (binomial) monthly ",
                                                                                             "averages exceed criteria: ", 
                                                                                             total_n, " total samples"),
                                TRUE ~ 'ERROR'
         )) %>%
  mutate(IR_category = factor(IR_category, levels=c("3", "3B", "2", "5" ), ordered=TRUE))

WS_category <- odeqIRtools::join_prev_assessments(WS_category, AU_type = 'WS')


# WS AU rollup ----------------------------------------------------------------------------------------------------

WS_AU_rollup <- WS_category %>%
  ungroup() %>%
  group_by(AU_ID, Pollu_ID, wqstd_code,  OWRD_Basin) %>%
  summarise(IR_category_AU = max(IR_category),
            Rationale_AU = str_c(Rationale,collapse =  " ~ " )) %>%
  mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code ))


WS_AU_rollup <- odeqIRtools::join_prev_assessments(WS_AU_rollup, AU_type = 'Other')

# Other AUs -------------------------------------------------------------------------------------------------------




chla_data_month_other <- df %>%
  filter(str_detect(AU_ID, "WS", negate = TRUE)) %>%
  mutate(yearmon = lubridate::floor_date(lubridate::as_date(SampleStartDate), "month")) %>%
  group_by(AU_ID, Pollu_ID, wqstd_code,  OWRD_Basin,  yearmon, Chla_Criteria) %>%
  summarise(month_average = mean(Result_cen, na.rm = TRUE),
            month_n = n() ) %>%
  ungroup()

other_3mo <- chla_data_month_other %>%
  group_by(AU_ID, Pollu_ID, wqstd_code,  OWRD_Basin,  Chla_Criteria, month_n) %>%
  dplyr::mutate(d = runner(x = data.frame(yearmon  = yearmon,
                                          month_average = month_average,
                                          month = as.yearmon(yearmon, "%m/%Y")),
                           k = "3 months",
                           lag = 0,
                           idx = yearmon,
                           f = function(x) list(x))) %>%
  dplyr::mutate(d = purrr::map(d, ~ .x %>%
                                 dplyr::summarise(avg_3mo = case_when(n_distinct(yearmon) == 3 ~ mean(month_average),
                                                                      TRUE ~ NA_real_),
                                                  months_3mo = paste(min(month), "-", max(month)) #str_c(month, collapse = ", ")
                                 )
  )) %>%
  tidyr::unnest_wider(d)


other_category <- other_3mo %>%
  mutate(month = as.yearmon(yearmon, "%m/%Y"),
         avg_3mo_excursion = case_when(is.na(avg_3mo) ~ 0,
                                       avg_3mo > Chla_Criteria ~ 1,
                                       TRUE ~ 0 )) %>%
  group_by(AU_ID,  Pollu_ID, wqstd_code,  OWRD_Basin) %>%
  summarise(total_n = sum(month_n),
            num_3mo_avg_calculated = sum(!is.na(avg_3mo)),
            num_excur_3mo_avg = sum(avg_3mo_excursion),
            excur_3mo_avg_months = ifelse(max(avg_3mo_excursion, na.rm = TRUE) == 1,str_c(unique(months_3mo[avg_3mo_excursion == 1]), collapse = "; "),NA),
            num_monthly_excur = sum(month_average > Chla_Criteria),
            critical_excursions_month = binomial_excursions(n(), "Conventionals"),
            monthly_excur_month = str_c(unique(month[month_average > Chla_Criteria]), collapse = "; ")
  ) %>%
  ungroup() %>%
  mutate(IR_category = case_when(num_excur_3mo_avg >= 1 ~ "5",
                                 num_monthly_excur > critical_excursions_month  ~ "5",
                                 total_n >= 8 & num_monthly_excur < critical_excursions_month  ~ "2",
                                 num_3mo_avg_calculated == 0 & num_monthly_excur > 0 ~ "3B",
                                 num_3mo_avg_calculated == 0 & num_monthly_excur == 0 ~ "3",
                                 num_3mo_avg_calculated > 0 & num_excur_3mo_avg == 0 ~ "2",
                                 TRUE ~ 'ERROR'
  ),
  Rationale =  case_when(num_excur_3mo_avg >= 1 ~ paste0("Impaired: ", num_excur_3mo_avg, " 3-month averages exceed critertia: ", 
                                                         excur_3mo_avg_months, ": ", 
                                                         total_n, " total samples"),
                         num_monthly_excur > critical_excursions_month ~ paste0("Impaired: ", num_monthly_excur, " month averages exceed criteria: ",
                                                                                monthly_excur_month, ": ", 
                                                                                total_n, " total samples") ,
                         total_n >= 8 & num_monthly_excur < critical_excursions_month  ~  paste0("Attaining: ",  
                                                                                                 "< 10% (binomial) monthly ",
                                                                                                 "averages exceed criteria: ", 
                                                                                                 num_monthly_excur, 
                                                                                                 " excursions of ", total_n, 
                                                                                                 " total samples"),
                         num_3mo_avg_calculated == 0 & num_monthly_excur > 0 ~  paste0("Insuffcient data: ", 
                                                                                       num_monthly_excur, " month averages exceed criteria: ",
                                                                                       monthly_excur_month, ". No 3-month averages calculated: ", 
                                                                                       total_n, " total samples"),
                         num_3mo_avg_calculated == 0 & num_monthly_excur == 0 ~ paste0("Insuffcient data: ", 
                                                                                       "0 month averages exceed criteria. No 3-month averages calculated: ", 
                                                                                       total_n, " total samples"),
                         num_3mo_avg_calculated > 0 & num_excur_3mo_avg == 0 ~ paste0("Attaining: ", 
                                                                                      "0 3-month averages exceed criteria. < 10% (binomial) monthly ",
                                                                                      "averages exceed criteria: ", 
                                                                                      total_n, " total samples"),
                         TRUE ~ 'ERROR'
  )) %>%
  mutate(IR_category = factor(IR_category, levels=c("3", "3B", "2", "5" ), ordered=TRUE)) %>%
  mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code ))

other_category <- join_prev_assessments(other_category, AU_type = 'Other')

# Write excel docs ------------------------------------------------------------------------------------------------

if(write_excel){
  
  chla_data_month_ws <- chla_data_month_ws %>%
    mutate(yearmon = as.yearmon(yearmon, "%m/%Y"))
  
  
  chla_data_month_other <- chla_data_month_other %>%
    mutate(yearmon = as.yearmon(yearmon, "%m/%Y"))
  
  
  wb <- createWorkbook()
  addWorksheet(wb, sheetName = "Chl-a Raw Data")
  addWorksheet(wb, sheetName = "Chl-a WS Data")
  addWorksheet(wb, sheetName = "Chl-a other Data")
  addWorksheet(wb, sheetName = "WS station categorization")
  addWorksheet(wb, sheetName = "WS AU categorization")
  addWorksheet(wb, sheetName = "Other AU categorization")
  
  header_st <- createStyle(textDecoration = "Bold", border = "Bottom")
  
  writeData(wb = wb, sheet = "Chl-a Raw Data", x = df, headerStyle = header_st)
  writeData(wb = wb, sheet = "Chl-a WS Data", x = chla_data_month_ws, headerStyle = header_st)
  writeData(wb = wb, sheet = "Chl-a other Data", x = chla_data_month_other, headerStyle = header_st)
  writeData(wb = wb, sheet = "WS station categorization", x = WS_category, headerStyle = header_st)
  writeData(wb = wb, sheet = "WS AU categorization", x = WS_AU_rollup, headerStyle = header_st)
  writeData(wb = wb, sheet = "Other AU categorization", x = other_category, headerStyle = header_st )
  
  
  print("Writing excel doc")
  saveWorkbook(wb, "Parameters/Outputs/chl-a.xlsx", overwrite = TRUE) 
  
}
chl_assessment <- list(chl_raw = as.data.frame(df),
                       chl_WS_data = as.data.frame(chla_data_month_ws),
                       chl_other_data = as.data.frame(chla_data_month_other),
                       chl_WS_station_cat = as.data.frame(WS_category),
                       chl_WS_AU_cat = as.data.frame(WS_AU_rollup),
                       chl_other_AU_cat = as.data.frame(other_category)) 

return(chl_assessment)

}
  