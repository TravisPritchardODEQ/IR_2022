## L.Merrick 8/5/2021 
#function to assess continuous data using the ten-ten 
#modifued By Rravis Pritchard to fit the IR process



pH_assessment <- function(cont_data, grab_data, write_xlsx = TRUE){

  

# Exclude continuous data dates from grab data --------------------------------------------------------------------

  
  #Get list of continuous data to exclude from grab data
  cont_data_to_exclude <- pH_cont %>%
    rename(SampleStartDate  = Result_Date) %>%
    select(MLocID, SampleStartDate) %>%
    distinct() %>%
    mutate(has_cont_data = 1)
  
  #exclude continuous data from grab data
  pH_grab_no_cont <- pH_grab %>%
    left_join(cont_data_to_exclude, by = c("MLocID", "SampleStartDate")) %>%
    #Keep grab if no continuous data on that same day
    filter(is.na(has_cont_data) | has_cont_data != 1) %>%
    select(-has_cont_data)
  
  
  
  
  
  pH_assessment_fun <- function(df_cont_data = pH_cont, df_grab_data = pH_grab_no_cont,  AU_type){
    #testing
    #  df_data = pH_cont
    #  AU_type = "other"
    # # 
    
    
    if(AU_type == "other"){  
      group1 <- c('AU_ID', 'MLocID', 'AU_GNIS_Name','GNIS_Name', 'OWRD_Basin', 'Pollu_ID', 'wqstd_code', 'Char_Name', 'Result_Date')
      group2 <-  c('AU_ID',  'OWRD_Basin', 'Pollu_ID', 'wqstd_code', 'Char_Name')
      
      
      inverse <- TRUE
      
      
    } else if (AU_type == "WS"){
      group1 <- c('AU_ID', 'MLocID', 'AU_GNIS_Name', 'GNIS_Name', 'OWRD_Basin', 'Pollu_ID', 'wqstd_code', 'Char_Name', 'Result_Date' )
      group2 <- c('AU_ID', 'MLocID','AU_GNIS_Name', 'GNIS_Name', 'OWRD_Basin', 'Pollu_ID', 'wqstd_code', 'Char_Name')
      
      inverse <- FALSE
    }
    
    
  
  
  
  cont_pH_ten_ten_data <- df_cont_data %>%
    filter(str_detect(AU_ID, "WS", negate = inverse)) %>%
    mutate(pH_violation = ifelse(Result_Numeric < pH_Min | Result_Numeric > pH_Max, 1, 0 ),
           pH_violation_high = ifelse(Result_Numeric > pH_Max, 1, 0 ),
           pH_violation_low = ifelse(Result_Numeric < pH_Min, 1, 0 )) %>%
    group_by_at(group1) %>%
    summarise(daily_ten_num_Samples = n(),
              daily_ten_num_violation = sum(pH_violation),
              daily_ten_num_violation_high = sum(pH_violation_high),
              daily_ten_num_violation_low = sum(pH_violation_low),
              pH_low_crit = min(pH_Min),
              pH_high_crit = max(pH_Max),
              pH_code = first(pH_code)) %>%
    mutate(daily_ten_critical_excursions = binomial_excursions(daily_ten_num_Samples,type = "Conventionals"),
           daily_ten_excursion =  case_when( daily_ten_num_violation >= daily_ten_critical_excursions ~ 1,
                                             daily_ten_num_violation < daily_ten_critical_excursions ~ 0),
           daily_ten_excursion_high = case_when(daily_ten_num_violation_high >= daily_ten_critical_excursions ~ 1,
                                                daily_ten_num_violation_high < daily_ten_critical_excursions ~ 0),
           daily_ten_excursion_low = case_when(daily_ten_num_violation_low >= daily_ten_critical_excursions ~ 1,
                                               daily_ten_num_violation_low < daily_ten_critical_excursions ~ 0))
    
  

  grab_data <- df_grab_data %>%
    filter(str_detect(AU_ID, "WS", negate = inverse)) %>%
    rename(Result_Date = SampleStartDate) %>%
    mutate(pH_violation = ifelse(Result_Numeric < pH_Min | Result_Numeric > pH_Max, 1, 0 ),
           pH_violation_high = ifelse(Result_Numeric > pH_Max, 1, 0 ),
           pH_violation_low = ifelse(Result_Numeric < pH_Min, 1, 0 )) %>%
    group_by_at(group1) %>%
    summarise(grab_result = first(IRResultNWQSunit),
              grab_num_Samples = n(),
              grab_num_violation = sum(pH_violation),
              grab_num_violation_high = sum(pH_violation_high),
              grab_num_violation_low = sum(pH_violation_low),
              pH_low_crit = min(pH_Min),
              pH_high_crit = max(pH_Max),
              pH_code = first(pH_code)) 
  
  pH_data_together <- grab_data %>%
    bind_rows(cont_pH_ten_ten_data) %>%
    arrange(AU_ID, Result_Date)
  
    
  cont_pH_categories <- pH_data_together %>%
    group_by_at(group2) %>% 
    summarise(total_continuous_days = n_distinct(Result_Date[!is.na(daily_ten_num_Samples)]),
              sum_daily_ten_excursion = n_distinct(Result_Date[!is.na(daily_ten_num_Samples) & daily_ten_excursion == 1]),
              sum_daily_ten_excursion_high = n_distinct(Result_Date[!is.na(daily_ten_num_Samples) & daily_ten_excursion_high > 0]),
              sum_daily_ten_excursion_low = n_distinct(Result_Date[!is.na(daily_ten_num_Samples) & daily_ten_excursion_low > 0]),
              total_grab_days =  n_distinct(Result_Date[!is.na(grab_num_Samples)]),
              sum_grab_excursions = n_distinct(Result_Date[!is.na(grab_num_Samples) & grab_num_violation > 0]),
              sum_grab_excursions_high = n_distinct(Result_Date[!is.na(grab_num_Samples) & grab_num_violation_high > 0]),
              sum_grab_excursions_low =  n_distinct(Result_Date[!is.na(grab_num_Samples) & grab_num_violation_low > 0]),
              total_sample_days = total_continuous_days +total_grab_days,
              total_excursions = sum_daily_ten_excursion + sum_grab_excursions) %>%
    mutate(critical_excursions = binomial_excursions(total_continuous_days +total_grab_days, type =  "Conventionals"), 
           IR_category = case_when(total_grab_days == 0 & total_continuous_days >= 8 & sum_daily_ten_excursion >= critical_excursions ~ "5",
                                   total_grab_days == 0 & total_continuous_days < 8 & sum_daily_ten_excursion >= critical_excursions ~ "3B",
                                   total_grab_days == 0 & total_continuous_days < 8 & sum_daily_ten_excursion < critical_excursions ~ "3",
                                   total_grab_days == 0 & total_continuous_days >= 8 & sum_daily_ten_excursion < critical_excursions ~ "2",
                                   total_continuous_days == 0 & total_grab_days >=8 & sum_grab_excursions >= critical_excursions ~ "5",
                                   total_continuous_days == 0 & total_grab_days < 8 & sum_grab_excursions >= critical_excursions ~ "3B",
                                   total_continuous_days == 0 & total_grab_days < 8 & sum_grab_excursions < critical_excursions ~ "3",
                                   total_continuous_days == 0 & total_grab_days >= 8 & sum_grab_excursions < critical_excursions ~ "2",
                                   total_grab_days > 0 & total_continuous_days > 0 & (total_grab_days + total_continuous_days) >= 8 & (sum_daily_ten_excursion + sum_grab_excursions ) >= critical_excursions ~ "5",
                                   total_grab_days > 0 & total_continuous_days > 0 & (total_grab_days + total_continuous_days) < 8 & (sum_daily_ten_excursion + sum_grab_excursions ) >= critical_excursions ~ "3B",
                                   total_grab_days > 0 & total_continuous_days > 0 & (total_grab_days + total_continuous_days) < 8 & (sum_daily_ten_excursion + sum_grab_excursions ) < critical_excursions ~ "3",
                                   total_grab_days > 0 & total_continuous_days > 0 & (total_grab_days + total_continuous_days) >= 8 & (sum_daily_ten_excursion + sum_grab_excursions ) < critical_excursions ~ "2",
                                   TRUE ~ "ERROR"
                                     ),
           Rationale = case_when(total_grab_days == 0 & total_continuous_days >= 8 & sum_daily_ten_excursion >= critical_excursions ~ paste0("Impaired: ",sum_daily_ten_excursion, 
                                                                                                                                             " daily time series measurements fall outside range of criteria (",
                                                                                                                                             sum_daily_ten_excursion_high, " above criteria, ",
                                                                                                                                             sum_daily_ten_excursion_low, " below criteria) ",
                                                                                                                                             total_continuous_days, " total days of continuous data. No grab samples used"),
                                 total_grab_days == 0 & total_continuous_days < 8 & sum_daily_ten_excursion >= critical_excursions ~  paste0("Insufficient data: Less than 8 samples, ",sum_daily_ten_excursion, 
                                                                                                                                             " daily time series measurements fall outside range of criteria (",
                                                                                                                                             sum_daily_ten_excursion_high, " above criteria, ",
                                                                                                                                             sum_daily_ten_excursion_low, " below criteria) ",
                                                                                                                                             total_continuous_days, " total days of continuous data. No grab samples used"),
                                 total_grab_days == 0 & total_continuous_days < 8 & sum_daily_ten_excursion < critical_excursions ~paste0("Insufficient data: Less than 8 samples, ",sum_daily_ten_excursion, 
                                                                                                                                          " daily time series measurements fall outside range of criteria (",
                                                                                                                                          sum_daily_ten_excursion_high, " above criteria, ",
                                                                                                                                          sum_daily_ten_excursion_low, " below criteria) ",
                                                                                                                                          total_continuous_days, " total days of continuous data. No grab samples used"),
                                 total_grab_days == 0 & total_continuous_days >= 8 & sum_daily_ten_excursion < critical_excursions ~ paste0("Attaining: ",sum_daily_ten_excursion, 
                                                                                                                                            " daily time series measurements fall outside range of criteria (",
                                                                                                                                            sum_daily_ten_excursion_high, " above criteria, ",
                                                                                                                                            sum_daily_ten_excursion_low, " below criteria) ",
                                                                                                                                            total_continuous_days, " total days of continuous data. No grab samples used"),
                                 total_continuous_days == 0 & total_grab_days >=8 & sum_grab_excursions >= critical_excursions ~ paste0("Impaired: ", sum_grab_excursions, 
                                                                                                                                        " samples fall outside criteria range (",
                                                                                                                                        sum_grab_excursions_high, " above criteria, ",
                                                                                                                                        sum_grab_excursions_low, " below criteria). ",
                                                                                                                                        total_grab_days, " total samples. No continuous data used."),
                                 total_continuous_days == 0 & total_grab_days < 8 & sum_grab_excursions >= critical_excursions ~ paste0("Insufficient data: Less than 8 samples. ", sum_grab_excursions, 
                                                                                                                                        " samples fall outside criteria range (",
                                                                                                                                        sum_grab_excursions_high, " above criteria, ",
                                                                                                                                        sum_grab_excursions_low, " below criteria). ",
                                                                                                                                        total_grab_days, " total samples. No continuous data used."),
                                 total_continuous_days == 0 & total_grab_days < 8 & sum_grab_excursions < critical_excursions ~ paste0("Insufficient data: Less than 8 samples. ", sum_grab_excursions, 
                                                                                                                                       " samples fall outside criteria range (",
                                                                                                                                       sum_grab_excursions_high, " above criteria, ",
                                                                                                                                       sum_grab_excursions_low, " below criteria). ",
                                                                                                                                       total_grab_days, " total samples. No continuous data used."),
                                 total_continuous_days == 0 & total_grab_days >= 8 & sum_grab_excursions < critical_excursions ~  paste0("Attaining: ", sum_grab_excursions, 
                                                                                                                                         " samples fall outside criteria range (",
                                                                                                                                         sum_grab_excursions_high, " above criteria, ",
                                                                                                                                         sum_grab_excursions_low, " below criteria). ",
                                                                                                                                         total_grab_days, " total samples. No continuous data used."),
                                 total_grab_days > 0 & total_continuous_days > 0 & (total_grab_days + total_continuous_days) >= 8 & (sum_daily_ten_excursion + sum_grab_excursions ) >= critical_excursions ~ paste0("Impaired: ",
                                                                                                                                                                                                                     sum_daily_ten_excursion, 
                                                                                                                                                                                                                     " daily time series measurements fall outside range of criteria (",
                                                                                                                                                                                                                     sum_daily_ten_excursion_high, " above criteria, ",
                                                                                                                                                                                                                     sum_daily_ten_excursion_low, " below criteria) ",
                                                                                                                                                                                                                     total_continuous_days, " total days of continuous data. ",
                                                                                                                                                                                                                     sum_grab_excursions, 
                                                                                                                                                                                                                     "grab samples fall outside criteria range (",
                                                                                                                                                                                                                     sum_grab_excursions_high, " above criteria, ",
                                                                                                                                                                                                                     sum_grab_excursions_low, " below criteria). ",
                                                                                                                                                                                                                     total_grab_days, " total grab samples."),
                                 total_grab_days > 0 & total_continuous_days > 0 & (total_grab_days + total_continuous_days) < 8 & (sum_daily_ten_excursion + sum_grab_excursions ) >= critical_excursions ~ paste0("Insufficient data: Less than 8 days of continuous and grab samples combined. ",
                                                                                                                                                                                                                    sum_daily_ten_excursion, 
                                                                                                                                                                                                                    " daily time series measurements fall outside range of criteria (",
                                                                                                                                                                                                                    sum_daily_ten_excursion_high, " above criteria, ",
                                                                                                                                                                                                                    sum_daily_ten_excursion_low, " below criteria) ",
                                                                                                                                                                                                                    total_continuous_days, " total days of continuous data. ",
                                                                                                                                                                                                                    sum_grab_excursions, 
                                                                                                                                                                                                                    " grab samples fall outside criteria range (",
                                                                                                                                                                                                                    sum_grab_excursions_high, " above criteria, ",
                                                                                                                                                                                                                    sum_grab_excursions_low, " below criteria). ",
                                                                                                                                                                                                                    total_grab_days, " total grab samples."),
                                 total_grab_days > 0 & total_continuous_days > 0 & (total_grab_days + total_continuous_days) < 8 & (sum_daily_ten_excursion + sum_grab_excursions ) < critical_excursions ~  paste0("Insufficient data: Less than 8 days of continuous and grab samples combined. ",
                                                                                                                                                                                                                    sum_daily_ten_excursion, 
                                                                                                                                                                                                                    " daily time series measurements fall outside range of criteria (",
                                                                                                                                                                                                                    sum_daily_ten_excursion_high, " above criteria, ",
                                                                                                                                                                                                                    sum_daily_ten_excursion_low, " below criteria) ",
                                                                                                                                                                                                                    total_continuous_days, " total days of continuous data. ",
                                                                                                                                                                                                                    sum_grab_excursions, 
                                                                                                                                                                                                                    " grab samples fall outside criteria range (",
                                                                                                                                                                                                                    sum_grab_excursions_high, " above criteria, ",
                                                                                                                                                                                                                    sum_grab_excursions_low, " below criteria). ",
                                                                                                                                                                                                                    total_grab_days, " total grab samples."),
                                 total_grab_days > 0 & total_continuous_days > 0 & (total_grab_days + total_continuous_days) >= 8 & (sum_daily_ten_excursion + sum_grab_excursions ) < critical_excursions ~ paste0("Attaining: ",
                                                                                                                                                                                                                    sum_daily_ten_excursion, 
                                                                                                                                                                                                                    " daily time series measurements fall outside range of criteria (",
                                                                                                                                                                                                                    sum_daily_ten_excursion_high, " above criteria, ",
                                                                                                                                                                                                                    sum_daily_ten_excursion_low, " below criteria) ",
                                                                                                                                                                                                                    total_continuous_days, " total days of continuous data. ",
                                                                                                                                                                                                                    sum_grab_excursions, 
                                                                                                                                                                                                                    " grab samples fall outside criteria range (",
                                                                                                                                                                                                                    sum_grab_excursions_high, " above criteria, ",
                                                                                                                                                                                                                    sum_grab_excursions_low, " below criteria). ",
                                                                                                                                                                                                                    total_grab_days, " total grab samples."),
                                 TRUE ~ "ERROR")
                                     ) %>%
    mutate(IR_category = factor(IR_category, levels=c("3D", "3", "3B", "2", "5" ), ordered=TRUE))
  
  
  cont_pH_categories <- join_prev_assessments(cont_pH_categories, AU_type = AU_type)
  
  ph_assessment_list <- list(data = pH_data_together,
                             assessment = cont_pH_categories)
  
        
  }
  
  
  
  WS_Assessment <- pH_assessment_fun(df_cont_data = pH_cont, df_grab_data = pH_grab_no_cont,  AU_type = "WS")
  WS_data <-  WS_Assessment[["data"]]
  WS_categories <- WS_Assessment[["assessment"]]
  
  Other_Assessment <- pH_assessment_fun(df_cont_data = pH_cont, df_grab_data = pH_grab_no_cont,  AU_type = "other")
  Other_data <-  Other_Assessment[["data"]]
  Other_categories <- Other_Assessment[["assessment"]] %>%
    mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code))
  
  
  
  WS_AU_rollup <- WS_categories %>%
    select(AU_ID, MLocID, GNIS_Name, Pollu_ID, wqstd_code,  OWRD_Basin, Char_Name, IR_category, Rationale) %>%
    ungroup() %>%
    group_by(AU_ID, Char_Name, Pollu_ID, wqstd_code,  OWRD_Basin) %>%
    summarise(IR_category_AU = max(IR_category),
              Rationale_AU = str_c(MLocID, ": ", Rationale, collapse =  " ~ " ) ) %>%
    mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code))
  
  WS_AU_rollup <- join_prev_assessments(WS_AU_rollup, AU_type = "Other")
  
  pH_list <- list(WS_data = WS_data,
                  WS_categories = WS_categories,
                  WS_AU_rollup = WS_AU_rollup,
                  Other_data = Other_data, 
                  Other_categories = Other_categories)
  
  

# write xlsx ------------------------------------------------------------------------------------------------------

if(write_xlsx){
  
  
  
  wb <- createWorkbook()
  
  header_st <- createStyle(textDecoration = "Bold", border = "Bottom")
  
  addWorksheet(wb, sheetName = "pH WS Data")
  freezePane(wb, "pH WS Data", firstRow = TRUE)
  
  
  cloneWorksheet(wb, "pH WS station cat", clonedSheet =  "pH WS Data")
  cloneWorksheet(wb, "pH WS AU rollup", clonedSheet =  "pH WS Data")
  cloneWorksheet(wb, "pH other AU data", clonedSheet =  "pH WS Data")
  cloneWorksheet(wb, "pH other AU cat", clonedSheet =  "pH WS Data")
  
  
  writeData(wb = wb, sheet = "pH WS Data", x = WS_data, headerStyle = header_st)
  writeData(wb = wb, sheet = "pH WS station cat", x = WS_categories, headerStyle = header_st)
  writeData(wb = wb, sheet = "pH WS AU rollup", x = WS_AU_rollup, headerStyle = header_st)
  writeData(wb = wb, sheet = "pH other AU data", x = Other_data, headerStyle = header_st)
  writeData(wb = wb, sheet = "pH other AU cat", x = Other_categories, headerStyle = header_st)
  
  print("Writing excel doc")
  saveWorkbook(wb, "Parameters/Outputs/pH.xlsx", overwrite = TRUE) 
  
}
  
  
  
  return(pH_list)
  
  }

