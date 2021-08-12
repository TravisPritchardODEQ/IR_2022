library(lubridate)
library(openxlsx)
fun_Tox_HH_analysis <-function(df, write_excel = TRUE){ 
  

# Testing ---------------------------------------------------------------------------------------------------------

  
  # df <- Results_censored_tox_HH
  # write_excel = TRUE
  
  

# parameters withs summations --------------------------------------------------------------------------------------
  

  Chlordane_data <- df %>%
    filter(Pollu_ID == '27') %>%
    # Identofy chlordane
    mutate(is_chlordane = ifelse(chr_uid %in% c('767'), 1, 0 )) %>%
    group_by(OrganizationID, MLocID, SampleStartDate, act_depth_height) %>%
    # Check to see if group has chlordane
    mutate(has_chlordane = ifelse(max(is_chlordane) == 1, 1, 0 )) %>%
    # Get percentage of non detects
    ungroup() %>%
    filter((has_chlordane == 1 & is_chlordane == 1) | has_chlordane == 0) %>%
    group_by(OrganizationID, MLocID, SampleStartDate, act_depth_height) %>%
    mutate(summed_percent_nondetect = round(sum(Result_Operator == "<")/n()*100)) %>%
    mutate(summing_censored_value = ifelse(Result_Operator == "<", 0, Result_cen )) %>%
    mutate(summed_percent_nondetect = round(sum(Result_Operator == "<")/n()*100),
           # Do the summing
           Summed_values = sum(summing_censored_value),
           # Create note on what the summing is based on
           #IR_note = ifelse(has_chlordane == 1, "", paste("result is sum of ", str_c(Char_Name, collapse  = "; ")) ),
           #IR_note = as.character(IR_note)
           )  %>%
    # Keep only the first row. This preserves all the metadata
    filter(row_number() == 1) %>%
    # Change the Char_Name to Endosulfan and the Result_cen column to the summed value
    mutate(Char_Name = "Chlordane",
           Result_cen = Summed_values) %>%
    # get rid of extra columns that were created
    select(-Summed_values,  -summing_censored_value, -has_chlordane,is_chlordane)
  
  
  
  # PCBs --------------------------------------------------------------------
  
  # Sum PCB Data
  #PCB data is identifed by Pollu_ID
  
  # PCB_data <- df  %>%
  #   filter(Pollu_ID == '153') %>%
  #   #Identufy the aroclors
  #   mutate(is_aroclor = ifelse(chr_uid %in% c('575','578','580','582','583','586','587'), 1, 0 )) %>% #THese are the uid for the Arochlors
  #   # Group by org, mloc, date, and depth to identify sampling event
  #   group_by(OrganizationID, MLocID, SampleStartDate, act_depth_height) %>%
  #   # Flag of the grouping has an arochlor sample
  #   mutate(Has_aroclor = ifelse(any(is_aroclor == 1), 1, 0)) %>%
  #   # Undo the grouping
  #   ungroup() %>%
  #   # keep the type (aroclor or congener) that has the least amount of non-detects by percentage
  #   # Group by the same as above, but add in the is_arochlor flag
  #   group_by(OrganizationID, MLocID, SampleStartDate, act_depth_height, is_aroclor) %>%
  #   # Calculate the percent nondetect of each group
  #   mutate(PCB_summed_percent_nondetect = round(sum(Result_Operator == "<")/n()*100)) %>%
  #   #undo the grouping
  #   ungroup() %>%
  #   # redo the original single sample grouping 
  #   group_by(OrganizationID, MLocID, SampleStartDate, act_depth_height) %>%
  #   # remove individual congeners if the group has arochlor data & the aroclors have a lower percentage of nondetects
  #   filter((Has_aroclor == 1 & is_aroclor == 1 & PCB_summed_percent_nondetect == min(PCB_summed_percent_nondetect)) | Has_aroclor == 0) %>%
  #   # Recalculate the percent censored values
  #   mutate(summed_censored_value = ifelse(Result_Operator == "<", 0, Result_cen )) %>%
  #   mutate(PCB_summed_percent_nondetect = round(sum(Result_Operator == "<")/n()*100),
  #          # Do the summing
  #          Summed_values = sum(summed_censored_value),
  #          # Create note on what the summing is based on
  #          IR_note = ifelse(Has_aroclor ==  1, "PCB - Sum of Aroclors", 
  #                           ifelse(Has_aroclor ==  0, "PCB - Sum of congeners", "ERROR" )),
  #          Result_Operator = max(Result_Operator)
  #   ) %>%
  #   # Keep only the first row. This preserves all the metadata
  #   filter(row_number() == 1) %>%
  #   # Change the Char_Name to Endosulfan and the Result_cen column to the summed value
  #   mutate(Char_Name = "PCBs",
  #          Result_cen = Summed_values) %>%
  #   # get rid of extra columns that were created
  #   select(-Summed_values,  -Has_aroclor,  -is_aroclor, -summed_censored_value) %>%
  #   mutate(IR_note = as.character(IR_note))
  

# Arsenic ---------------------------------------------------------------------------------------------------------
  
  arsenic_data <- df %>%
    filter(Pollu_ID == '9') %>%
    # If have inorganic, keep it
    mutate(is_inorganic = ifelse(Char_Name == 'Arsenic, Inorganic', 1, 0 )) %>%
    group_by(OrganizationID, MLocID, SampleStartDate, act_depth_height) %>%
    mutate(has_inorganic = case_when(any(Char_Name == 'Arsenic, Inorganic') ~ 1,
                                     !any(Char_Name == 'Arsenic, Inorganic') ~ 0)) %>%
    ungroup() %>%
    filter((has_inorganic == 1 & is_inorganic == 1) | has_inorganic == 0) %>%
    mutate(Crit_Fraction = ifelse(is_inorganic == 0 , "Total", Crit_Fraction)) %>%
    select(-is_inorganic, -has_inorganic)
  
  
  # Put summed data together
  results_analysis <- df %>%
    filter(Pollu_ID != 153,
           Pollu_ID != 27,
           Pollu_ID != 9) %>%
    bind_rows(#PCB_data, 
      Chlordane_data,
      arsenic_data)
  
  

# Initial data ----------------------------------------------------------------------------------------------------

  
  tox_HH_data <- results_analysis %>%
    # Set null crit_fraction to "Total"
    mutate(Crit_Fraction = ifelse(is.na(Crit_Fraction), "Total", Crit_Fraction )) %>%
    # Create column for simplfied version of sample fraction
    # THese distinctions came from Sara Krepps
    mutate(Simplified_sample_fraction = ifelse(Sample_Fraction %in% c("Total", "Extractable",
                                                                      "Total Recoverable","Total Residual", 
                                                                      "None", "Volatile", "Semivolatile")  |
                                                 is.na(Sample_Fraction), 'Total', 
                                               ifelse(Sample_Fraction == "Dissolved"  |
                                                        Sample_Fraction == "Filtered, field"  |
                                                        Sample_Fraction == "Filtered, lab"  , "Dissolved", "Error"))) %>%
    group_by(OrganizationID, MLocID, Char_Name, SampleStartDate, act_depth_height) %>%
    # If group has Total fractionin it, mark with a 1. If ony dissolved, mark with 0
    mutate(Has_Crit_Fraction = ifelse(any(Simplified_sample_fraction == Crit_Fraction), 1, 0)) %>%
    # Filter out the results that do not macth criteira fraction, if the group has matching criteria. Also keep where whole group does not match
    ungroup() %>%
    filter((Has_Crit_Fraction == 1 & Simplified_sample_fraction == Crit_Fraction) | Has_Crit_Fraction == 0) %>% 
    # Remove results with null evaluation_criteria (indicating a mismatch between water type and criteria (ex freshwater phosporus samples ))
    filter(!is.na(crit)) %>%
    mutate(evaluation_result = ifelse(Char_Name == "Arsenic" & Sample_Fraction == "Total" & WaterTypeCode == 2, Result_cen*0.8, 
                                      ifelse(Char_Name == "Arsenic" & Sample_Fraction == "Total" & WaterTypeCode != 2, Result_cen*0.59, Result_cen ))) %>%
    mutate(excursion = ifelse(evaluation_result > crit, 1, 0 )) %>%
    mutate(is.3d = case_when(Result_Operator == "<" & IRResultNWQSunit > crit ~ 1,
                             TRUE ~ 0 )) %>%
    mutate(Crit_Fraction = ifelse(Char_Name == "Arsenic", "Inorganic", Char_Name ))
  
  

# assessment ------------------------------------------------------------------------------------------------------


# WS --------------------------------------------------------------------------------------------------------------

  HH_tox_assess_fun <- function(df_data = tox_HH_data, AU_type){
    

# Testing ---------------------------------------------------------------------------------------------------------
    # df_data = tox_HH_data
    # AU_type = 'WS'
    # 
    if(AU_type == "other"){  
      group1 <- c('AU_ID', 'GNIS_Name', 'OWRD_Basin', 'Pollu_ID', 'wqstd_code', 'Char_Name' , 
                  'Simplified_sample_fraction', 'Crit_Fraction')
      
      group2 <- c('AU_ID', 'Char_Name')
      inverse <- TRUE
      
      
    } else if (AU_type == "WS"){
      group1 <- c('AU_ID', 'MLocID', 'GNIS_Name', 'OWRD_Basin', 'Pollu_ID', 'wqstd_code', 'Char_Name' , 
                   'Simplified_sample_fraction', 'Crit_Fraction')
      
      group2 <- c('AU_ID', 'MLocID', 'Char_Name')
      inverse <- FALSE
    }
    
    
    tox_HH_assessment <- df_data %>%
      filter(str_detect(AU_ID, "WS", negate = inverse)) %>%
      group_by_at(group1) %>%
      summarise(OWRD_Basin = first(OWRD_Basin),
                Pollu_ID = first(Pollu_ID),
                crit = max(crit),
                num_samples = n(),
                summed_percent_nondetect = sum(Result_Operator == "<")/n(),
                num_3d = sum(is.3d),
                num_not_3d = num_samples - num_3d,
                percent_3d = num_3d/num_samples * 100,
                num_excursions = sum(excursion),
                geomean = case_when(percent_3d == 100 ~ NA_real_,
                                    num_not_3d >= 3 ~ round(geo_mean(Result_cen[!(is.3d)]), 7),
                                    TRUE ~ -NA_real_
                                    )) %>%
      ungroup() %>%
      group_by_at(group2) %>%
      mutate(num_fraction_types =  n(),
             IR_category =  case_when(percent_3d == 100 ~ "3D",
                                      num_samples >= 3 & geomean > crit ~ "5",
                                      num_samples < 3 & num_excursions >= 1 ~ "3B",
                                      num_samples < 3 & num_excursions == 0 ~ "3",
                                      num_not_3d < 3 ~ "3",
                                      geomean <= crit ~ "2",
                                      TRUE ~ "ERROR"), 
             Rationale =  case_when(percent_3d == 100 ~ paste0("All results are non-detects with detection limits above criteria- ", num_samples, " total samples"),
                                    num_samples >= 3 & geomean > crit ~ paste("Geometric mean of", round(geomean, 7), "above criteria of", crit,  " - ", num_samples, " total samples"),
                                    num_samples < 3 & num_excursions >= 1 ~ paste('Only', num_samples, " samples", 'and', num_excursions, "excursions"), 
                                    num_samples < 3 & num_excursions == 0 ~ paste('Only', num_samples, " samples", 'and', "0", "excursions"),
                                    num_not_3d < 3 ~ paste("Only", num_not_3d, 'samples have QL above criteria',   " - ", num_samples, " total samples"),
                                    geomean <= crit ~ paste0("Geometric mean ", geomean, " < criteria (", crit, ")",  " - ", num_samples, " total samples"),
                                    TRUE ~ "ERROR")) %>%
        arrange(AU_ID, Char_Name) %>%
      mutate(IR_category = factor(IR_category, levels=c("3D", "3", "3B", "2", "5" ), ordered=TRUE))
    
    
    return(tox_HH_assessment)
    
  }
  
  
  tox_HH_WS_assessments <- HH_tox_assess_fun(df_data = tox_HH_data, AU_type = 'WS')
  
  tox_HH_other_assessments <- HH_tox_assess_fun(df_data = tox_HH_data, AU_type = 'other')
  
 
  
  WS_AU_rollup <- tox_HH_WS_assessments %>%
    select(AU_ID, MLocID, GNIS_Name, Pollu_ID, wqstd_code, Char_Name, OWRD_Basin,  IR_category, Rationale) %>%
    group_by(AU_ID, Pollu_ID, wqstd_code, Char_Name, OWRD_Basin) %>%
    summarise(IR_category_AU = max(IR_category),
              Rationale_AU = str_c(MLocID, ": ", Rationale, collapse =  " ~ " ) ) %>%
    mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code))
  
  
  
  
  if(write_excel){
    
    
    wb <- createWorkbook()
    
    addWorksheet(wb, sheetName = "HH Toxt Data")

    addWorksheet(wb, sheetName = "HH Tox WS Station Cat")
    addWorksheet(wb, sheetName = "WS AU combined Cat")
    addWorksheet(wb, sheetName = "HH Tox Other AU Cat")

    
  
    
    
    header_st <- createStyle(textDecoration = "Bold", border = "Bottom")
    
    
    freezePane(wb, "HH Toxt Data", firstRow = TRUE) 
    freezePane(wb, "HH Tox WS Station Cat", firstRow = TRUE) 
    freezePane(wb, "HH Tox Other AU Cat", firstRow = TRUE) 
    freezePane(wb, "WS AU combined Cat", firstRow = TRUE) 
   
    
    
    writeData(wb, "HH Toxt Data", x = tox_HH_data, headerStyle = header_st) 
    writeData(wb, "HH Tox WS Station Cat", x = tox_HH_WS_assessments, headerStyle = header_st) 
    writeData(wb, "WS AU combined Cat", x = WS_AU_rollup, headerStyle = header_st) 
    writeData(wb, "HH Tox Other AU Cat", x= tox_HH_other_assessments,  headerStyle = header_st) 
    

    
    print("Writing excel doc")
    saveWorkbook(wb, "Parameters/Outputs/Tox_HH.xlsx", overwrite = TRUE) 
    
  }
  
  
  tox_HH_list <- list(HH_tox_data = tox_HH_data,
                      HH_tox_WS_staion_cat = tox_HH_WS_assessments,
                      HH_tox_WS_AU_combined_cat = WS_AU_rollup,
                      HH_tox_other_AU_cat = tox_HH_other_assessments
                      ) 
  
   return(tox_HH_list)
}
  