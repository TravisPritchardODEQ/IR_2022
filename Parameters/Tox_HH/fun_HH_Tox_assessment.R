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

  PCB_data <- df  %>%
    filter(Pollu_ID == '153') %>%
    #Identufy the aroclors
    mutate(is_aroclor = ifelse(chr_uid %in% c('575','578','580','582','583','586','587'), 1, 0 )) %>% #THese are the uid for the Arochlors
    # Group by org, mloc, date, and depth to identify sampling event
    group_by(OrganizationID, MLocID, SampleStartDate, act_depth_height) %>%
    # Flag of the grouping has an arochlor sample
    mutate(Has_aroclor = ifelse(any(is_aroclor == 1), 1, 0)) %>%
    # Undo the grouping
    ungroup() %>%
    # keep the type (aroclor or congener) that has the least amount of non-detects by percentage
    # Group by the same as above, but add in the is_arochlor flag
    group_by(OrganizationID, MLocID, SampleStartDate, act_depth_height, is_aroclor) %>%
    # Calculate the percent nondetect of each group
    mutate(PCB_summed_percent_nondetect = round(sum(Result_Operator == "<")/n()*100)) %>%
    #undo the grouping
    ungroup() %>%
    # redo the original single sample grouping
    group_by(OrganizationID, MLocID, SampleStartDate, act_depth_height) %>%
    # remove individual congeners if the group has arochlor data & the aroclors have a lower percentage of nondetects
    filter((Has_aroclor == 1 & is_aroclor == 1 & PCB_summed_percent_nondetect == min(PCB_summed_percent_nondetect)) | Has_aroclor == 0) %>%
    # Recalculate the percent censored values
    mutate(summed_censored_value = ifelse(Result_Operator == "<", 0, Result_cen )) %>%
    mutate(PCB_summed_percent_nondetect = round(sum(Result_Operator == "<")/n()*100),
           # Do the summing
           Summed_values = sum(summed_censored_value),
           # Create note on what the summing is based on
           IR_note = ifelse(Has_aroclor ==  1, "PCB - Sum of Aroclors",
                            ifelse(Has_aroclor ==  0, "PCB - Sum of congeners", "ERROR" )),
           Result_Operator = max(Result_Operator)
    ) %>%
    # Keep only the first row. This preserves all the metadata
    filter(row_number() == 1) %>%
    # Change the Char_Name to Endosulfan and the Result_cen column to the summed value
    mutate(Char_Name = "PCBs",
           Result_cen = Summed_values) %>%
    # get rid of extra columns that were created
    select(-Summed_values,  -Has_aroclor,  -is_aroclor, -summed_censored_value) %>%
    mutate(IR_note = as.character(IR_note))


# Arsenic ---------------------------------------------------------------------------------------------------------
  
  #Arsenic criteria is for inorganic. If we have inorganic arsenic on a day, discard non inorganic char name. 
  
  arsenic_data <- df %>%
    filter(Pollu_ID == '9') %>%
    # If have inorganic, keep it
    mutate(is_inorganic = ifelse(Char_Name == 'Arsenic, Inorganic', 1, 0 )) %>%
    group_by(OrganizationID, MLocID, SampleStartDate, act_depth_height) %>%
    mutate(has_inorganic = case_when(any(Char_Name == 'Arsenic, Inorganic') ~ 1,
                                     !any(Char_Name == 'Arsenic, Inorganic') ~ 0)) %>%
    ungroup() %>%
    filter((has_inorganic == 1 & is_inorganic == 1) | has_inorganic == 0) %>%
    #If we don't have inorganic arsenic, then we use the total fraction and apply a conversion factor
      #If we don't have inorganic, temporarily set the criteria fraction to total so the assessment code works properly
      #we will set this back to 'inorganic' later
    mutate(Crit_Fraction = ifelse(is_inorganic == 0 , "Total", Crit_Fraction)) %>%
    select(-is_inorganic, -has_inorganic) 
  
  
  # Put summed data together
    #rmeove summed data and then join their datatables back
  results_analysis <- df %>%
    filter(Pollu_ID != 153,
           Pollu_ID != 27,
           Pollu_ID != 9) %>%
    bind_rows(PCB_data, 
      Chlordane_data,
      arsenic_data)
  
  

# Initial data ----------------------------------------------------------------------------------------------------

  
  tox_HH_data <- results_analysis %>%
    # Set null crit_fraction to "Total"
    mutate(Crit_Fraction = ifelse(is.na(Crit_Fraction), "Total", Crit_Fraction )) %>%
    # Create column for simplfied version of sample fraction
    # THese distinctions came from Sara Krepps
    mutate(Simplified_sample_fraction = case_when(Sample_Fraction %in% c("Total", "Extractable",
                                                                         "Total Recoverable","Total Residual", 
                                                                         "None", "Volatile", "Semivolatile")  | is.na(Sample_Fraction) ~ 'Total', 
                                                  Sample_Fraction == "Dissolved"  |
                                                    Sample_Fraction == "Filtered, field"  |
                                                    Sample_Fraction == "Filtered, lab"   ~ "Dissolved", 
                                                  TRUE ~ "Error")) %>%
    group_by(OrganizationID, MLocID, Char_Name, SampleStartDate, act_depth_height) %>%
    # If group has Total fraction in it, mark with a 1. If ony dissolved, mark with 0
    mutate(Has_Crit_Fraction = ifelse(any(Simplified_sample_fraction == Crit_Fraction), 1, 0)) %>%
    # Filter out the results that do not match criteria fraction, if the group has matching criteria. Also keep where whole group does not match
    ungroup() %>%
    filter((Has_Crit_Fraction == 1 & Simplified_sample_fraction == Crit_Fraction) | Has_Crit_Fraction == 0) %>% 
    # Remove results with null evaluation_criteria (indicating a mismatch between water type and criteria (ex freshwater phosporus samples ))
    filter(!is.na(crit)) %>%
    mutate(evaluation_result = ifelse(Char_Name == "Arsenic" & Simplified_sample_fraction == "Total" & WaterTypeCode == 2, Result_cen*0.8, 
                                      ifelse(Char_Name == "Arsenic" & Simplified_sample_fraction == "Total" & WaterTypeCode != 2, Result_cen*0.59, Result_cen ))) %>%
    mutate(excursion = ifelse(evaluation_result > crit, 1, 0 )) %>%
    mutate(is.3d = case_when(Result_Operator == "<" & IRResultNWQSunit > crit ~ 1,
                             TRUE ~ 0 )) %>%
    mutate(Crit_Fraction = ifelse(Char_Name == "Arsenic", "Inorganic", Crit_Fraction ))
  
  

# assessment ------------------------------------------------------------------------------------------------------


# WS --------------------------------------------------------------------------------------------------------------

  HH_tox_assess_fun <- function(df_data = tox_HH_data, AU_type){
    

# Testing ---------------------------------------------------------------------------------------------------------
    # df_data = tox_HH_data
    # AU_type = 'WS'
    # 
    if(AU_type == "other"){  
      group1 <- c('AU_ID',  'Pollu_ID', 'wqstd_code', 'Pollutant' , 'Crit_Fraction')
      
      group2 <- c('AU_ID', 'Pollutant')
      inverse <- TRUE
      
      
    } else if (AU_type == "WS"){
      group1 <- c('AU_ID', 'MLocID', 'AU_GNIS_Name', 'Pollu_ID', 'wqstd_code', 'Pollutant' , 'Crit_Fraction')
      
      group2 <- c('AU_ID', 'MLocID', 'Pollutant')
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
                                    num_not_3d >= 3 ~ geo_mean(evaluation_result[!(is.3d)]),
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
                                      geomean < crit ~ "2",
                                      TRUE ~ "ERROR"), 
             Rationale =  case_when(percent_3d == 100 ~ paste0("All results are non-detects with detection limits above criteria- ", num_samples, " total samples"),
                                    num_samples >= 3 & geomean > crit ~ paste("Geometric mean of", geomean, "above criteria of", crit,  " - ", num_samples, " total samples"),
                                    num_samples < 3 & num_excursions >= 1 ~ paste('Only', num_samples, " samples", 'and', num_excursions, "excursions"), 
                                    num_samples < 3 & num_excursions == 0 ~ paste('Only', num_samples, " samples", 'and', num_excursions, "excursions"),
                                    num_not_3d < 3 ~ paste("Only", num_not_3d, 'samples have QL above criteria',   " - ", num_samples, " total samples"),
                                    geomean < crit ~ paste0("Geometric mean ", geomean, " < criteria (", crit, ")",  " - ", num_samples, " total samples"),
                                    TRUE ~ "ERROR")) %>%
        arrange(AU_ID, Pollutant) %>%
      mutate(IR_category = factor(IR_category, levels=c("3D", "3", "3B", "2", "5" ), ordered=TRUE)) %>%
      rename(Char_Name = Pollutant)
    
    tox_HH_assessment <- join_prev_assessments(tox_HH_assessment, AU_type = AU_type)
    
    return(tox_HH_assessment)
    
  }
  
  
  tox_HH_WS_assessments <- HH_tox_assess_fun(df_data = tox_HH_data, AU_type = 'WS')
  
  tox_HH_other_assessments <- HH_tox_assess_fun(df_data = tox_HH_data, AU_type = 'other')
  
 
  
  WS_AU_rollup <- tox_HH_WS_assessments %>%
    select(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code, Char_Name,  IR_category, Rationale) %>%
    group_by(AU_ID, Pollu_ID, wqstd_code, Char_Name) %>%
    summarise(IR_category_AU = max(IR_category),
              Rationale_AU = str_c(MLocID, ": ", Rationale, collapse =  " ~ " ) ) %>%
    mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code))
  
  WS_AU_rollup <- join_prev_assessments(WS_AU_rollup, AU_type = "other")
  
  
  if(write_excel){
    
    
    wb <- createWorkbook()
    
    header_st <- createStyle(textDecoration = "Bold", border = "Bottom")

    addWorksheet(wb, sheetName = "HH Toxt Data")
    freezePane(wb, "HH Toxt Data", firstRow = TRUE) 
    
    cloneWorksheet(wb, "HH Tox WS Station Cat", clonedSheet =  "HH Toxt Data")
    cloneWorksheet(wb, "WS AU combined Cat", clonedSheet =  "HH Toxt Data")
    cloneWorksheet(wb, "HH Tox Other AU Cat", clonedSheet =  "HH Toxt Data")

    
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
  