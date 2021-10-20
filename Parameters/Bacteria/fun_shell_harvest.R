

Shell_Harvest <- function(df, write_excel = TRUE) {
  

# Testing and setup -----------------------------------------------------------------------------------------------
#df <- Bacteria_results
  
  
  
  print("Begin shellfish harvesting analysis")
  ########################################
  # Test data set did not have any bact code 3 fecal data. Cnage code to 3 when run full analysis
  #########################################
  
  

  
  shell_harvest <- df %>%
    filter(Bacteria_code == 3,
           Char_Name == "Fecal Coliform") %>%
    rename(median_crit = Geomean_Crit) %>%
    select(-Perc_Crit, -lowest_crit) %>%
    mutate(SS_Crit = 43,
           median_crit = 14,
           SS_exceed = ifelse(Result_cen > SS_Crit, 1, 0))
  
  if(nrow(shell_harvest) == 0) {
    stop("No available data")
  }
  
  

# WS Unit ---------------------------------------------------------------------------------------------------------


  
  shell_harvest_analysis_WS <- shell_harvest %>%
    filter(str_detect(AU_ID, "WS", negate = FALSE)) %>%
    group_by(MLocID, AU_ID,AU_GNIS_Name, Pollu_ID, wqstd_code, OWRD_Basin, SS_Crit, median_crit ) %>%
    summarise(num_samples = n(),
              median = ifelse(num_samples >= 5, median(Result_cen), NA ),
              num_SS_exceed = sum(SS_exceed),
              percent_SS_exceed = round(num_SS_exceed/num_samples, 2)) %>%
    mutate(IR_category = case_when(!is.na(median) & median > median_crit ~ "5",
                                   num_samples >= 5 & percent_SS_exceed > 0.1 ~ "5" ,
                                   num_samples < 5 &  num_SS_exceed >= 1 ~  "3B",
                                   num_samples < 5 &  num_SS_exceed == 0 ~ "3",
                                   num_samples >= 5 & median <= median_crit & num_SS_exceed/num_samples <= 0.1 ~ "2",
                                   TRUE ~ "ERROR"),
           Rationale = case_when(!is.na(median) & median > median_crit ~ paste0(MLocID, ": Impaired:  Median fecal coliform concentration of ",
                                                                                median, "/100ml is greater than criteria value of ",
                                                                                median_crit, "- ", num_samples, " total samples" ),
                                   num_samples >= 5 & percent_SS_exceed > 0.1 ~  paste0(MLocID, ": Impaired: ", percent_SS_exceed, " of all samples exceed single sample criteria value of ",
                                                                                        SS_Crit, " - ", num_samples, " total samples") ,
                                   num_samples < 5 &  num_SS_exceed >= 1 ~  paste0(MLocID, ": Insufficient data: less than 5 samples, ",  num_SS_exceed, 
                                                                                   " exceedances- ",num_samples, " total samples"),
                                   num_samples < 5 &  num_SS_exceed == 0 ~ paste0(MLocID, ": Insufficient data: less than 5 samples, 0 exceedances. ",
                                                                                  num_samples, " total samples"),
                                   num_samples >= 5 & median <= median_crit & num_SS_exceed/num_samples <= 0.1 ~ paste0(MLocID, ": Attaining: median concentration of ",
                                                                                                                        median, " is less than ",
                                                                                                                        median_crit, ". ",percent_SS_exceed, 
                                                                                                                        "% exccedance rate of Single sample criteria is < 10%- ",
                                                                                                                        num_samples, " total samples"),
                                   TRUE ~ "ERROR")
           
           ) %>%
    mutate(IR_category = factor(IR_category, levels=c("3", "3B", "2", "5" ), ordered=TRUE))
  
  
  WS_AU_rollup <- shell_harvest_analysis_WS %>%
    ungroup() %>%
    group_by(AU_ID, Pollu_ID, wqstd_code,  OWRD_Basin) %>%
    summarise(IR_category_AU = max(IR_category),
              Rationale = str_c(Rationale,collapse =  " ~ " ) ) %>%
    mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code ))
  


# Other unit ------------------------------------------------------------------------------------------------------

  
  shell_harvest_analysis_other <- shell_harvest %>%
    filter(str_detect(AU_ID, "WS", negate = TRUE)) %>%
    group_by(AU_ID, Pollu_ID, wqstd_code, OWRD_Basin, SS_Crit, median_crit ) %>%
    summarise(num_samples = n(),
              median = ifelse(num_samples >= 5, median(Result_cen), NA ),
              num_SS_exceed = sum(SS_exceed),
              percent_SS_exceed = round(num_SS_exceed/num_samples, 2)) %>%
    mutate(IR_category = case_when(!is.na(median) & median > median_crit ~ "5",
                                   num_samples >= 5 & percent_SS_exceed > 0.1 ~ "5" ,
                                   num_samples < 5 &  num_SS_exceed >= 1 ~  "3B",
                                   num_samples < 5 &  num_SS_exceed == 0 ~ "3",
                                   num_samples >= 5 & median <= median_crit & num_SS_exceed/num_samples <= 0.1 ~ "2",
                                   TRUE ~ "ERROR"),
           Rationale = case_when(!is.na(median) & median > median_crit ~ paste0("Impaired: Median fecal coliform concentration of ",
                                                                                median, "/100ml is greater than criteria value of ",
                                                                                median_crit, "- ", num_samples, " total samples" ),
                                 num_samples >= 5 & percent_SS_exceed > 0.1 ~  paste0("Impaired: ", percent_SS_exceed, " of all samples exceed single sample criteria value of ",
                                                                                      SS_Crit, " - ", num_samples, " total samples") ,
                                 num_samples < 5 &  num_SS_exceed >= 1 ~  paste0("Insufficient data: less than 5 samples, ",  num_SS_exceed, 
                                                                                 " exceedances- ",num_samples, " total samples"),
                                 num_samples < 5 &  num_SS_exceed == 0 ~ paste0("Insufficient data: less than 5 samples, 0 exceedances. ",
                                                                                num_samples, " total samples"),
                                 num_samples >= 5 & median <= median_crit & num_SS_exceed/num_samples <= 0.1 ~ paste0("Attaining: median concentration of ",
                                                                                                                      median, " is less than ",
                                                                                                                      median_crit, ". ",percent_SS_exceed, 
                                                                                                                      "% exccedance rate of Single sample criteria is < 10%- ",
                                                                                                                      num_samples, " total samples"),
                                 TRUE ~ "ERROR")
           
    ) %>%
    mutate(IR_category = factor(IR_category, levels=c("3", "3B", "2", "5" ), ordered=TRUE))%>%
    mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code ))
  
  
  
  
  if(write_excel){
    
    wb <- createWorkbook()
    addWorksheet(wb, sheetName = "Shell Bacteria Data_WS")
    addWorksheet(wb, sheetName = "WS station categorization")
    addWorksheet(wb, sheetName = "WS AU categorization")
    addWorksheet(wb, sheetName = "Other AU categorization")

    
    header_st <- createStyle(textDecoration = "Bold", border = "Bottom")
    freezePane(wb, "Shell Bacteria Data_WS", firstRow = TRUE) 
    freezePane(wb, "WS station categorization", firstRow = TRUE) 
    freezePane(wb, "WS AU categorization", firstRow = TRUE) 
    freezePane(wb, "Other AU categorization", firstRow = TRUE) 
    
    
    
    writeData(wb = wb, sheet = "Shell Bacteria Data_WS", x = shell_harvest, headerStyle = header_st)
    writeData(wb = wb, sheet = "WS station categorization", x = shell_harvest_analysis_WS, headerStyle = header_st)
    writeData(wb = wb, sheet = "WS AU categorization", x = WS_AU_rollup, headerStyle = header_st)
    writeData(wb = wb, sheet = "Other AU categorization", x = shell_harvest_analysis_other, headerStyle = header_st )
  
    
    
    print("Writing excel doc")
    saveWorkbook(wb, "Parameters/Outputs/bacteria shell harvest.xlsx", overwrite = TRUE) 
    
  }
 
  
  
  shell_harvest <-list(shell_harvest_bacteria_data_ws=as.data.frame(shell_harvest),
                      ws_station_categorization=as.data.frame(shell_harvest_analysis_WS),
                      ws_au_categorization=as.data.frame(WS_AU_rollup),
                      other_au_categorization=as.data.frame(shell_harvest_analysis_other))
  
  return(shell_harvest)
  
  
   
}
  
  