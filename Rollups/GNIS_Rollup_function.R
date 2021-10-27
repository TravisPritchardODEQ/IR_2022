
GNIS_rollup <- function(df, periods = FALSE){
  

  #testing
  #df <- tox_AL_other_WS_station
  
  
  if(!periods){
    
    
    Pollu_IDs <- unique(df$Pollu_ID)
    wqstd_codes <- unique(df$wqstd_code)
    
    subset_prev_cat <- odeqIRtools::WS_GNIS_previous_listings %>%
      filter(Pollu_ID %in% Pollu_IDs,
             wqstd_code %in% wqstd_codes) %>%
      select(-Char_Name, -period)
    
    subset_prev_AU_cat <- odeqIRtools::AU_previous_categories %>%
      filter(Pollu_ID %in% Pollu_IDs,
             wqstd_code %in% wqstd_codes,
             grepl("WS", AU_ID)) %>%
      select(-Char_Name, -period) %>%
      mutate(AU_previous_IR_category = str_remove(AU_previous_IR_category, 'Category '))
    
    stations_GNIS_rollup <- df %>%
      mutate(IR_category = factor(IR_category, levels=c('3D',"3", "3B", "2", "5" ), ordered=TRUE),
             AU_GNIS = str_c(AU_ID, AU_GNIS_Name, sep = ";")) %>%
      group_by(AU_ID, AU_GNIS, Pollu_ID, wqstd_code) %>%
      summarise(stations =  paste(MLocID, collapse = "; "),
                GNIS_IR_category = max(IR_category)) %>%
      full_join(subset_prev_cat, by = c("AU_ID", "AU_GNIS", "Pollu_ID", "wqstd_code")) %>%
      full_join(subset_prev_AU_cat) %>%
      
      mutate(GNIS_IR_category = case_when(is.na(GNIS_IR_category) ~ "Unassessed",
                                          TRUE ~as.character(GNIS_IR_category)),
             GNIS_previous_IR_impairement = case_when(is.na(GNIS_previous_IR_impairement) ~ "Not previously listed",
                                                      TRUE ~ GNIS_previous_IR_impairement)) %>%
      mutate(GNIS_IR_category = factor(GNIS_IR_category, levels=c("Unassessed", '3D',"3", "3B", "2", "5", '4A' ), ordered=TRUE)) %>%
      separate(AU_GNIS, c(NA, "AU_GNIS_Name"),sep = ";") %>%
      #arrange(AU_ID, AU_GNIS_Name) %>%
      ungroup() %>%
      mutate(GNIS_final_IR_category = case_when(GNIS_IR_category == "Unassessed" ~ as.character(GNIS_previous_IR_impairement),
                                                GNIS_previous_IR_impairement == 'Not previously listed' ~ as.character(GNIS_IR_category),
                                                GNIS_IR_category == '2' ~ '2',
                                                GNIS_IR_category == '5' ~ '5',
                                                grepl('3', GNIS_IR_category) &GNIS_previous_IR_impairement == 'Not previously listed' ~ as.character(GNIS_IR_category),
                                                grepl('3', GNIS_IR_category) &GNIS_previous_IR_impairement != 'Not previously listed' ~ as.character(GNIS_previous_IR_impairement),
      )) %>%
      mutate(GNIS_final_IR_category = str_remove(GNIS_final_IR_category, 'Category ')) %>%
      mutate(GNIS_final_IR_category = factor(GNIS_final_IR_category, levels=c("Unassessed", '3D',"3", "3B", "2", "5", '4A' ), ordered=TRUE)) %>%
      group_by(AU_ID, Pollu_ID, wqstd_code) %>%
      mutate(AU_final_status = case_when(#GNIS_IR_category == "Unassessed" & GNIS_previous_IR_impairement == 'Not previously listed' ~ AU_previous_IR_category,
                                         all(GNIS_final_IR_category == '2') ~ "2",
                                         any(GNIS_IR_category == "Unassessed") ~ as.character(AU_previous_IR_category),
                                         any(GNIS_IR_category == '5') & !any(AU_previous_IR_category == '4A') ~ '5',
                                         any(GNIS_IR_category == '5') & any(AU_previous_IR_category == '4A') ~ '4A',
                                         any(GNIS_IR_category == '5') & is.na(AU_previous_IR_category) ~ '5',
                                         all(GNIS_IR_category == '2') ~ '2',
                                         all(GNIS_IR_category %in% c('2', '3', '3B', '3D')) ~ as.character(max(GNIS_IR_category)),
                                         TRUE ~ "Error"
                                         )) %>%
      ungroup() %>%
      mutate(GNIS_remove_impairment = case_when(GNIS_final_IR_category == 2 & GNIS_previous_IR_impairement %in% c("Category 5", "Category 4A") ~ "Yes",
                                                TRUE ~ "No"),
             AU_delist = case_when(AU_final_status == 2 & AU_previous_IR_category %in% c('5', '4A') ~ "Yes",
                            TRUE ~ "No"
      ))%>%
      ungroup() %>%
      arrange(AU_ID, AU_GNIS_Name) 
    
    return(stations_GNIS_rollup)
    
  } else if(periods){

    stop("Periods are not ready yet")
    
  }
  
}
