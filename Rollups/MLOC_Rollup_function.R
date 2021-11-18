
# MLOC Rollup -----------------------------------------------------------------------------------------------------

Mloc_Rollup_function <- function(df, periods = FALSE, DO = FALSE){
  
  
  #testing
  # df <- test
  # periods <-  FALSE
  # DO = FALSE
  # 
  
  if(!periods){
    
    
   
    
    stations_GNIS_rollup <- df %>%
      mutate(IR_category = factor(IR_category, levels=c('3D',"3", "3B", "3C", "2", "5" ), ordered=TRUE),
             AU_GNIS = str_c(AU_ID, AU_GNIS_Name, sep = ";")) %>%
      group_by(AU_ID, MLocID,AU_GNIS, Pollu_ID, wqstd_code) %>%
      summarise(MLocID_IR_category = max(IR_category),
                Rationale = paste(Rationale, collapse = " ~ ")) %>%
      mutate(MLocID_IR_category = case_when(is.na(MLocID_IR_category) ~ "Unassessed",
                                          TRUE ~as.character(MLocID_IR_category))) %>%
      mutate(MLocID_IR_category = factor(MLocID_IR_category, levels=c("Unassessed", '3D',"3", "3B", "3C", "2", "5", '4A' ), ordered=TRUE)) %>%
      separate(AU_GNIS, c(NA, "AU_GNIS_Name"),sep = ";") %>%
      #arrange(AU_ID, AU_GNIS_Name) %>%
      ungroup() %>%
     
      mutate(MLocID_IR_category = str_remove(MLocID_IR_category, 'Category ')) %>%
      mutate(MLocID_IR_category = factor(MLocID_IR_category, levels=c("Unassessed", '3D',"3", "3B","3C", "2", "5", '4A' ), ordered=TRUE)) 
     
    
    
  } else if(periods & !DO){
    
    stations_GNIS_rollup <- df %>%
      mutate(IR_category = factor(IR_category, levels=c('3D',"3", "3B", "3C", "2", "5" ), ordered=TRUE),
             AU_GNIS = str_c(AU_ID, AU_GNIS_Name, sep = ";")) %>%
      group_by(AU_ID, MLocID,AU_GNIS, Pollu_ID, wqstd_code, period) %>%
      summarise(MLocID_IR_category = max(IR_category),
                Rationale = paste(Rationale, collapse = " ~ ")) %>%
     mutate(MLocID_IR_category = case_when(is.na(MLocID_IR_category) ~ "Unassessed",
                                          TRUE ~as.character(MLocID_IR_category))) %>%
      mutate(MLocID_IR_category = factor(MLocID_IR_category, levels=c("Unassessed", '3D',"3", "3B","3C", "2", "5", '4A' ), ordered=TRUE)) %>%
      separate(AU_GNIS, c(NA, "AU_GNIS_Name"),sep = ";") %>%
      #arrange(AU_ID, AU_GNIS_Name) %>%
      ungroup() %>%
      
      mutate(MLocID_IR_category = str_remove(MLocID_IR_category, 'Category ')) %>%
      mutate(MLocID_IR_category = factor(MLocID_IR_category, levels=c("Unassessed", '3D',"3", "3B","3C", "2", "5", '4A' ), ordered=TRUE)) 
    
    
    
  } else if (DO){
    
 
    stations_GNIS_rollup <- df %>%
      mutate(IR_category = factor(IR_category, levels=c('3D',"3", "3B", "3C","2", "5" ), ordered=TRUE),
             AU_GNIS = str_c(AU_ID, AU_GNIS_Name, sep = ";")) %>%
      group_by(AU_ID, MLocID,AU_GNIS, Pollu_ID, wqstd_code, period) %>%
      summarise(MLocID_IR_category = max(IR_category),
                Rationale = paste(Rationale, collapse = " ~ ")) %>%
      mutate(MLocID_IR_category = case_when(is.na(MLocID_IR_category) ~ "Unassessed",
                                            TRUE ~as.character(MLocID_IR_category))) %>%
      mutate(MLocID_IR_category = factor(MLocID_IR_category, levels=c("Unassessed", '3D',"3", "3B","3C", "2", "5", '4A' ), ordered=TRUE)) %>%
      separate(AU_GNIS, c(NA, "AU_GNIS_Name"),sep = ";") %>%
      #arrange(AU_ID, AU_GNIS_Name) %>%
      ungroup() %>%
      
      mutate(MLocID_IR_category = str_remove(MLocID_IR_category, 'Category ')) %>%
      mutate(MLocID_IR_category = factor(MLocID_IR_category, levels=c("Unassessed", '3D',"3", "3B","3C", "2", "5", '4A' ), ordered=TRUE)) 
    
    
  }
  
  
  
  
  
  return(stations_GNIS_rollup)
  
}



