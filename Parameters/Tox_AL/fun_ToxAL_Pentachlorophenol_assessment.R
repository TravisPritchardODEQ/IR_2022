library(odeqIRtools)
library(lubridate)


TOX_AL_penta_analysis <- function(df){
  

# testing ---------------------------------------------------------------------------------------------------------
#df <- tox_AL_penta_data
  
  
  # Assign violations
  penta_data_analysis <- df %>%
    mutate(evaluation_crit = ifelse(WaterTypeCode == 2, pmin(CMC_crit, CCC_crit, na.rm = TRUE), 7.9 )) %>%
    mutate(excursion = ifelse(Result_cen > evaluation_crit, 1, 0 ))
  
  fun_penta_analysis <- function(df_data = penta_data_analysis, AU_type){
    
    if(AU_type == "other"){  
      group1 <- c('AU_ID', 'AU_GNIS_Name', 'OWRD_Basin', 'Pollu_ID', 'wqstd_code', 'Char_Name' )
      
    
      inverse <- TRUE
      
      
    } else if (AU_type == "WS"){
      group1 <- c('AU_ID', 'MLocID', 'AU_GNIS_Name', 'OWRD_Basin', 'Pollu_ID', 'wqstd_code', 'Char_Name')
      
     
      inverse <- FALSE
    }
  
  #Summarize data and assign critical excursions and IR category
  penta_data_summary <- penta_data_analysis %>%
    filter(str_detect(AU_ID, "WS", negate = inverse)) %>%
    group_by_at(group1) %>%
    summarise(num_samples = n(),
              num_excursions = sum(excursion),
              percent_3d = round(sum(Result_Operator == "<" & IRResultNWQSunit > evaluation_crit )/num_samples * 100)) %>%
    mutate(critical_excursions = binomial_excursions(num_samples, type = 'Toxics'),
           IR_category = case_when(percent_3d == 100 ~ "3D",
                                   num_samples < 2 & num_excursions >= critical_excursions ~ "5", 
                                   num_samples < 10  & num_excursions == 0 ~ "3",
                                   num_samples < 10  & num_excursions > 0  ~ "3B",
                                   num_samples >=  10 & num_excursions < critical_excursions ~ "2",
                                   TRUE ~ "ERROR"),
           Rationale =  case_when(percent_3d == 100 ~ paste0("Insufficient data: ", "All results are non-detects with detection limits above criteria- ",
                                                                                  num_samples, " total samples"),
                                  num_samples < 2 & num_excursions >= critical_excursions ~  paste0("Impaired: ", num_excursions,
                                                                                                    " excursion of criteria with ",
                                                                                                    num_samples, " total samples. ", 
                                                                                                    critical_excursions, " needed to list."),
                                  num_samples < 10 & num_excursions == 0 ~ paste0("Insufficient data: ", "Only ",num_samples, " sample with ",
                                                                                  num_excursions," excursions"),
                                  num_samples < 10 & num_excursions > 0 ~ paste0("Insufficient data: ", "Only ",num_samples, " sample with ",
                                                                                  num_excursions," excursion"),
                                  
                                  num_samples >=  10 & num_excursions < critical_excursions ~ paste0("Attaining: ", num_excursions,
                                                                                  " excursion of criteria with ",
                                                                                  num_samples, " total samples. ", 
                                                                                  critical_excursions, " needed to list."),
                                  TRUE ~ "ERROR"))
  
  penta_data_summary <- join_prev_assessments(penta_data_summary, AU_type = AU_type)
                                  
}
  
  
  AL_tox_penta_WS <- fun_penta_analysis(df = penta_data_analysis, AU_type = "WS" )
  AL_tox_penta_other <- fun_penta_analysis(df = penta_data_analysis,AU_type = "other" ) %>%
    mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code))
  
  WS_AU_rollup <- AL_tox_penta_WS %>%
    select(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code,  OWRD_Basin, Char_Name, IR_category, Rationale) %>%
    ungroup() %>%
    group_by(AU_ID, Char_Name, Pollu_ID, wqstd_code,  OWRD_Basin) %>%
    summarise(IR_category_AU = max(IR_category),
              Rationale_AU = str_c(MLocID, ": ", Rationale, collapse =  " ~ " ) ) %>%
    mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code))
  WS_AU_rollup <- join_prev_assessments(WS_AU_rollup, AU_type = 'other')
  
  Tox_AL_penta_list <- list(data                            =  penta_data_analysis,
                            AL_tox_Penta_WS                =AL_tox_penta_WS,
                            AL_tox_Penta_other              = AL_tox_penta_other,
                            AL_tox_Penta_WS_rollup        = WS_AU_rollup)
  return(Tox_AL_penta_list)

}