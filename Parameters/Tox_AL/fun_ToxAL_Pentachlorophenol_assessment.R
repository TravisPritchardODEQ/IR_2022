library(odeqIRtools)
library(lubridate)


TOX_AL_penta_analysis <- function(df){
  
  # Assign violations
  penta_data_analysis <- df %>%
    mutate(evaluation_crit = ifelse(WaterTypeCode == 2, pmin(CMC_crit, CCC_crit, na.rm = TRUE), 7.9 )) %>%
    mutate(excursion = ifelse(Result_cen > evaluation_crit, 1, 0 ))
  
  
  #Summarize data and assign critical excursions and IR category
  penta_data_summary <- penta_data_analysis %>%
    group_by(AU_ID) %>%
    summarise(OWRD_Basin = first(OWRD_Basin),
              num_samples = n(),
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
                                  

  
  Tox_AL_penta_list <- list(data = penta_data_analysis,
                            penta_cats = penta_data_summary)
}