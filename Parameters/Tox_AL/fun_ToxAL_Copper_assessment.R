require(tidyverse)
require(odeqIRtools) 
library(odbc)
library(DBI)
library(glue)
library(dbplyr)


copper_assessment <- function(CU_file= 'Parameters/Tox_AL/Copper_criteria_results.csv', database = "IR_Dev"){
  

# Somewhere in the export/import some MLociD's got truncated. Get a list to fix that ------------------------------

  
  
  
  #open connection to database
  con <- DBI::dbConnect(odbc::odbc(), database)
  
  CU_stations <- tbl(con, 'VW_Copper') %>%
    select(MLocID, Result_UID) %>%
    collect() %>%
    rename(MLocID_cor = MLocID)
  
  save(CU_stations, file = 'CU_stations.Rdata')
  
  
Cu_BLM <- read.csv(CU_file)

# get criteria values - FW crit = lowest 

Cu_BLM_crit <- Cu_BLM %>%
  left_join(CU_stations,  by = "Result_UID") %>%
  mutate(MLocID = MLocID_cor) %>%
  select(-MLocID_cor) %>%
  #lowest SW criteria 
  mutate(CCC_SW = 3.1) %>%
  mutate(crit = case_when(WaterTypeCode == 2 ~  pmin(CCC_ug.L,CMC_ug.L), 
                          WaterTypeCode == 3 ~ CCC_SW,
                          WaterTypeCode == 1 ~ pmin(CCC_ug.L,CMC_ug.L,CCC_SW),
                          TRUE ~ pmin(CCC_ug.L,CMC_ug.L))) %>%
  mutate(crit = as.numeric(crit)) %>%
  mutate(Crit_fraction = "Dissolved")

print('Begin analysis')

# Use the conversion factor to transform total results to dissolved results  
Results_censored <- censor_data(Cu_BLM_crit) %>%
  mutate(Result_cen = as.numeric(Result_cen)) %>%
  mutate(Simplfied_Sample_Fraction = ifelse(Sample_Fraction ==  "Dissolved",  "Dissolved", "Total" )) %>%
  group_by(MLocID, SampleStartDate, Char_Name,Result_Depth) %>%
  mutate(Has_Crit_Fraction = ifelse(Crit_fraction == "Total" & max(Simplfied_Sample_Fraction) == "Total", 1, 
                                    ifelse(Crit_fraction == "Dissolved" & min(Simplfied_Sample_Fraction) == "Dissolved", 1, 0 ))) %>%
  # Filter out the results that do not macth criteira fraction, if the group has matching criteria. Also keep where whole group does not match
  ungroup() %>%
  filter((Has_Crit_Fraction == 1 & Simplfied_Sample_Fraction == Crit_fraction) | Has_Crit_Fraction == 0) %>%
  mutate(excursion = ifelse(Result_cen > crit , 1, 0 ))


# Assessment function ---------------------------------------------------------------------------------------------



AL_tox_CU_assess_fun <- function(df_data = Results_censored, AU_type){
  
  # 
  if(AU_type == "other"){  
    group1 <- c('AU_ID', 'Pollu_ID', 'wqstd_code', 'Char_Name')
    
    
    inverse <- TRUE
    
    
  } else if (AU_type == "WS"){
    group1 <- c('AU_ID', 'MLocID', 'AU_GNIS_Name',  'Pollu_ID', 'wqstd_code', 'Char_Name' )
    
    
    inverse <- FALSE
  }
  
  
  Results_tox_AL_CU_cats <- df_data %>%
    filter(str_detect(AU_ID, "WS", negate = inverse)) %>%
    group_by_at(group1) %>%
    #Summarise data
    summarise(criteria_fraction = first(Crit_fraction),
              num_samples = n(),
              percent_3d = round(sum(Result_Operator == "<" & IRResultNWQSunit > crit )/num_samples * 100),
              num_fraction_types = n_distinct(Simplfied_Sample_Fraction),
              num_samples_total_fraction = sum(Simplfied_Sample_Fraction == "Total"),
              num_Samples_dissolved_fraction = sum(Simplfied_Sample_Fraction == "Dissolved"),
              num_excursions_all = sum(excursion),
              num_excursions_total_fraction = sum(excursion[Simplfied_Sample_Fraction == "Total"]),
              num_excursions_dissolved_fraction = sum(excursion[Simplfied_Sample_Fraction == "Dissolved"]),
              num_samples_crit_excursion_calc = ifelse(criteria_fraction == "Total", num_samples_total_fraction + num_excursions_dissolved_fraction, 
                                                       num_Samples_dissolved_fraction + (num_samples_total_fraction - num_excursions_total_fraction )),
              critical_excursions = binomial_excursions(num_samples_crit_excursion_calc, type = "Toxics")) %>%
    # Assign categories
    mutate(IR_category = case_when(percent_3d == 100 ~ "3D",
                                   num_samples >= 2 & criteria_fraction == "Dissolved" & num_excursions_dissolved_fraction >= critical_excursions ~ "5",
                                   num_samples >= 2 & criteria_fraction == "Total" & num_excursions_all >= critical_excursions ~ "5",
                                   num_samples < 10 & num_excursions_all >= 1 ~ "3B",
                                   criteria_fraction == "Dissolved" & num_excursions_total_fraction > 0 & num_Samples_dissolved_fraction == 0 ~ "3B",
                                   num_samples_crit_excursion_calc < 10 & num_excursions_all == 0 ~ "3",
                                   
                                   num_samples >= 10 & num_excursions_dissolved_fraction < critical_excursions ~ "2"
    ),
    Rationale = case_when(percent_3d == 100 ~ paste0("Insufficient data: All results are non-detects with detection limits above criteria- ", num_samples, " total samples"),
                          
                          num_samples >= 2 & criteria_fraction == "Dissolved" & num_excursions_dissolved_fraction >= critical_excursions ~  paste0("Impaired: ", num_excursions_dissolved_fraction,
                                                                                                                                                   " excursion of dissolved fraction results. ",
                                                                                                                                                   num_samples, " total samples"),
                          num_samples >= 2 & criteria_fraction == "Total" & num_excursions_all >= critical_excursions ~  paste0(num_excursions_all, " excursions is less than ",
                                                                                                                                critical_excursions, " needed to list- ",
                                                                                                                                num_samples, " total samples"),
                          num_samples < 10 & num_excursions_all >= 1 ~ paste0("Insufficient data: ", num_excursions_all,
                                                                              " excursion of criteria with ",
                                                                              num_samples, " total samples"),
                          criteria_fraction == "Dissolved" & num_excursions_total_fraction > 0 & num_Samples_dissolved_fraction == 0 ~ paste0("Insufficient data: ", "Only total fraction results available, criteria is 'Dissolved' ",
                                                                                                                                              num_excursions_all, " total excursions of ", 
                                                                                                                                              num_samples, " total samples"),
                          num_samples_crit_excursion_calc < 10 & num_excursions_all == 0 ~ paste0("Insufficient data: ", num_excursions_all,
                                                                                                  " excursion of criteria with ",
                                                                                                  num_samples, " total samples"),
                          
                          num_samples >= 10 & num_excursions_dissolved_fraction < critical_excursions ~ paste0("Attaining: ", num_excursions_all, " excursions is less than ",
                                                                                                               critical_excursions, " needed to list- ",
                                                                                                               num_samples, " total samples")
    )) %>%
    mutate(IR_category = factor(IR_category, levels=c("3D", "3", "3B", "2", "5" ), ordered=TRUE))
  
  Results_tox_AL_CU_cats <- join_prev_assessments(Results_tox_AL_CU_cats, AU_type = AU_type)     
  return(Results_tox_AL_CU_cats)
  
}


AL_Tox_CU_WS <- AL_tox_CU_assess_fun(df_data = Results_censored, AU_type = "WS")


AL_Tox_CU_other <- AL_tox_CU_assess_fun(df_data = Results_censored, AU_type = "other") %>%
  mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code))


WS_AU_rollup <- AL_Tox_CU_WS %>%
  select(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code,   Char_Name, IR_category, Rationale) %>%
  ungroup() %>%
  group_by(AU_ID, Char_Name, Pollu_ID, wqstd_code) %>%
  summarise(IR_category_AU = max(IR_category),
            Rationale_AU = str_c(MLocID, ": ", Rationale, collapse =  " ~ " ) ) %>%
  mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code))

WS_AU_rollup <- join_prev_assessments(WS_AU_rollup, AU_type = 'other')
Results_tox_CU_AL <- list(data = Results_censored,
                          AL_Tox_CU_WS = AL_Tox_CU_WS,
                          WS_AU_rollup = WS_AU_rollup,
                          AL_Tox_CU_other = AL_Tox_CU_other)

return(Results_tox_CU_AL)
}



  
